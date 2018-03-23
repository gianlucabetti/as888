000100040531     H DECEDIT('0,') DATEDIT(*ymd.)
000200020910      * FNLR32R *----------------------------------------------------*
000300020910      *        - INVENTARIO DISCORDANZE ARRIVI I.D.A.    ESEGUE      *
000400020910      *--------------------------------------------------------------*
000500911015     FTABEL00F  IF   E           K DISK
000600911015     FAZORG01L  IF   E           K DISK
000700000503     FFNFWA01L  IF   E           K DISK
000800000503     FFNFVA02L  IF   E           K DISK
000900030114     FFNFVA01L  IF   E           K DISK    RENAME(FNFVA000:FNFVA001)
001000070110     FFNBRV01L  IF A E           K DISK
001100070110     FFNBRV02L  IF   E           K DISK    RENAME(FNBRV000:FNBRV002)
001200070110     FFNBRV09L  IF   E           K DISK    RENAME(FNBRV000:FNBRV009)
001300070110     FFNBRV07L  IF   E           K DISK    RENAME(FNBRV000:FNBRV007)
001400040322     FFISPPWRF  IF A E           K DISK
001500030114     FFNANM02L  IF A E           K DISK    INFDS(FNANM2)
001600030114     FFNANM00F  UF   E             DISK    RENAME(FNANM000:FNANMF)
001700020917     FFNFVV01L  UF   E           K DISK
001800030114     FFNFVV02L  IF   E           K DISK    RENAME(FNFVV000:FNFVV002)
001900960212     FFNBLT27L  IF   E           K DISK
002000040531     FFiar501l  IF   E           K DISK
002100960212     FFNART27L  IF   E           K DISK
002200030114     FFNBTT37L  IF   E           K DISK    RENAME(FNbtt000:FNbtt037)
002300941222     FFNARB01L  UF   E           K DISK
002400980114     FAZCAE02L  IF   E           K DISK
002500030114     FQSYSPRT   O    F  132        PRINTER USROPN     OFLIND(*INOF)
002600030114
002700030114     D STA             S             66    DIM(14) CTDATA PERRCD(1)             SKIERE STAMPA
002800920716     D ERR             S             21    DIM(4) CTDATA PERRCD(1)              SKIERE ERRORE
002900960302     D CMD2            S             51    DIM(1) CTDATA PERRCD(1)              OVRPRTF
003000911015     D*
003100921124     D FVA             S             11  0 DIM(208)                             FV ABBINATI
003200921124     D STF             S              1    DIM(132)                             STAMPA FV ABB
003300030114     D L1              S              3  0 DIM(30)                              FIL GESTITE
003400921124     D*
003500960515     D SKI             S              3  0 DIM(4)                               LINEE TRANSITO
003600160408
003700160408     d sps             s              2  0 dim(101)
003800160408     d sord            s              3  0 dim(101)
003900160408     d ix              s              3  0
004000160408     d x1              s              3  0
004100160408     d x2              s              3  0
004200160408
004300990511     D FNANM2          DS
004400960515     D  ANMNR2               397    400B 0
004500921124     D*LINEE   DI ARRIVO
004600921124     D                 DS
004700921124     D  FVALNA                 1      3  0
004800921124     D  FVAFFV                 4    243
004900921124     D  FVAFF2               244    450
005000000503     D  FWAFF3               451    690
005100000503     D  FWAFF4               691    900
005200000503     D  VUOTO                901    903
005300030114     D  FFV                    1    903    DIM(301)                             LINEE FOGLIO
005400921124     D                 DS
005500921124     D  FVALAI                 1      3  0
005600921124     D  FVALNP                 4      6  0
005700921124     D  FVANFV                 7     11  0
005800921124     D  FVAPAR                 1     11  0
005900921124     D                 DS
006000921124     D  ALF3                   1      3
006100921124     D  ALF1                   4      4
006200921124     D  ALF5                   5      9
006300921124     D  FVAALF                 1      9
006400921124     D*FILIALE DI SCARICO
006500921124     D                 DS
006600921124     D  LNA2                   1      3  0
006700921124     D  FVAFLP                 4    243
006800921124     D  FVAFL2               244    450
006900000503     D  FWAFL3               451    690
007000000503     D  FWAFL4               691    900
007100000503     D  VUOTO2               901    903
007200030114     D  FLP                    1    903    DIM(301)                             LINEE TRANSITO
007300911015     D WLBDAT          DS
007400941222     D  G02DAT                 1      8  0
007500941222     D  G02INV                 9     16  0
007600941222     D  G02ERR                17     17
007700941222     D  G02TGI                18     22  0
007800900622     D KPJBA         E DS
007900101202      *
008000101202      * - Parametri per richiamo al pgm di controllo profilo utenti
008100101202     d TIBS34ds      e ds
008200101202      * - Ds di riferimento al file esterno AzUte00F
008300101202     d AZUTEds       e ds                  ExtName(AzUte00F)
008400101202      * - Ds per dati organigramma
008500101202     d dDatiUte      e ds
008600101202      *
008700950810     D* DS PER TRUL06R - CARICAMENTO £X
008800030114     D TRUL06DS      E DS
008900030114     D  LIN                    1     90  0 DIM(30)                              FIL. COMODO
009000970602     D DSUL11          DS
009100970602     D  D11TLA                 1      1
009200970602     D  D11NPG                 2      2
009300970602     D  D11SPG                 3      3
009400970602     D  D11FAS                 4      4
009500040604     D* PASSAGGIO DATI A FNLR32R
009600040604     D PARA32          DS
009700040604     D  P32VALORE              1      1
009800040604     D  P32fgs                 2      4  0
009900040604     D  P32Tla                 5      5
010000040604     D* CAMPO DI OUTPUT
010100040604     D  P32MANCA               5      6
010200040604     D  P32AAS                 7     10  0
010300040604     D  P32LNP                11     13  0
010400040604     D  P32NRS                14     15  0
010500040604     D  P32NSP                16     22  0
010600040604     D  P32bva                23     23
010700040604     D* CAMPO DI input
010800040604     d* data foglio arrivi
010900040604     D  P32dfvv               24     31  0
011000941228     D DSLR01        E DS
011100000512     D OG143         E DS
011200960711     D DSTV          E DS
011300960711     D DS7A          E DS
011400911016     D DS7C          E DS
011500160408     D DS7J          E DS
011600040531     D dar5gen       E DS
011700040109     D DSLV53        E DS                  EXTNAME(FNLV53DS)
011800971009     D DSLV55        E DS                  EXTNAME(FNLV55DS)
011900070222     D FNLS45DS      E DS
012000070110     D DSbrv         E DS                  EXTNAME(fnbrv00f)
012100960410     D WLBDA8          DS
012200960410     D  G08DAT                 1      8  0
012300960410     D  G08INV                 9     16  0
012400960410     D  G08ERR                17     17
012500960410     D  G08TGI                18     22  0
012600021214      *
012700030109     D Wanm005         s                   like(ANMCaa) inz(005)
012800030109     D Wanm050         s                   like(ANMCaa) inz(050)
012900030109     D Wanm090         s                   like(ANMCaa) inz(090)
013000030109     D Wanm160         s                   like(ANMCaa) inz(160)
013100040109     D SIogas          s                   like(§ogas)
013200040109     D MYogas          s                   like(§ogas)
013300040109     D TPogas          s                   like(§ogas)
013400030114     D W05cpt          s                   like(§7Ccpt)
013500030114     D W05spt          s                   like(§7Cspt)
013600030114     D laitfp          s                   like(d55tfp)
013700030114     D flptfp          s                   like(d55tfp)
013800040531     D bolaas          s                   like(bltaas)
013900040531     D bollnp          s                   like(bltlnp)
014000040531     D bolnrs          s                   like(bltnrs)
014100040531     D bolnsp          s                   like(bltnsp)
014200040531     D ktrd            s                   like(ar5trd) inz('GEN')
014300040611     D Wvalore         s                   like(p32valore)
014400040611     D PARBRV          s                   like(DSBRV)
014500050929     D katr            s                   like(sppatr)
014600070215     D wtimez          S               z
014700070215     D dataiso         S               d   datfmt(*iso)
014800070215     D oraiso          S               t   timfmt(*iso)
014900070215
015000070215     D                SDS
015100070215     D  NOMPGM                 1     10
015200030114
015300950816     I*
015400070110     I* PER CHIAREZZA DI PROGRAMMA (E PER NON SPORCARE I CAMPI DI FNBRV
015500070110     I* AL MOMENTO DI UTILIZZO DI FNBRV09L) RIDENOMINO TUTTI I CAMPI
015600070110     I* DI FNBRV09L, CHIAMANDOLI COME FISPP
015700070110     IFNBRV009
015800070110     I              BRVNVR                      SPPNVR
015900941223     I              BRVFGS                      SPPLPV
016000941223     I              BRVNPG                      SPPNPG
016100941223     I              BRVNFV                      SPPNFV
016200000127     I              BRVPES                      SPPPES
016300941223     I              BRVLNP                      SPPLNP
016400941223     I              BRVLNA                      SPPLNA
016500941223     I              BRVNRS                      SPPNRS
016600941223     I              BRVNSC                      SPPNSC
016700941223     I              BRVZNC                      SPPZNC
016800941223     I              BRVNPS                      SPPNPS
016900941223     I              BRVCAN                      SPPCAN
017000941223     I              BRVVUC                      SPPVUC
017100941223     I              BRVPUC                      SPPPUC
017200941223     I              BRVDCS                      SPPDCS
017300941223     I              BRVHCS                      SPPHCS
017400000127     I              BRVDFS                      SPPDFS
017500000127     I              BRVHMS                      SPPHMS
017600941223     I              BRVATR                      SPPATR
017700070110     I              BRVTSU                      SPPTSU
017800941223     I              BRVFTR                      SPPFTR
017900941223     I              BRVDTR                      SPPDTR
018000000127     I              BRVHTR                      SPPHTR
018100000127     I              BRVFLE                      SPPFLE
018200070110     I              BRVPRU                      SPPPRU
018300070110     I              BRVMIS                      SPPMIS
018400070110     I              BRVTAP                      SPPTAP
018500921124     I*---------------------------------------------------------------*
018600921124     I*   RIEPILOGO INDICATORI
018700921124     I*---------------------------------------------------------------*
018800950810     I* 05    - COLLO ARRIVATO SU ALTRO FOGLIO VIAGGIO
018900921126     I* 10    - CONFERMA IDA
019000980114     I* 23    - FILIALE DEL FOGLIO VIAGGIO 2 LIVELLO IN ARRIVO IN ALTRA
019100040109     I*         AREA DI PARTENZA  e <> OGAS
019200070110     I* 30    - LETTURA FNBRV
019300000209     I* 31    - LETTURA FISPP
019400921126     I* 32    - DI COMODO
019500950811     I* 33    - ON - LNA DELL'AREA  OFF-COLLO IN TRANSITO
019600921126     I* 34    - DI COMODO
019700950811     I* 36    - OFF  ESISTE BOLLA ARRIVO O BOLLA TRANSITO
019800921126     I* 37    - STAMPA ANOMALIA
019900950811     I* 38    - LNA DI CUI SONO SOLO TERMINAL
020000940330     I* 39    - DI COMODO
020100950811     I* 40    - LNA DI CUI SONO SOLO TERMINAL
020200100712     I* 41    - STAMPA ANOMALIA  senza crearla...
020300921124     I*---------------------------------------------------------------*
020400030114
020500900622     C     *ENTRY        PLIST
020600900622     C                   PARM                    KPJBA
020700040603     C                   PARM                    PARbrv
020800040604     C                   PARM                    PARa32
020900040604     c
021000040604     c                   select
021100040603     c* Se pgm richiamo per anom 05 --> eseguo solo routine majore
021200040604    0c                   when      %PARMS>1   and p32tla<>'C'
021300040603     c* Imposto il flag come definitivo
021400040603     c                   eval      dlrfs1='S'
021500040604     c                   eval      dlrfgs=p32fgs
021600040604     c                   eval      wdfv=p32dfvv
021700040611     c                   eval      Wvalore=p32valore
021800040611     c                   eval      DSBRV=PARBRV
021900040603     c*
022000040603     c* Imposto i campi della spunta nei campi di SPP
022100040603     c                   eval      spplnp=brvlnp
022200040603     c                   eval      spplna=brvlna
022300040603     c                   eval      sppnrs=brvnrs
022400040603     c                   eval      sppnsc=brvnsc
022500040603     c                   eval      sppnfv=brvnfv
022600040603     c                   eval      sppznc=brvznc
022700040603     c                   eval      spplpv=brvfgs
022800040603     c                   eval      sppnps=brvnps
022900040604     c* vedo se creare anomalia collo partito non arrivao
023000040603     c                   exsr      MAJORE
023100040604     c* chiusura files
023200040604    0c                   when      %PARMS>1   and p32tla='C'
023300040604     c
023400040604   x0c                   other
023500040604     c* pgm non richiamato
023600040603     c*
023700040611     c                   clear                   Wvalore
023800941228     C                   MOVEL     KPJBU         DSLR01
023900950811     C* IMPOSTAZIONI INIZIALI
024000911015     C                   EXSR      IMPO
024100950811     C* CREAZIONE FILE DI WORK, LEGGENDO LE SPUNTE PARTENZA DEI FVP ABB
024200911015     C                   EXSR      CRTWOK
024300950810     C*
024400950810     C* SEGNALO ANOMALIE  -->  PARTITO NON ARRIVATO
024500950817     C*
024600950817     C* NEI 2 LIVELLO IN ARRIVO TALE ANOMALIA NON VIENE CREATA IN
024700950817     C* QUANTO E'VALIDA QUELLA DEL TERMINAL DI ARRIVO
024800950817     C* INFATTI DI SPUNTE PARTENZA NON NE VIENE CARICATA NESSUNA
024900000209     C     *LOVAL        SETLL     FISPPWR
025000000209     C                   READ      FISPPWR                                31
025100950810     C*
025200000512    1C     *IN31         DOWEQ     *OFF
025300000512     C*
025400000512     C* SE PER LE BOLLE LNP POSTE DEVO SOLO O STAMPARE O CREARE
025500000512     C*  O NULLA, VEDO SE LA LNP E' POSTE
025600000512    2C     W05CPT        IFEQ      'S'
025700000512     C     W05SPT        ANDEQ     'S'
025800000512   X2C                   ELSE
025900000512    3C     SPPLNP        IFNE      SAVLNP
026000000512     C     SPPLNP        CHAIN     AZORG                              39
026100000512     C  N39              MOVEL     ORGDES        DESLNP
026200000512     C   39              MOVEL     *BLANKS       DESLNP           10
026300000512     C*
026400000512     C  N39              MOVEL     ORGDE3        OG143
026500000512     C   39              CLEAR                   OG143
026600021122     c                   if        §ogntw='PPT'
026700021122     c                   movel     'S'           LNPPT             1
026800021213     c                   else
026900021213     c                   clear                   LNPPT
027000021122     c                   endif
027100000512     C*
027200000512     C                   MOVEL     SPPLNP        SAVLNP            3 0
027300000512    3C                   ENDIF
027400000512    2C                   ENDIF
027500000512     C*
027600000512    2C     LNPPT         IFNE      'S'
027700000512     C     W05CPT        OREQ      'S'
027800000512     C     W05SPT        OREQ      'S'
027900950810     C                   EXSR      MAJORE
028000000512    2C                   ENDIF
028100000515     C                   READ      FISPPWR                                31
028200000512    1C                   ENDDO
028300950810     C*
028400950810     C* SEGNALO ANOMALIE  -->  ARRIVATO NON PARTITO
028500950810     C* LEGGO LE SPUNTE ARRIVI CONSEGNA E SPUNTA IMA DEI FOGLI CON DATA
028600950810     C*  UGUALE AL FOGLIO ARRIVI
028700950816     C*
028800950810     C                   Z-ADD     2             KNPG2
028900950810     C                   EXSR      REDFVV
029000950811     C*
029100950810     C                   Z-ADD     3             KNPG2
029200950810     C                   EXSR      REDFVV
029300950810     C*
029400950810     C                   Z-ADD     4             KNPG2
029500950810     C                   EXSR      REDFVV
029600910320     C*
029700921124     C* SE DEFINITIVO CHIUDO IL FOGLIO
029800941228     C     DLRFS1        IFEQ      'S'
029900911015     C                   EXSR      UPDFV
030000950810     C                   ENDIF
030100040603     c
030200040604    0C                   ENDSL
030300101202     c*
030400970509     C** CHIUSURA PGM
030500040604     c                   if        %parms=1 or p32tla='C'
030600970509     C     WFC44         IFEQ      'S'
030700070222     C                   MOVEL     'C'           dls45flg
030800070222     C                   CALL      'FNLS45R'
030900070222     c                   parm                    fnls45ds
031000070222     c                   parm                    dsbrv
031100970509     C                   ENDIF
031200971128     C*
031300971128     C                   MOVEL     'C'           D55TLA
031400971128     C                   CALL      'FNLV55R'
031500971128     C                   PARM                    DSLV55
031600040604     c                   endif
031700911015     C*
031800040604     c* Verifico come chiudere il pgm
031900040604     c                   if        %parms=1 or p32tla<>' '
032000000000     C                   SETON                                        LR
032100040604     c                   else
032200040604     C                   SETON                                        RT
032300040604     c                   endif
032400941222     C**************************************************************************
032500941222     C*    CREO FILE DI WORK
032600941222     C**************************************************************************
032700911015     C     CRTWOK        BEGSR
032800911015     C*
032900911015     C                   MOVEA     *ZEROS        FVA
033000921124     C                   Z-ADD     0             I
033100921124     C*
033200941227     C* ELABORO SOLTANTO I FOGLI VIAGGIO DIRETTI(FVAATR=' ') ESCLUDENDO
033300960711     C*  I FV DI DEFLUENZA DEL MIO TERMINAL
033400941222     C     KFVA1         SETLL     FNFVA02L
033500941222     C     KFVA1         READE     FNFVA02L                               30
033600911015     C*
033700940606    2C     *IN30         DOWEQ     '0'
033800980114     C*  DETERMINO IL TERMINAL DI ARRIVO DELLA FILIALE GESTIONE
033900980114     C                   CLEAR                   DSLV55
034000980114     C                   MOVEL     'A'           D55TPT
034100980114     C                   Z-ADD     WFGS          D55LIN
034200980114     C                   Z-ADD     WDFV          D55DRF
034300980114     C                   Z-ADD     FVALNP        D55LNP
034400980114     C                   CALL      'FNLV55R'
034500980114     C                   PARM                    DSLV55
034600960711     C*  VEDO SE FV DI DEFLUENZA
034700960711     C     *IN23         IFEQ      *ON
034800980114     C     FVALNP        ANDEQ     D55TFA
034900960711     C                   EXSR      CTRDEF
035000960711     C                   ENDIF
035100940607     C*
035200940607     C* SE NON SONO 2 LIV  IN ARRIVO                       -->ELABORO
035300040604     C* ELABORO I FOGLI DIRETTI (FVAATR =' ') ora i transiti sono a posto
035400040604     c*  al 100% anche per la sardegna per cui elaboro solo i fogli diretti
035500040604     c
035600980421     C* ELABORO ANCHE I FOGLI ILLUMINATI SE LA FIL DI TRANSITO E'
035700980421     C*  SULLO STESSO AS  RISPETTO A FVALAI
035800940607    4C     *IN23         IFEQ      *OFF
035900040604    3C     fvaatr        andeq     *blanks
036000070423     c* Elaboro anche i fogli illuminati se LOCALI
036100070423    4C     *IN23         oreq      *OFF
036200070423    3C     fvaatr        andeq     'T'
036300070423    3C     fvaTTR        andeq     'L'
036400940607     C* SE SONO 2 LIV IN ARR E FOGLIO NON DAL MIO TERMINAL -->ELABORO
036500940607     C     *IN23         OREQ      *ON
036600980114     C     FVALNP        ANDNE     D55TFA
036700960711    3C     FVAATR        ANDEQ     ' '
036800960711     C*  O DAL MIO TERMINAL MA FV LOCALE
036900960711     C     *IN23         OREQ      *ON
037000980114     C     FVALNP        ANDEQ     D55TFA
037100960711     C     §TVDEF        ANDNE     'S'
037200940607     C*
037300000503     C     KFWA          CHAIN     FNFWA01L                           39
037400000503     C     *IN39         IFEQ      *ON
037500000503     C     FWAATB        ORNE      ' '
037600000503     C                   CLEAR                   FWAFF3
037700000503     C                   CLEAR                   FWAFF4
037800000503     C                   CLEAR                   FWAFL3
037900000503     C                   CLEAR                   FWAFL4
038000000503     C                   ENDIF
038100000503     C*
038200921124     C                   ADD       1             I                 3 0
038300921124     C                   Z-ADD     FVAPAR        FVA(I)
038400921124     C*
038500921124     C                   Z-ADD     FVALNA        LNA2
038600911015     C*
038700911015     C* CREO FILE DI WORK
038800921124     C                   Z-ADD     1             X
038900921124     C                   MOVEL     FVALAI        ALF3              3
039000921126     C     ALF3          LOOKUP    FLP(X)                                 31
039100921124     C*
039200940607    5C     *IN31         DOWEQ     '1'
039300921222     C*
039400921222     C* CARICO LE SPUNTE PARTENZE
039500921222     C                   EXSR      CARSPP
039600921222     C*
039700921124     C                   ADD       1             X                 3 0
039800921124     C     ALF3          LOOKUP    FLP(X)                                 31
039900940607    5C                   END
040000921222     C*
040100921222     C* ELABORO LA LINEA DI INVIO SE NON ANCORA ELABORATA
040200921222     C                   Z-ADD     1             X
040300921222     C     ALF3          LOOKUP    FFV(X)                                 31
040400940607    5C   31ALF3          IFNE      FLP(X)
040500980421     C* SE FOGLIO ILLUMINATO E NON E' 2 LIVELLO IN ARRIVO,
040600980421     C*  ELABORO SOLO SE IL P.O. DI SCARICO HA LO STESSO TERMINAL PART
040700980421     C*  DI FVALAI
040800980421     C                   CLEAR                   WELAB
040900980421     C     *IN23         IFEQ      *OFF
041000980421     C                   CLEAR                   DSLV55
041100980421     C                   MOVEL     'P'           D55TPT
041200980421     C                   Z-ADD     FVALAI        D55LIN
041300980421     C                   Z-ADD     WDFV          D55DRF
041400980421     C                   CALL      'FNLV55R'
041500980421     C                   PARM                    DSLV55
041600980421     C                   MOVEL     D55TFP        LAITFP
041700980421     C**
041800980421     C                   CLEAR                   DSLV55
041900980421     C                   MOVEL     'P'           D55TPT
042000980421     C                   MOVEL     FLP(X)        D55LIN
042100980421     C                   Z-ADD     WDFV          D55DRF
042200980421     C                   CALL      'FNLV55R'
042300980421     C                   PARM                    DSLV55
042400980421     C                   MOVEL     D55TFP        FLPTFP
042500980421     C**
042600980421     C** CARICO LE SPUNTE PERCHE' ALTRIMENTI NON SI CREEREBBERO LE ANOM
042700980421     C*  05 DAL MOMENTO CHE IL TRANSITO (E LE BOLLE TRANSITO NON CI SON
042800980421     C*  O) E' SULLO STESSO AS E C'E' RIPARTENZA MA NON C'E' ABBINAMENT
042900980421     C*  IN ARRIVO E MANCA L'AUTOGENERAZIONE SPUNTA PARTENZA DA TRANS.
043000980421     C*  VISTO CHE LE BOLLE NON CI SONO. L'ANOM 05 DOVREBBE ESSERE
043100980421     C*  CREATA DALLE SPUNTE PARTENZA TRANSITO, MA NON AVENDOLE
043200980421     C*  MEGLIO CREARLE SULLE SPUNTE ORIGINALI DI PARTENZA CHE NIENTE
043300980421     C*  ES. 63 PER 14 TRANSITO 15 . SE MANCA UN COLLO LO CREO QUANDO
043400980421     C*  14 FA L'IDA DEL FOGLIO DI 63 ANCHE SE TEORICAMENTE DOVREBBE
043500980421     C*  FARLO SUL FOGLIO DI 15
043600980421     C     FLPTFP        IFEQ      LAITFP
043700980421     C                   MOVEL     'S'           WELAB
043800980421     C                   ENDIF
043900070423     c* Se si tratta di foglio locale, elaboro sempre la linea di
044000070423     c*  invio
044100070423     c                   if        fvattr='L'
044200070423     C                   MOVEL     'S'           WELAB
044300070423     C                   ENDIF
044400070423     c
044500980421     C                   ELSE
044600980421     C**
044700980421     C                   MOVEL     'S'           WELAB
044800980421     C                   ENDIF
044900921222     C*
045000921222     C* CARICO LE SPUNTE PARTENZE
045100980421     C     WELAB         IFEQ      'S'
045200921222     C                   EXSR      CARSPP
045300980421    5C                   ENDIF
045400921222     C*
045500940607    5C                   END
045600940607    4C                   END
045700921124     C**
045800921124     C**
045900941222     C     KFVA1         READE     FNFVA02L                               30
046000940606    2C                   END
046100921124     C**
046200921124     C*
046300921124     C                   SORTA     FVA
046400921124     C*
046500950810     C                   Z-ADD     1             B                 3 0
046600921124     C     FVA(208)      IFEQ      0
046700921124     C                   Z-ADD     209           B
046800921124     C                   ELSE
046900921124     C     FVA(B)        DOWEQ     0
047000921124     C                   ADD       1             B
047100921124     C                   END
047200921124     C                   END
047300910321     C*
047400910321     C                   ENDSR
047500960711     C**************************************************************************
047600960711     C*    CONTROLLO SE FV DI DEFLUENZA
047700960711     C**************************************************************************
047800960711     C     CTRDEF        BEGSR
047900960711     C                   MOVEL(P)  FVATTR        KEY
048000960711     C                   MOVEL     'TV'          COD
048100960711     C     KTAB          CHAIN     TABEL                              32
048200960711     C  N32              MOVEL     TBLUNI        DSTV
048300960711     C   32              CLEAR                   DSTV
048400960711     C                   ENDSR
048500941222     C**************************************************************************
048600941222     C*    CARICO SPUNTE DELLE LINEE SELEZIONATE DEL FOGLIO
048700941222     C**************************************************************************
048800921222     C     CARSPP        BEGSR
048900941222     C*
049000921222     C                   MOVEL     FFV(X)        SAVLNA
049100921222     C*
049200941222     C                   Z-ADD     1             KNPG                           1=PARTENZE
049300000127     C                   Z-ADD     FVANFV        KNFV3
049400070110     C     KBRV2         SETLL     FNBRV02L
049500070110     C     KBRV2         READE     FNBRV02L                               32
049600921222     C*
049700960506     C     *IN32         DOWEQ     *OFF
049800960506     C***** NON ESCLUDO PIU' I LOCALI PERCHE' ADESSO LA FILIALE SEGNAC
049900960506     C***** PUO' ESSERE <> DALLA FILIALE BOLLA E QUINDI ESSERE IN L1
050000960506     C***** MA NON ESSERE LOCALE PERCHE' PARTITO DA ALTRO AS
050100960506     C*****      BRVLNP    LOKUPL1                       31
050200960506     C**N31                DO
050300941223     C                   Z-ADD     BRVLNP        SPPLNP
050400941223     C                   Z-ADD     BRVLNA        SPPLNA
050500941223     C                   Z-ADD     BRVNRS        SPPNRS
050600941223     C                   Z-ADD     BRVNSC        SPPNSC
050700941223     C                   Z-ADD     BRVNFV        SPPNFV
050800941223     C                   Z-ADD     BRVZNC        SPPZNC
050900941223     C                   Z-ADD     BRVFGS        SPPLPV
051000941223     C                   Z-ADD     BRVNPS        SPPNPS
051100050929     C                   movel     BRVatr        SPPatr
051200000209     C                   WRITE     FISPPWR
051300921222     C*
051400070110     C     KBRV2         READE     FNBRV02L                               32
051500921222     C                   END
051600941222     C*
051700921222     C                   ENDSR
051800941222     C**************************************************************************
051900941222     C*    SEGNACOLLO PARTITO MA NON ARRIVATO
052000941222     C**************************************************************************
052100911015     C     MAJORE        BEGSR
052200100712     C                   SETOFF                                       3741
052300950811     C* CONTROLLO L'ESISTENZA DELLE BOLLE
052400950811     C                   Z-ADD     SPPLNP        KLNP
052500950811     C                   Z-ADD     SPPLNA        KLNA
052600950811     C                   Z-ADD     SPPNRS        KNRS
052700950811     C                   Z-ADD     SPPNSC        KNSC
052800980401     C                   Z-ADD     SIMFEL        KFGS
052900980414     C                   Z-ADD     SPPLPV        LNPB
053000050929     C                   movel     SPPatr        katr
053100040611     C                   MOVEL     'MA'          WTIPO             2
053200950811     C*
053300950811     C                   EXSR      CHAFIL
053400040603     c* solo se richiresto colli di valore, lo faccio solo per loro
053500040611    0c                   if        Wvalore='S' and §ar5bva='V' or
053600040611     c                             Wvalore=' '
053700911127     C*
053800980401     C                   SELECT
053900960513     C* VEDO SE ESISTE DATA ARRIVO --> NO ANOMALIA
054000950811     C* LNA DELL'AREA
054100980401    1C     *IN33         WHENEQ    *ON
054200980401     C     *IN36         ANDEQ     *OFF
054300980401     C**
054400980401    2C     ARTDCM        IFEQ      0
054500920207     C     ARTDAM        ANDEQ     0
054600911127     C                   SETON                                        37
054700980401    2C                   ENDIF
054800950811     C*
054900980401     C* NON TROVATA BOLLA --> FACCIO CHAIN SULLE SPUNTE
055000980401    1C     *IN33         WHENEQ    *ON
055100980401     C     *IN36         ANDEQ     *ON
055200980401     C* CERCO UNA SPUNTA NELL'AREA DI ARRIVO
055300980401     C                   EXSR      CERSPU
055400980401     C     WTROV         IFEQ      ' '
055500980401     C                   SETON                                        37
055600980401     C                   ENDIF
055700930617     C*
055800980401     C*    TRANSITO (NON GENERO ANOMALIA MA CREO SPUNTA ARR.PIST.89 SE
055900980401     C*              IDA DEFINITIVO)
056000980401    1C     *IN33         WHENEQ    *OFF
056100040611     c* i transiti li controllo solo se elaborazione da ida e non richiamato
056200040611   1ac                   if        %parms=1
056300040611    2C     *IN36         IFEQ      *OFF
056400950608     C*
056500980401    3C     BTTDET        IFEQ      0
056600000127     C     BTTDUT        ANDEQ     0
056700100712     c*
056800100712     c     bttdet        orgt      0
056900100712     c     bttpet        andeq     89
057000100712     C                   SETON                                        3741
057100100712     c
057200100712     C     DLRFS1        ifeq      'S'
057300100712    3C     BTTDET        andeq     0
057400100712     C     BTTDUT        ANDEQ     0
057500960430     C                   EXSR      CRTSPU
057600980401    3C                   ENDIF
057700100712     c
057800100712    3C                   ENDIF
057900980401   X2C                   ELSE
058000950608     C*
058100040531     C* 36 ON -        --> DISGUIDO (NON E' ANOMALIA PARTITO NON ARR.)
058200980401     C*                    CREO SPUNTA ARR. SOLO SE ELABORAZ.DEFINITIVA
058300980401     C**
058400980401    3C     DLRFS1        IFEQ      'S'
058500980401     C* CERCO UNA SPUNTA NELL'AREA DI ARRIVO
058600980401     C                   EXSR      CERSPU
058700980401    4C     WTROV         IFEQ      ' '
058800980401     C                   EXSR      CRTSPU
058900980401    4C                   ENDIF
059000980401    3C                   ENDIF
059100980401    2C                   ENDIF
059200040611   1aC                   ENDIF
059300980401     C**
059400980401     C                   ENDSL
059500950608     C*
059600950608     C*  3 7   O N   -   A N O M AL I A
059700930618    1C     *IN37         IFEQ      *ON
059800920716     C                   MOVEL     ERR(1)        STAERR           21
059900911015     C                   MOVEL     SPPLNP        STALNP            3 0
060000911015     C                   MOVEL     SPPLNA        STALNA            3 0
060100911015     C                   MOVEL     SPPNRS        STANRS            2 0
060200911015     C                   MOVEL     SPPNSC        STASCN            7 0
060300911129     C                   MOVEL     SPPZNC        STAZNC            2 0
060400000127     C                   Z-ADD     SPPNFV        STAFV             6 0
060500921126     C                   MOVEL     SPPLPV        STALPV            4
060600921126     C                   MOVE      '-'           STALPV
060700920716     C                   MOVEL     SPPNPS        STANPP
060800920716     C                   MOVEL     '  '          STANPA
060900911015     C*
061000000512     C* STAMPO PER NO POSTE E PER LNP POSTE, SE PREVISTO
061100000512     C     LNPPT         IFNE      'S'
061200000512     C     W05SPT        OREQ      'S'
061300040603     c* se pgm richiamato non stampa
061400040603     c                   if        %parms=1
061500911015     C                   EXSR      STAMPA
061600000512     C                   ENDIF
061700040603     C                   ENDIF
061800911015     C*
061900950707     C* SOLO SE ELABORAZIONE DEFINITIVA CREO L'ANOMALIA
062000100712     c*  per i transiti (41 on) non creo mai...
062100941228    2C     DLRFS1        IFEQ      'S'
062200100712    2C     *in41         andeq     *off
062300100712     c
062400000512   2AC     LNPPT         IFNE      'S'
062500000512     C     W05CPT        OREQ      'S'
062600040604     c
062700040604     c* Memorizzo che il collo manca se pgm richiamato
062800040604     c                   if        %parms>1
062900040604     c                   eval      p32manca='S'
063000040604     c                   eval      p32aas=bolaas
063100040604     c                   eval      p32lnp=bollnp
063200040604     c                   eval      p32nrs=bolnrs
063300040604     c                   eval      p32nsp=bolnsp
063400040604     c                   eval      p32bva=§ar5bva
063500040604     c                   endif
063600040604     c
063700021214     C                   Z-ADD     Wanm005       COMCAA            3 0
063800960515     C                   MOVEL     'S'           WELAB             1
063900960514     C                   CLEAR                   WANOM
064000960515     C*
064100990511     C     KANM          CHAIN     FNANM000                           32
064200960514     C*
064300960514     C* ELABORO SE NON TROVO L'ANOMALIA O IL FV E' DIVERSO
064400960514    3C     *IN32         DOWEQ     *OFF
064500960514     C* SE E' LO STESSO FOGLIO NON FACCIO NULLA
064600960514    4C     ANMNFV        IFEQ      SPPNFV
064700960514     C     ANMCDU        ANDEQ     SPPLPV
064800960514     C                   MOVEL     'N'           WELAB
064900960514     C                   SETON                                        32
065000960514     C                   ELSE
065100960515     C* SALVO I CAMPI CHE SERVONO E MEMORIZZO CHE C'E' GIA' L'ANOMALIA
065200960514     C                   MOVEL     'S'           WANOM             1
065300960514     C                   MOVEL     ANMFL1        WSAFL1
065400990618     C                   MOVEL     ANMFT1        WSAFT1
065500960514     C                   MOVEL     ANMFL2        WSAFL2
065600960514     C                   Z-ADD     ANMNR2        WSANR2            9 0
065700970529     C                   MOVEL     ANMNFV        WSANFV
065800970529     C                   Z-ADD     ANMCDU        WSACDU
065900980317     C                   Z-ADD     ANMNPS        WSANPS
066000960514     C*
066100990511     C     KANM          READE     FNANM000                               32
066200960514    4C                   ENDIF
066300960514    3C                   ENDDO
066400960514     C*
066500960514     C* SE L'ANOMALIA ESISTE
066600960514     C*  MEMORIZZO LE FILIALI TRASMISSIONI, IL LIVELLO IDD E IL NFV
066700960514    3C     WELAB         IFEQ      'S'
066800960514     C* SE HO TROVATO UN'ALTRA ANOMALIA LA RICHAINO
066900960514     C     WANOM         IFEQ      'S'
067000990511     C     WSANR2        CHAIN     FNANM00F                           32
067100960514     C                   ENDIF
067200960514     C*
067300040531     C                   MOVEL     bolaas        ANMaas
067400040531     C                   MOVEL     bollnp        ANMlnp
067500040531     C                   MOVEL     bolnrs        ANMnrs
067600040531     C                   MOVEL     bolnsp        ANMnsp
067700040531     c* memorizzo n anmft4 se collo di valore
067800040531     c                   eval      anmft4=       §ar5bva
067900040531     c
068000040531     C                   MOVEL     SPPLNP        ANMFLS
068100911015     C                   MOVEL     SPPLNA        ANMLNA
068200911015     C                   MOVEL     SPPNRS        ANMNRS
068300911015     C                   MOVEL     SPPNSC        ANMSCN
068400950608     C                   Z-ADD     SPPNPS        ANMNPS
068500990511     C                   Z-ADD     SPPNFV        ANMNFV
068600950810     C                   MOVEL     SPPLPV        ANMCDU
068700950816     C                   Z-ADD     WDFV          ANMDAO
068800960514     C                   MOVEL     DLRFGS        ANMFLE
068900950816     C                   MOVEL     SPPLPV        WFL2
069000950816     C     ANMLNP        IFGT      0
069100950816     C                   MOVEL     ANMLNP        WFL1
069200950816     C                   ELSE
069300950816     C                   MOVEL     ANMFLS        WFL1
069400950816     C                   ENDIF
069500160408     c* Se Anomalia già presente memorizzo i dati del foglio e della pistola
069600160408     c* in base alla priorità delle due pistole tenendo quella con priorità più bassa
069700160408    4c                   if        wanom='S'
069800160408     c                   eval      x1=%lookup(wsanps:sps)
069900160408     c                   eval      x2=%lookup(sppnps:sps)
070000160408    5c                   if        sord(x1)<sord(x2)
070100160408     C                   MOVEL     WSACDU        ANMCDU
070200160408     C                   MOVEL     WSANFV        ANMNFV
070300160408     C                   MOVEL     WSANPS        ANMNPS
070400160408   x5c                   else
070500970529     C* SE IL FOGLIO PER CUI STO CREANDO E' LA LNP DELLA BOLLA O COLLO
070600970529     C*  TENGO IL FOGLIO PRESENTE NELL'ANOMALIA SE NON PROVENIVA
070700970529     C*  DALLA PARTENZA
070800160408     C**   WANOM         IFEQ      'S'
070900160408     C**   SPPLPV        ANDEQ     WFL1
071000160408    6C     SPPLPV        ifeq      WFL1
071100970529     C     WSACDU        ANDNE     WFL1
071200970529     C                   MOVEL     WSACDU        ANMCDU
071300970529     C                   MOVEL     WSANFV        ANMNFV
071400980317     C                   MOVEL     WSANPS        ANMNPS
071500160408    6C                   ENDIF
071600160408    5c                   endif
071700160408    4c                   endif
071800911016     C*
071900911016     C                   EXSR      CRTANM
072000930618    3C                   END
072100911015     C*
072200040603     c* Eseguo aggiornamento bolla, solo se non richiamato
072300040603   3ac                   if        %parms=1
072400930618     C   33
072500930618    3CANN36              DO
072600941222     C     KARB          CHAIN     FNARB01L                           34
072700930621    4C  N34ARBAMA        IFLT      2
072800911015     C                   Z-ADD     0             ARBAMA
072900950608    4C                   ENDIF
073000911127     C*
073100941222     C  N34              UPDATE    FNARB000
073200950608    3C                   ENDDO
073300040603   3aC                   ENDif
073400101202     c*
073500000512   2AC                   END
073600000512    2C                   END
073700911015     C*
073800930618    1C                   END
073900040603    0C                   ENDif
074000941222     C*
074100911015     C                   ENDSR
074200980401     C**************************************************************************
074300980401     C* CERCA UNA SPUNTA DEL TERMINAL  O 2 LIVELLO CON STESSO TERMINAL
074400980401     C**************************************************************************
074500980401     C     CERSPU        BEGSR
074600980401     C                   CLEAR                   WTROV
074700070110     C     KBRV5         SETLL     FNBRV07L
074800070110     C     KBRV5         READE     FNBRV07L                               40
074900050929    1C     *IN40         DOWEQ     *OFF
075000980401     C                   EXSR      TERFGS
075100101202     c*
075200050929    2C     FGSFE0        IFEQ      DLRFGS
075300040109     c     brvfgs        oreq      dlrfgs
075400101202     c*
075500050929     c* Per le spunte tipiche partenza, imp entrata e partenza, considero
075600050929     c*  come non trovata se la spunta ha come terminal partenza il
075700050929     c*  p.o  gestione della spunta partenza per la quale devo creare l'anom
075800050929    3c                   if        katr=' ' and fgstfp=lnpb
075900050929    4c                   if        brvnpg=3 or brvnpg=1
076000050929     c* cerco con fnlv53 se foglio imp o defluenza
076100050929     C                   CLEAR                   DSLV53
076200050929     C                   Z-ADD     SIMFEL        D53FEL
076300050929     C                   Z-ADD     brvfgs        D53FGS
076400050929     C                   Z-ADD     brvnfv        D53NFV
076500050929     C                   MOVEL     brvnpg        D53NPG
076600050929     C                   CALL      'FNLV53R'
076700050929     C                   PARM                    DSLV53
076800050929    4c                   endif
076900101202     c*
077000101202     c*
077100050929    4c                   if        brvnpg=5 or
077200050929     c                             (brvnpg=3 and d53spg='P') or
077300050929     c                             (brvnpg=1 and d53def=' ')
077400050929   x4c                   else
077500050929     C                   MOVEL     'S'           WTROV             1
077600050929     C                   SETON                                        40
077700050929    4c                   endif
077800101202     c*
077900050929   x3c                   else
078000980401     C                   MOVEL     'S'           WTROV             1
078100980401     C                   SETON                                        40
078200050929    3c                   endif
078300050929    2C                   ENDIF
078400101202     c*
078500070110     C  n40KBRV5         READE     FNBRV07L                               40
078600050929    1C                   ENDDO
078700980401     C                   ENDSR
078800941222     C**************************************************************************
078900941222     C*    SEGNACOLLO ARRIVATO MA NON PARTITO
079000941222     C**************************************************************************
079100911015     C     MINORE        BEGSR
079200970602     C                   SETOFF                                       05
079300950707     C                   MOVEL     *BLANKS       WFLAG             1            DI COMODO
079400950811     C*
079500950811     C* PER VEDERE SE E' PARTITO SU ALTRO FV
079600070110     C* DEVO LEGGERE DA FNBRV LA PRIMA SPUNTA LA CUI FGS NON E' SIMFEL
079700950811     C                   Z-ADD     1             KNPG                           1=PARTENZE
079800941222     C                   Z-ADD     BRVLNP        KLNP                           LNP
079900941222     C                   Z-ADD     BRVLNA        KLNA                           LNA
080000941222     C                   Z-ADD     BRVNRS        KNRS                           SERIE
080100941222     C                   Z-ADD     BRVNSC        KNSC                           SEGNACOLLO
080200070110     C     KBRV4         SETGT     FNBRV09L
080300070110     C     KBRV4         READPE    FNBRV09L                               32
080400941227     C     SPPLPV        DOWEQ     SIMFEL
080500941223     C     *IN32         ANDEQ     *OFF
080600070110     C     KBRV4         READPE    FNBRV09L                               32
080700941222     C                   ENDDO
080800950811     C*
080900950811     C* 32 OFF - SPUNTA TROVATA: VERIFICO IL FOGLIO DEVE ESSERE  ESIST
081000940330    1C     *IN32         IFEQ      *OFF
081100970602     C                   SETON                                        05
081200101202     c*
081300040109     c* Per controllare se è abbinato, richiamo fnlv53
081400040109     C                   CLEAR                   DSLV53
081500040109     C                   MOVEL     'A'           D53TFO
081600040109     C                   Z-ADD     SIMFEL        D53FEL
081700040109     C                   Z-ADD     spplpv        D53FGS
081800040109     C                   Z-ADD     sppnfv        D53NFV
081900040109     C                   MOVEL     '1'           D53NPG
082000040109     C                   MOVEL     sppFLE        D53FLF
082100040109     C                   MOVEL     'B'           D53ABB
082200040109     C                   MOVEL     sppLNA        D53LNA
082300040109     C                   CALL      'FNLV53R'
082400040109     C                   PARM                    DSLV53
082500040109     c* se c'e' errore non trovato il foglio giusto - 32 on
082600040109     c                   if        d53err<>*blanks
082700040109     c                   seton                                        32
082800040109     c                   endif
082900930318     C*
083000950811     C* NON SEGNALO L'ANOMALIA DI PARTITO SU ALTRO FV SE:
083100950811     C* 1) LA LINEA DI INVIO DEL FOGLIO E' DIVERSA DALLA LINEA DEL
083200950811     C*  FOGLIO IDA CHE STO ELABORANDO
083300950811    2C     *IN32         IFEQ      *OFF
083400950811     C*
083500040109    3C     d53LAI        IFNE      DLRFGS
083600950811     C                   MOVEL     '1'           WFLAG
083700950811    3C                   ENDIF
083800950811     C*
083900950811     C* 2) LA LNP DEL F.V.  E' = TERM.ARR.  LNA SPUNTA
084000971128     C                   CLEAR                   DSLV55
084100971128     C                   MOVEL     'A'           D55TPT
084200971128     C                   MOVE      SPPLNA        D55LIN
084300040109     C                   Z-ADD     d53DFV        D55DRF
084400971128     C                   CALL      'FNLV55R'
084500971128     C                   PARM                    DSLV55
084600971128    3C     D55ERR        IFEQ      *BLANKS
084700040109     C     spplpv        ANDEQ     D55TFA
084800950811     C                   MOVEL     '1'           WFLAG
084900950811    3C                   ENDIF
085000950811    2C                   ENDIF
085100040109     c
085200040109     c* chain su fnfva per avere fvaatr non passato da lv53
085300040109     C  n32              Z-ADD     SPPNFV        WNFV
085400040109     c  n32KSPP3         chain     FNFVA01L
085500040109    1C                   ENDIF
085600930318     C**
085700950608     C* A R R I V A T O   M A   P A R T I T O   S U   A L T R O   F V
085800950608    1C     *IN32         IFEQ      *OFF
085900941227     C     FVAATR        ANDEQ     ' '                                          ' '=NO TRANSITO
086000950215     C     WFLAG         ANDEQ     *BLANKS
086100040109     C     d53DFA        ANDNE     WDFV
086200921222     C*
086300950811     C* A R R I V A T O   E   N O N   P A R T I T O
086400950608     C     *IN32         OREQ      *ON
086500921222     C*
086600921222     C* ERRORE: ARRIVATO MA PARTITO SU ALTRO FOGLIO VIAGGIO
086700940330    2C     *IN32         IFEQ      *OFF
086800920716     C                   MOVEL     ERR(3)        STAERR
086900040109     C                   MOVE      d53DFV        G02INV
087000911127     C                   MOVE      *ZEROS        G02DAT
087100911127     C                   MOVEL     3             G02ERR
087200941222     C                   CALL      'XSRDA8'
087300911127     C                   PARM                    WLBDAT
087400941223     C                   MOVEL     G02DAT        STSDFV            6 0          GG/MM/AA (6)
087500941222     C                   MOVE      G02DAT        DUE               2 0          ANNO (2)
087600941222     C                   MOVE      DUE           STSDFV
087700921222     C                   MOVEL     SPPNPS        STANPP                          PIST PARTEN
087800021214     C                   Z-ADD     Wanm160       COMCAA
087900940330    2C                   END
088000911127     C*
088100921222     C* ERRORE: ARRIVATO NON PARTITO
088200930318     C   32              MOVEL     ERR(2)        STAERR
088300950811     C   32              MOVEL     '  '          STANPP            2
088400021214     C   32              Z-ADD     wanm050       COMCAA
088500921222     C* CAMPI PER STAMPA ERRORE
088600911015     C                   MOVEL     BRVLNP        STALNP            3 0
088700911015     C                   MOVEL     BRVLNA        STALNA            3 0
088800911015     C                   MOVEL     BRVNRS        STANRS            2 0
088900911015     C                   MOVEL     BRVNSC        STASCN            7 0
089000911129     C                   MOVEL     BRVZNC        STAZNC            2 0
089100000127     C                   MOVEL     *ZEROS        STAFV             6 0
089200921126     C                   MOVEL     *BLANKS       STALPV            4
089300920716     C                   MOVEL     BRVNPS        STANPA            2
089400000512     C*
089500040109     C* CHAIN PER VEDERE COME E' L'ANOMALIA
089600000512    2C     COMCAA        IFNE      SAVCAA
089700000512     C                   MOVEL     '7C'          COD
089800000512     C                   MOVEL(P)  COMCAA        KEY
089900000512     C     KTAB          CHAIN     TABEL                              39
090000000512     C  N39              MOVEL     TBLUNI        DS7C
090100000512     C   39              CLEAR                   DS7C
090200000512     C                   MOVEL     COMCAA        SAVCAA            3 0
090300000512    2C                   ENDIF
090400000512     C*
090500000512    3C     §7CCPT        IFEQ      'S'
090600000512     C     §7CSPT        ANDEQ     'S'
090700000512   X3C                   ELSE
090800000512    4C     STALNP        IFNE      SAVLNP
090900000512     C     STALNP        CHAIN     AZORG                              39
091000000512     C  N39              MOVEL     ORGDES        DESLNP
091100000512     C   39              MOVEL     *BLANKS       DESLNP           10
091200000512     C                   MOVEL     STALNP        SAVLNP            3 0
091300000512     C  N39              MOVEL     ORGDE3        OG143
091400000512     C   39              CLEAR                   OG143
091500021122     c                   if        §ogntw='PPT'
091600021122     c                   movel     'S'           LNPPT
091700021213     c                   else
091800021213     c                   clear                   LNPPT
091900021122     c                   endif
092000000512    4C                   END
092100000512    2C                   ENDIF
092200000512     C*
092300940330     C*
092400940330     C* A N O M A L I A : COLLO ARRIVATO NON PARTITO
092500951002     C*                   NON LA CREO --> GIA' CREATA A SCARICO SPUNTE
092600951002     C*
092700951002     C*                   ARRIVATO MA PARTITO SU ALTRO FV
092800951002     C*                       LA CREO --> SOLO SE CHIUSURA IDA
092900951002    2C     *IN32         IFEQ      *OFF
093000951002     C     DLRFS1        ANDEQ     'S'
093100040109     C* VEDO SE ESCLUDERE BOLLE LNP POSTE
093200000512   2AC     LNPPT         IFNE      'S'
093300000515     C     §7CCPT        OREQ      'S'
093400940426     C*
093500040109    3C     d53NFA        IFNE      0
093600940426     C*
093700941223     C* LEGGO IL FOGLIO ABBINATO
093800040109     C                   Z-ADD     d53NPa        KNPG                           CATEGORIA F.ABBINATO
093900040109     C                   Z-ADD     d53NFA        KNFV                           NR.FOGLIO ABBINATO
094000040109     C                   Z-ADD     d53LAI        KFGS
094100020917     C     KFVV1         CHAIN(N)  FNFVV01L                           35
094200950811     C   35              CLEAR                   FVVDFV
094300941223     C*
094400040109    4C     d53DFV        IFNE      FVVDFV
094500940426     C*
094600021214     C                   Z-ADD     wanm160       COMCAA
094700990511     C     KANM          SETLL     FNANM000                               39
094800951002    5C  N39              DO
094900040531     C                   MOVEL     bolaas        ANMaas
095000040531     C                   MOVEL     bollnp        ANMlnp
095100040531     C                   MOVEL     bolnrs        ANMnrs
095200050707     C                   MOVEL     bolnsp        ANMnsp
095300040531     c
095400040531     c* collo di valore
095500040531     c                   eval      anmft4=§ar5bva
095600040531     C                   MOVEL     BRVLNP        ANMFLS
095700920429     C                   MOVEL     BRVLNA        ANMLNA
095800920429     C                   MOVEL     BRVNRS        ANMNRS
095900920429     C                   MOVEL     BRVNSC        ANMSCN
096000920429     C                   MOVEL     BRVNPS        ANMNPS
096100990511     C                   Z-ADD     BRVNFV        ANMNFV
096200950811     C                   MOVEL     BRVFGS        ANMCDU
096300950818     C                   MOVEL     SIMFEL        ANMFLE
096400950818     C                   Z-ADD     WDFV          ANMDAO
096500950818     C                   MOVEL     ANMCDU        WFL1
096600920429     C*
096700920429     C                   EXSR      CRTANM
096800951002    5C                   ENDDO
096900951002    4C                   ENDIF
097000951002    3C                   ENDIF
097100940426     C*
097200000512   2AC                   END
097300000512    2C                   END
097400911016     C*
097500911126     C* STAMPO DISCORDANZA
097600000512     C     LNPPT         IFNE      'S'
097700000512     C     §7CSPT        OREQ      'S'
097800911016     C                   EXSR      STAMPA
097900940330    1C                   END
098000000512    1C                   END
098100911015     C*
098200970602     C                   SETOFF                                       05
098300950810     C* SCRIVO LA SPUNTA MANCANTE IN MODO CHE NON TORNI A DARE ANOMALIA
098400950810     C*  SE E' PRESENTE IN ALTRA CATEGORIA
098500000209     C                   CLEAR                   FISPPWR
098600950810     C                   Z-ADD     BRVLNP        SPPLNP
098700950810     C                   Z-ADD     BRVLNA        SPPLNA
098800950810     C                   Z-ADD     BRVNRS        SPPNRS
098900950810     C                   Z-ADD     BRVNSC        SPPNSC
099000950810     C                   Z-ADD     BRVZNC        SPPZNC
099100000209     C                   WRITE     FISPPWR
099200911015     C                   ENDSR
099300941222     C**************************************************************************
099400941222     C* IMPOSTO CAMPI FISSI
099500941222     C**************************************************************************
099600911015     C     IMPO          BEGSR
099700930526     C*
099800950810     C* FOGLIO ARRIVI
099900941228     C                   Z-ADD     DLRNPG        KNPG                           CATEGORIA
100000941228     C                   Z-ADD     DLRNFV        KNFV                           NR.FOGLIO
100100020917     C                   Z-ADD     DLRFGS        KFGS                           FIL.GESTIONE
100200020917     C     KFVV1         CHAIN(N)  FNFVV01L                           32
100300930526     C*
100400930526     C                   MOVE      FVVDFV        G02INV
100500930526     C                   MOVE      *ZEROS        G02DAT
100600930526     C                   MOVEL     3             G02ERR
100700941222     C                   CALL      'XSRDA8'
100800930526     C                   PARM                    WLBDAT
100900941222     C                   Z-ADD     G02DAT        STADFV
101000950810     C*
101100950811     C                   MOVEL     FVVDFV        WDFV
101200980114     C                   MOVEL     FVVFGS        WFGS
101300930526     C*
101400930526     C*                FILIALE APERTURA
101500941223     C     FVVFGS        CHAIN     AZORG                              32
101600950811     C  N32              MOVEL     ORGDES        DESFGS           15
101700940607     C*
101800040109     c* controllo OGAS di simfel
101900040109     c     simfel        chain     azorg01l
102000040109     C                   MOVEL     ORGDE3        OG143
102100040109     C                   MOVEL     §OGAS         siOGAS
102200980114     C* DETERMINO TERMINAL DI ARRIVO DELLA FIL.GESTIONE
102300971009     C                   CLEAR                   DSLV55
102400980114     C                   MOVEL     'A'           D55TPT
102500971009     C                   Z-ADD     FVVFGS        D55LIN
102600971009     C                   Z-ADD     FVVDFV        D55DRF
102700971009     C                   CALL      'FNLV55R'
102800971009     C                   PARM                    DSLV55
102900980114     C                   Z-ADD     D55TFA        TFASAV
103000980114     C* 23 ON - E' UN SECONDO LIVELLO IN ARRIVO AVENTE UN TER-PART <>
103100040109     C*         DAL TER-PART. DEL TER-ARR  e <> OGAS
103200971009     C     D55TFA        IFNE      FVVFGS
103300980114     C                   CLEAR                   DSLV55
103400980114     C                   MOVEL     'P'           D55TPT
103500980114     C                   Z-ADD     TFASAV        D55LIN
103600980114     C                   Z-ADD     FVVDFV        D55DRF
103700980114     C                   CALL      'FNLV55R'
103800980114     C                   PARM                    DSLV55
103900980114     C     D55TFP        IFNE      SIMFEL
104000040109     C*
104100040109     c* controllo OGAS di simfel
104200040109     c     d55tfp        chain     azorg01l
104300040109     C                   MOVEL     ORGDE3        OG143
104400040109     C                   MOVEL     §OGAS         tpOGAS
104500040109     c                   if        tpogas<>siogas
104600940607     C                   SETON                                        23
104700940607     C                   ENDIF
104800040109     C                   ENDIF
104900980114     C                   ENDIF
105000980114     C* SE 23 OFF VERIFICO SE ESISTE ALMENO UN ECCEZIONE TER-ARR PER
105100980114     C* CUI LA FILIALE GESTIONE RISULTA UN 2° LIV IN ARRIVO AVENTE UN
105200980114     C* TER-PART <> DAL TER-PART DEL TER-ARRIVO
105300980114     C     *IN23         IFEQ      *OFF
105400980114     C     KCAE          SETLL     AZCAE02L
105500980114     C     KCAE          READE     AZCAE02L                               32
105600980114     C     *IN32         DOWEQ     *OFF
105700980114     C     *IN23         ANDEQ     *OFF
105800980114     C     CAETFP        IFGT      *ZEROS
105900980114     C     CAEATB        ANDEQ     *BLANKS
106000980114     C     CAEDDE        ANDLE     FVVDFV
106100980114     C     CAEDSC        ANDGE     FVVDFV
106200980114     C     CAETFE        ANDNE     FVVFGS
106300980114     C                   CLEAR                   DSLV55
106400980114     C                   MOVEL     'P'           D55TPT
106500980114     C                   Z-ADD     CAETFE        D55LIN
106600980114     C                   Z-ADD     FVVDFV        D55DRF
106700980114     C                   CALL      'FNLV55R'
106800980114     C                   PARM                    DSLV55
106900980114     C     D55TFP        IFNE      SIMFEL
107000040109     C*
107100040109     c* controllo OGAS di simfel
107200040109     c     d55tfp        chain     azorg01l
107300040109     C                   MOVEL     ORGDE3        OG143
107400040109     C                   MOVEL     §OGAS         tpOGAS
107500040109     c                   if        tpogas<>siogas
107600980114     C                   SETON                                        23
107700980114     C                   END
107800040109     C                   END
107900980114     C                   END
108000040109     c*
108100980114     C     KCAE          READE     AZCAE02L                               32
108200980114     C                   ENDDO
108300980114     C                   ENDIF
108400940607     C*
108500930526     C* CAMPI FISSI ANOMALIA
108600990511     C                   CLEAR                   FNANM000
108700921022     C                   MOVEL     'I'           ANMFSA
108800911015     C*
108900921124     C                   MOVEL     '-'           ALF1
109000960410     C*
109100911015     C                   SETON                                        OF
109200911109     C*
109300941228     C     DLRFS1        IFEQ      'S'
109400911109     C                   SETON                                        10
109500950810     C                   ENDIF
109600930205     C*
109700930205     C* DECODIFICA   CATEGORIA FOGLIO
109800930205     C                   MOVEL     '7N'          COD
109900950810     C                   MOVEL(P)  DLRNPG        KEY
110000930205     C     KTAB          CHAIN     TABEL                              32
110100930205     C  N32              MOVEL     TBLUNI        DESNPG           12
110200930526     C*
110300930526     C* CARICO TABELLA FILIALI GESTITE £1
110400030114     C                   CLEAR                   TRUL06ds
110500950810     C                   MOVE      '£1'          D06COD
110600950810     C                   MOVEL     'L'           D06TLA
110700950810     C                   MOVEL     SIMFEL        D06KEY
110800030114     C                   MOVEL     TRUL06ds      KPJBU
110900950810     C*
111000950810     C                   CALL      'TRUL06R'
111100950810     C                   PARM                    KPJBA
111200030114     C                   MOVEL     KPJBU         TRUL06ds
111300950810     C                   MOVEA     LIN           L1
111400000512     C** VEDO SE L'ANOMALIA 05 DEVE ESSERE CREATA E/O STMAPATA ANCHE
111500000512     C**  PER LE POSTE
111600021214     C                   Z-ADD     Wanm005       COMCAA
111700000512    1C     COMCAA        IFNE      SAVCAA
111800000512     C                   MOVEL     '7C'          COD
111900000512     C                   MOVEL(P)  COMCAA        KEY
112000000512     C     KTAB          CHAIN     TABEL                              39
112100000512     C  N39              MOVEL     TBLUNI        DS7C
112200000512     C   39              CLEAR                   DS7C
112300000512     C                   MOVEL     COMCAA        SAVCAA            3 0
112400000512    1C                   ENDIF
112500000512     C*
112600000512     C                   MOVEL     §7CCPT        W05CPT
112700000512     C                   MOVEL     §7CSPT        W05SPT
112800030114      *
112900030114      * Determino AS del p.o. gestione
113000030114     C     dlrfgs        CHAIN     AZORG01L                           32
113100030114     C                   MOVEL     ORGDE3        OG143
113200030114     C                   MOVEL     §OGAS         MyOGAS
113300030114     c*
113400911015     C                   ENDSR
113500950811     C**************************************************************************
113600950811     C* CHAIN PER VEDERE SE ESISTE LA BOLLA IN ARRIVO O LA BOLLA TRANS
113700950811     C**************************************************************************
113800040109     C     CHAFIL        BEGSR
113900950811     C                   SETOFF                                       333638
114000040531     c                   clear                   dar5gen
114100980807     C* USO LNP BOLLA PER PRENDERE TERMINAL DI ARRIVO DELLA SPUNTA
114200980401     C*  CHE STO LEGGENDO
114300980414     C* NEL CASO DI COLLO PARTITO NON ARRIVATO USO COME LNPB SEMPRE
114400980414     C*  LA LINEA DI PARTENZA FOGLIO VIAGGIO DELLA SPUNTA PARTENZA
114500980414     C* NEL CASO DI COLLO ARRIVATO NON PARTITO INVECE CERCO LA LNP
114600980414     C*  ORIGINALE DELLA BOLLA
114700021122     C*
114800980401     C* CHAIN FVA PER PRENDERE LA DATA DI PARTENZA
114900980807     C*  SOLO SE VENGO DALLA ROUTINE DI MAJORE
115000980807     C     WTIPO         IFEQ      'MA'
115100000127     C                   Z-ADD     SPPNFV        WNFV
115200980401     C     KSPP3         CHAIN     FNFVA01L                           34
115300980807     C* PER MINORE HO GIA' LA DATA DI FVV
115400980807     C                   ELSE
115500980807     C                   SETOFF                                       34
115600980807     C                   ENDIF
115700980807     C**
115800980807    1C     *IN34         IFEQ      *OFF
115900980401     C                   CLEAR                   DSLV55
116000980401     C                   MOVEL     'A'           D55TPT
116100980401     C                   Z-ADD     KLNA          D55LIN
116200980401     C                   Z-ADD     LNPB          D55LNP
116300980807     C     WTIPO         IFEQ      'MA'
116400980401     C                   Z-ADD     FVADFV        D55DRF
116500980807     C                   ELSE
116600980807     C                   Z-ADD     FVVDFV        D55DRF
116700980807     C                   ENDIF
116800980401     C                   CALL      'FNLV55R'
116900980401     C                   PARM                    DSLV55
117000101202     c*
117100980401    2C     D55TFA        IFEQ      DLRFGS
117200980414     C     KLNA          OREQ      DLRFGS
117300040531     c                   seton                                        33
117400950811     C     KART          CHAIN     FNART27L                           36
117500980401     C**
117600980401   X2C                   ELSE
117700040531     c                   setoff                                       33
117800021122     C     KBTT          CHAIN     FNBTT37L                           36
117900980401    2C                   ENDIF
118000980401    1C                   ENDIF
118100040531     c
118200040531     c* chain comunque su FNBLT per avere num.spedizione anche dei disguidi
118300040531     C     KART          CHAIN     FNblt27L
118400040531     c                   if        %found(fnblt27l)
118500040531     c                   eval      bolaas=bltaas
118600040531     c                   eval      bollnp=bltlnp
118700040531     c                   eval      bolnrs=bltnrs
118800040531     c                   eval      bolnsp=bltnsp
118900040531     c* verifico se si tratta di collo di valore per memorizzarlo su anomali
119000040531     c     kar5          chain     fiar501l
119100040531     c                   if        %found(fiar501l)
119200040531     c                   movel     ar5uni        dar5gen
119300040531     c                   endif
119400040531     c                   else
119500040531     c                   clear                   bolaas
119600040531     c                   clear                   bollnp
119700040531     c                   clear                   bolnrs
119800040531     c                   clear                   bolnsp
119900040531     c                   endif
120000950811     C                   ENDSR
120100941222     C**************************************************************************
120200941222     C*    AGGIORNO FOGLI VIAGGIO
120300941222     C**************************************************************************
120400911015     C     UPDFV         BEGSR
120500941222     C*
120600941228     C                   Z-ADD     DLRNPG        KNPG                           CATEGORIA
120700941228     C                   Z-ADD     DLRNFV        KNFV                           NR.FOGLIO
120800020917     C                   Z-ADD     DLRFGS        KFGS                           FIL.GESTIONE
120900020917     C     KFVV1         CHAIN     FNFVV01L                           32
121000911127     C  N32              MOVEL     'S'           FVVFCF
121100941222     C  N32              EXCEPT    UPDFVV
121200921124     C*
121300921124     C* SOLO SE INVIO IDD CREO IL RECORD PER LA PARTENZA
121400911015     C*
121500921124     C                   MOVEL     '7A'          COD
121600921124     C                   MOVEL     '1       '    KEY
121700921124     C     KTAB          CHAIN     TABEL                              32
121800921124     C  N32              MOVEL     TBLUNI        DS7A
121900921124     C   32              MOVEL     ' '           §7AIDD
122000921124     C*
122100950811    1C     §7AIDD        IFEQ      'S'
122200950817     C*
122300950811     C                   MOVEL     *ZEROS        ANMNRS
122400950811     C                   MOVEL     *ZEROS        ANMSCN
122500021214     C                   Z-ADD     Wanm090       COMCAA
122600950811     C                   MOVEL     *ZEROS        ANMNPS
122700950811     C                   MOVEL     *ZEROS        ANMNSP
122800950811     C                   MOVEL     *ZEROS        ANMAAS
122900950811     C                   MOVEL     *ZEROS        ANMFLS
123000950818     C                   Z-ADD     WDFV          ANMDAO
123100000515     C*
123200000515     C                   Z-ADD     COMCAA        ANMCAA
123300000515    1C     COMCAA        IFNE      SAVCAA
123400000515     C                   MOVEL     '7C'          COD
123500000515     C                   MOVEL(P)  ANMCAA        KEY
123600000515     C     KTAB          CHAIN     TABEL                              39
123700000515     C  N39              MOVEL     TBLUNI        DS7C
123800000515     C   39              CLEAR                   DS7C
123900000515     C                   MOVEL     COMCAA        SAVCAA            3 0
124000000515    1C                   ENDIF
124100000515     C*
124200950811     C* COME FILIALE GESTIONE METTO NON SIMFEL MA LA FILIALE CHE
124300950818     C*  PROPRIO HA FATTO L'IDA
124400950811     C                   Z-ADD     DLRFGS        ANMFLE
124500950811     C* CREO RECORD ANOMALIA --> "TUTTO OK" PER LA  FIL PARTENZA
124600950811     C*
124700950811     C* RILEGGO TUTTI I FV, PERCHE' L'OK IDA LO DEVO DARE ANCHE
124800950817     C*  PER QUELLI TRANSITO
124900950817     C*  ESCLUDO SOLO I FOGLI ARRIVATI ILLUMINATI PER I 2 LIVELLO
125000001122     C*  IN ARRIVO  (ELABORO SOLO FOGLIO LOCALE)
125100950811     C     KFVA1         SETLL     FNFVA000
125200950811     C     KFVA1         READE     FNFVA000                               32
125300950811     C*
125400950811    2C     *IN32         DOWEQ     *OFF
125500950817     C                   CLEAR                   WFLAG
125600950817     C*
125700911016     C                   MOVEL     FVALNP        ANMLNP
125800960412     C                   MOVEL     FVALNP        ANMCDU
125900990511     C                   Z-ADD     FVANFV        ANMNFV
126000921124     C                   MOVEL     FVALAI        ANMLNA
126100950816     C                   MOVEL     FVALNP        WFL1
126200950816     C                   CLEAR                   WFL2
126300000503     C*
126400000503     C     KFWA          CHAIN     FNFWA01L                           39
126500000503     C     *IN39         IFEQ      *ON
126600000503     C     FWAATB        ORNE      ' '
126700000503     C                   CLEAR                   FWAFF3
126800000503     C                   CLEAR                   FWAFF4
126900000503     C                   CLEAR                   FWAFL3
127000000503     C                   CLEAR                   FWAFL4
127100000515     C                   CLEAR                   FWAFGS
127200000503     C                   ENDIF
127300000503     C*
127400950817     C*
127500001122     C* PER I 2 LIVELLO IN ARRIVO ELABORO SOLO FOGLIO LOCALE
127600950817    3C     FVAATR        IFEQ      'T'
127700950817     C     *IN23         ANDEQ     *ON
127800001122     C     FVATTR        ANDNE     'L'
127900950817     C                   EXSR      CTRFVA
128000950817    3C                   ENDIF
128100950817     C*
128200950817    3C     WFLAG         IFEQ      ' '
128300000515     C*
128400000515    4C     §7CCPT        IFNE      'S'
128500000515     C                   CLEAR                   SAVLNP
128600000515     C                   CLEAR                   OG143
128700000515     C                   CLEAR                   FGSPT
128800000515    5C     FWAFGS        IFGT      0
128900000515     C     FWAFGS        CHAIN     AZORG01L                           32
129000000515     C  N32              MOVEL     ORGDE3        OG143
129100000515     C   32              CLEAR                   OG143
129200021122     c                   if        §ogntw='PPT'
129300021122     c                   movel     'S'           FGSPT             1
129400021213     c                   else
129500021213     c                   clear                   FGSPT
129600021122     c                   endif
129700101202     c*
129800000515    5C                   ENDIF
129900000515     C*
130000000515     C     FVALNP        CHAIN     AZORG01L                           32
130100000515     C  N32              MOVEL     ORGDE3        OG143
130200000515     C   32              CLEAR                   OG143
130300000515    4C                   ENDIF
130400000515     C**
130500021122     c     §OGNTW        ifne      'PPT'
130600021122    4C**   §OGPT         IFNE      'S'
130700000515    4C     FGSPT         ANDNE     'S'
130800000515     C     §7CCPT        OREQ      'S'
130900911016     C                   EXSR      CRTANM
131000000515    4C                   ENDIF
131100000515    3C                   ENDIF
131200911015     C*
131300950811     C     KFVA1         READE     FNFVA000                               32
131400950811    2C                   ENDDO
131500950811    1C                   ENDIF
131600961021     C*
131700961021     C                   CLEAR                   ANMLNP
131800961021     C                   CLEAR                   ANMNFV
131900961021     C                   Z-ADD     ANMFLE        ANMLNA
132000961021     C                   CLEAR                   ANMCDU
132100961021     C                   CLEAR                   WFL1
132200961021     C                   CLEAR                   WFL2
132300961021     C                   EXSR      CRTANM
132400911015     C*
132500911015     C                   ENDSR
132600950817     C**************************************************************************
132700951002     C* CONTROLLO I FV PER VEDERE SE CREARE L'ANOMALIA OK IDA
132800950817     C**************************************************************************
132900950817     C     CTRFVA        BEGSR
133000950817     C* VERIFICO PER LA FILIALE DI GESTIONE FOGLIO QUALE E' LO SCARICO
133100950817     C* 1) SE = AL TERMINAL DI ARRIVO ESCLUDO
133200950817     C* 2) SE TRANSITO, VERIFICO IL FOGLIO DELLA FILIALE DI TRANSITO
133300950817     C*    DOVE SCARICA
133400950817     C                   MOVEL     FVALAI        SAVLAI
133500950817     C                   EXSR      CARFVA
133600980203     C*
133700980203     C                   MOVE      '1'           WTROV
133800950817     C*
133900960502    1C     WFLAG         DOWEQ     ' '
134000950817     C     FVAATR        ANDEQ     'T'
134100950817     C     WFLP          ANDGT     0
134200980203     C     WTROV         ANDEQ     '1'
134300980203     C                   MOVE      *BLANKS       WTROV
134400950817     C*
134500950817     C* LEGGO FNFVA01L PER VEDERE LA FILIALE DI SCARICO:
134600950817     C     WFLP          SETGT     FNFVA01L
134700950817     C     WFLP          READPE    FNFVA01L                               31
134800950817     C*
134900960502    2C     *IN31         DOWEQ     *OFF
135000000503     C*
135100000503     C     KFWA          CHAIN     FNFWA01L                           39
135200000503     C     *IN39         IFEQ      *ON
135300000503     C     FWAATB        ORNE      ' '
135400000503     C                   CLEAR                   FWAFF3
135500000503     C                   CLEAR                   FWAFF4
135600000503     C                   CLEAR                   FWAFL3
135700000503     C                   CLEAR                   FWAFL4
135800000503     C                   ENDIF
135900980203     C* MEMORIZZO CHE HO TROVATO ALMENO UN RECORD CON QUESTA CHIAVE
136000980203     C                   MOVE      '1'           WTROV             1
136100950817     C* L'INVIO DEVE CORRISPONDERE A QUELLO CHE STO ELABORANDO
136200960502    3C     FVALAI        IFEQ      SAVLAI
136300960502     C* SE FOGLIO DIRETTO NON HO BISOGNO DI CONTROLLARE
136400960502    4C     FVAATR        IFEQ      ' '
136500950817     C                   SETON                                        31
136600950817     C                   ELSE
136700950817     C                   EXSR      CARFVA
136800960502    4C                   ENDIF
136900960502     C*
137000960502   X3C                   ELSE
137100960502     C* SE L'INVIO NON CORRISPONDE MA E' UN DIRETTO VERIFICO SE
137200960502     C*  LA LINEA SCARICA ALLA FIL DI INVIO
137300960502    4C     FVAATR        IFEQ      ' '
137400960502     C                   Z-ADD     FVALNA        LNA2
137500960502     C                   MOVEL     SAVLAI        ALF3
137600960502     C                   Z-ADD     1             X
137700960502     C     ALF3          LOOKUP    FFV(X)                                 33
137800960502     C   33              MOVEL     FLP(X)        W0030             3 0
137900960502    5C   33W0030         IFEQ      FVALAI
138000960502     C                   SETON                                        31
138100960502     C                   MOVEL     '1'           WFLAG
138200960502    5C                   ENDIF
138300960502    4C                   ENDIF
138400960502    3C                   ENDIF
138500950817     C*
138600950817     C  N31WFLP          READPE    FNFVA01L                               31
138700960502    2C                   ENDDO
138800960502    1C                   ENDDO
138900950817     C*
139000950817     C                   ENDSR
139100950817     C**************************************************************************
139200950817     C* CARICO LE LINEE DEL FV
139300950817     C**************************************************************************
139400950817     C     CARFVA        BEGSR
139500950817     C                   Z-ADD     FVALNA        LNA2
139600950817     C                   CLEAR                   WFLP
139700950817     C*
139800950817     C                   MOVEL     SAVLAI        ALF3
139900950817     C                   Z-ADD     1             X
140000950817     C     ALF3          LOOKUP    FFV(X)                                 33
140100950817     C* SE NON LO TROVO, ELABORO
140200950817     C     *IN33         IFEQ      *ON
140300950817     C                   MOVEL     FLP(X)        WFLP
140400980114     C* DETERMINO IL TERMINAL DI ARRIVO DELLA FILIALE GESTIONE
140500980114     C                   CLEAR                   DSLV55
140600980114     C                   MOVEL     'A'           D55TPT
140700980114     C                   Z-ADD     WFGS          D55LIN
140800980114     C                   Z-ADD     WDFV          D55DRF
140900980114     C                   Z-ADD     FVALNP        D55LNP
141000980114     C                   CALL      'FNLV55R'
141100980114     C                   PARM                    DSLV55
141200980114     C     WFLP          IFEQ      D55TFA
141300950817     C                   MOVEL     '1'           WFLAG
141400950817     C                   SETON                                        31
141500950817     C                   ENDIF
141600950817     C                   ENDIF
141700950817     C                   ENDSR
141800941222     C**************************************************************************
141900941222     C* CREO ANOMALIA
142000941222     C**************************************************************************
142100911016     C     CRTANM        BEGSR
142200941222     C*
142300040531     c* imposto data e ora creazione anomalia, sempre
142400050707     C                   TIME                    Wtime            14 0
142500050707     c                   movel     wtime         wora4             4 0
142600040531     c                   move      wora4         anmflo
142700050707     c
142800050707     C                   CLEAR                   WLBDA8
142900050707     C                   MOVE      Wtime         G08DAT
143000050707     C                   CALL      'XSRDA8'
143100050707     C                   PARM                    WLBDA8
143200050707     C                   MOVE      G08INV        wdata             6 0
143300050707     C                   MOVEl     wdata         anmflo
143400050707     c
143500040531     c* non imposto la zona
143600040531     c                   eval      anmfl4=999
143700040531     c
143800930526     C                   Z-ADD     COMCAA        ANMCAA
143900960515    1C     COMCAA        IFNE      SAVCAA
144000911016     C                   MOVEL     '7C'          COD
144100950811     C                   MOVEL(P)  ANMCAA        KEY
144200940330     C     KTAB          CHAIN     TABEL                              39
144300940330     C  N39              MOVEL     TBLUNI        DS7C
144400960515     C   39              CLEAR                   DS7C
144500930526     C                   MOVEL     COMCAA        SAVCAA            3 0
144600960515    1C                   ENDIF
144700911016     C*
144800970602     C* PER L'ANOMALIA 160 IMPOSTO FASE E FOGLIO DELLA SPUNTA CHE ME
144900970602     C*  LA CREA. PER LE ALTRE ANOMALIA TENGO LA FASE IDA
145000021214     C     COMCAA        IFNE      wanm160
145100960514     C                   MOVEL     'I'           ANMFSA
145200970602     C                   ELSE
145300970602     C                   MOVEL     A160FS        ANMFSA
145400970602     C                   ENDIF
145500911016     C                   MOVEL     §7CTAN        ANMTAN
145600911016     C                   MOVEL     §7CSAN        ANMSAN
145700911016     C                   MOVEL     §7CAIE        ANMAIE
145800911016     C                   Z-ADD     §7CLID        ANMLID
145900921022     C                   Z-ADD     0             ANMDCH
146000921022     C                   MOVEL     ' '           ANMFSC
146100960514     C                   MOVEL     '  '          ANMCCH
146200960514     C                   CLEAR                   ANMATB
146300911018     C* CHIUSURA AUTOMATICA
146400960515    1C     §7CCHA        IFEQ      'S'
146500950811     C                   Z-ADD     WDFV          ANMDCH
146600921022     C                   MOVEL     'I'           ANMFSC
146700960515    1C                   END
146800911018     C                   Z-ADD     0             ANMFL1
146900911109     C                   Z-ADD     0             ANMFL2
147000911109     C* ESTERNA
147100960515    1C     §7CAIE        IFEQ      'E'
147200911109     C* VEDO A CHI TRASMETTERE
147300950816     C                   MOVEL     WFL1          ANMFL1
147400950816     C                   MOVEL     WFL2          ANMFL2
147500921022     C* SE UGUALI CANCELLO IL 2°
147600960515    2C     ANMFL1        IFEQ      ANMFL2
147700911109     C                   Z-ADD     0             ANMFL2
147800960515   X2C                   ELSE
147900971128     C                   CLEAR                   DSLV55
148000971128     C                   MOVEL     'P'           D55TPT
148100971128     C                   MOVE      ANMFL1        D55LIN
148200971128     C                   MOVE      DATEU8        D55DRF
148300971128     C                   CALL      'FNLV55R'
148400971128     C                   PARM                    DSLV55
148500971128     C     D55ERR        IFNE      *BLANKS
148600971128     C                   MOVE      ANMFL1        WTFP1
148700971128     C                   ELSE
148800971128     C                   MOVE      D55TFP        WTFP1
148900971128     C                   END
149000971128     C                   CLEAR                   DSLV55
149100971128     C                   MOVEL     'P'           D55TPT
149200971128     C                   MOVE      ANMFL2        D55LIN
149300971128     C                   MOVE      DATEU8        D55DRF
149400971128     C                   CALL      'FNLV55R'
149500971128     C                   PARM                    DSLV55
149600971128     C     D55ERR        IFNE      *BLANKS
149700971128     C                   MOVE      ANMFL2        WTFP2
149800971128     C                   ELSE
149900971128     C                   MOVE      D55TFP        WTFP2
150000971128     C                   END
150100971128    3C     WTFP1         IFEQ      WTFP2
150200960410     C                   CLEAR                   ANMFL2
150300960515    3C                   END
150400960515    2C                   ENDIF
150500911109     C*
150600960515    1C                   ENDIF
150700911016     C*
150800960514     C* PER ANOMALIA 005 VEDO, SE CE N'E' UN'ALTRA, COSA FARE
150900960515    1C     ANMCAA        IFEQ      05
151000960514     C     WANOM         ANDEQ     'S'
151100960514     C**
151200960514     C                   Z-ADD     0             WC                1 0
151300960806     C                   CLEAR                   SKI
151400960515     C** CONTO A QUANTE FILIALI DEVO TRASMETTERE
151500960515    2C     WSAFL1        IFGT      0
151600960514     C                   ADD       1             WC
151700960514     C                   MOVEL     WSAFL1        SKI(WC)
151800960515    2C                   ENDIF
151900960515    2C     WSAFL2        IFGT      0
152000960514     C                   ADD       1             WC
152100960514     C                   MOVEL     WSAFL2        SKI(WC)
152200960515    2C                   ENDIF
152300960514     C*
152400960515    2C     ANMFL1        IFGT      0
152500960514     C     ANMFL1        LOOKUP    SKI                                    39
152600960514     C  N39              ADD       1             WC
152700960516     C  N39              MOVEL     ANMFL1        SKI(WC)
152800960515    2C                   ENDIF
152900960515    2C     ANMFL2        IFGT      0
153000960514     C     ANMFL2        LOOKUP    SKI                                    39
153100960514     C  N39              ADD       1             WC
153200960516     C  N39              MOVEL     ANMFL2        SKI(WC)
153300960515    2C                   ENDIF
153400960514     C**
153500960514     C* SE WC <= 2 ALLORA TENGO SOLO L'ULTIMA ANOMALIA
153600960517     C*  SE SI TRATTA DI UNA MIA FILIALE - ALTRIMENTI DEVO
153700960517     C*  TRASMETTERE L'ANNULLAMENTO DELLA ANOMALIA E LA SCRITTURA DELLA
153800960517     C*  NUOVA
153900960515    2C     WC            IFLE      2
154000960517     C     *IN36         ANDEQ     *OFF
154100960517    2C     WC            ORLE      2
154200960517     C     *IN38         ANDEQ     *OFF
154300960515     C*
154400960515    3C     ANMFL2        IFEQ      0
154500960515    4C     WSAFL1        IFNE      ANMFL1
154600960515     C                   Z-ADD     WSAFL1        ANMFL2
154700960515     C                   ELSE
154800960515    5C     WSAFL2        IFNE      ANMFL1
154900960515     C                   Z-ADD     WSAFL2        ANMFL2
155000960515    5C                   ENDIF
155100960515    4C                   ENDIF
155200960515    3C                   ENDIF
155300960515     C*
155400990511     C                   UPDATE    FNANMF
155500960515   X2C                   ELSE
155600970529     C* ENTRO QUI SE L'ANOMLIA E' PER UN MIO 2 LIVELLO IN ARRIVO SU
155700970529     C*  ALTRO AS O SE HO PIU' DI 2 P.O. DI TRASMISSIONE
155800960514     C*
155900960514     C* SCRIVO LA SECONDA ANOMALIA
156000990618     C     ANMFL1        IFGT      0
156100990618     C                   MOVEL     WSAFT1        ANMFT1
156200990618     C                   ENDIF
156300990618     C     ANMFL2        IFGT      0
156400990618     C                   MOVEL     WSAFT1        ANMFT2
156500990618     C                   ENDIF
156600990511     C                   WRITE     FNANM000
156700960515     C**
156800960515     C* SALVO LE FILIALI TRASMISSIONI CHE HO SCRITTO
156900960515     C                   MOVEL     ANMFL1        WSAFL1
157000960515     C                   MOVEL     ANMFL2        WSAFL2
157100960514     C*
157200960515     C* TENGO LA PRIMA MA CANCELLANDO LE FILIALI TRASMISSIONI = ALLA 2
157300970529     C* IMPOSTO SOGGETTO ANOMALIA = E (PER NON STAMPARE 2 VOLTE L'ANOM
157400970529     C*    NELL'IDD O PER NON FARLA VEDERE 2 VOLTE IN GESTIONE ANOMAL)
157500960514     C* REIMPOSTO IL LIVELLO IDD DALL'INIZIO
157600960514     C* RICHAINO
157700990511     C     WSANR2        CHAIN     FNANM00F                           39
157800960517     C                   MOVEL     §7CAIE        ANMAIE
157900960517     C                   CLEAR                   ANMDCH
158000960517     C                   CLEAR                   ANMCCH
158100960517     C                   CLEAR                   ANMFSC
158200960517     C*
158300960517    3C     WC            IFGT      2
158400960514     C                   MOVEL     'E'           ANMSAN
158500960514     C                   MOVEL     §7CLID        ANMLID
158600960515     C*
158700960517    4C     ANMFL1        IFEQ      WSAFL1
158800960515     C     ANMFL1        OREQ      WSAFL2
158900960515     C                   CLEAR                   ANMFL1
159000960517    4C                   ENDIF
159100960517    4C     ANMFL2        IFEQ      WSAFL1
159200960515     C     ANMFL2        OREQ      WSAFL2
159300960515     C                   CLEAR                   ANMFL2
159400960517    4C                   ENDIF
159500960515     C* SE ENTRAMBE = 0 (IMPOSSIBILE!!!) CANCELLO L'ANOMALIA
159600960517    4C     ANMFL1        IFEQ      0
159700960515     C     ANMFL2        ANDEQ     0
159800990511     C                   DELETE    FNANMF
159900960517   X4C                   ELSE
160000960515     C* SE E' LA 1 FILIALE CANCELLATA IMPOSTO LA 2 NELLA PRIMA
160100960517    5C     ANMFL1        IFEQ      0
160200960515     C                   Z-ADD     ANMFL2        ANMFL1
160300960515     C                   CLEAR                   ANMFL2
160400960517    5C                   ENDIF
160500960517     C*
160600990511     C                   UPDATE    FNANMF
160700960517    4C                   ENDIF
160800960517     C**
160900960517   X3C                   ELSE
161000960517     C**
161100960517     C* ANNULLO SOLAMENTE L'ANOMALIA
161200960517     C                   MOVEL     'A'           ANMATB
161300990511     C                   UPDATE    FNANMF
161400960517    3C                   ENDIF
161500960515     C*
161600960515    2C                   ENDIF
161700960514     C**
161800960515   X1C                   ELSE
161900960618     C                   CLEAR                   ANMFT1
162000960618     C                   CLEAR                   ANMFT2
162100990511     C                   WRITE     FNANM000
162200960515    1C                   ENDIF
162300941222     C*
162400911016     C                   ENDSR
162500941222     C**************************************************************************
162600941222     C* STAMPO DISCORDANZA
162700941222     C**************************************************************************
162800900621     C     STAMPA        BEGSR
162900941222     C*
163000911015     C* STAMPA TESTATA
163100911015     C   OF              DO
163200921124     C* PRIMA PARTE TESTATA
163300921124     C                   EXCEPT    TES
163400911015     C*
163500921124     C                   Z-ADD     1             I
163600921124     C                   Z-ADD     B             Y                 3 0
163700921124     C                   MOVEA     *BLANKS       STF
163800921124     C                   Z-ADD     0             SAVLAI
163900911015     C*
164000921124     C     Y             DOWLE     208
164100921124     C                   MOVEL     FVA(Y)        FVAPAR
164200921124     C*
164300921124     C     FVALAI        IFNE      SAVLAI
164400921124     C     I             IFGT      1
164500921124     C                   EXCEPT    TES2
164600921124     C                   MOVEA     *BLANKS       STF
164700921124     C                   Z-ADD     1             I
164800921124     C                   END
164900921124     C* SALVO NUOVA LINEA
165000950817     C                   MOVEL     FVALAI        SAVLAI
165100921124     C* DECODIFICA  LINEA
165200940330     C     SAVLAI        CHAIN     AZORG                              39
165300940330     C  N39              MOVEL     ORGDES        DESLAI           20
165400940330     C   39              MOVEL     *BLANKS       DESLAI
165500921124     C*
165600921124     C* STAMPO RIGA DI TESTA DELLA LINEA DI INVIO
165700921124     C                   EXCEPT    TES1
165800921124     C                   END
165900921124     C*
166000921124     C     I             IFEQ      131
166100921124     C                   EXCEPT    TES2
166200921124     C                   MOVEA     *BLANKS       STF
166300921124     C                   Z-ADD     1             I
166400921124     C                   END
166500921124     C*
166600921124     C                   MOVEL     FVALNP        ALF3
166700921124     C                   MOVEL     FVANFV        ALF5
166800921124     C                   MOVEA     FVAALF        STF(I)
166900921124     C                   ADD       10            I
167000921124     C**
167100921124     C                   ADD       1             Y
167200921124     C                   END
167300921124     C*
167400921124     C     I             IFGT      1
167500921124     C                   EXCEPT    TES2
167600921124     C                   END
167700911015     C*
167800911015     C* STAMPO TERZA PARTE TESTATA
167900911015     C                   EXCEPT    TES3
168000911015     C*
168100921124     C                   SETOFF                                       OF
168200911015     C                   END
168300911015     C*
168400921126     C* LINEE
168500921126     C     STALNA        IFNE      COMLNA
168600921126     C* LINEA ARRIVO
168700940330     C     STALNA        CHAIN     AZORG                              39
168800940330     C  N39              MOVEL     ORGDES        DESLNA
168900940330     C   39              MOVEL     *BLANKS       DESLNA           10
169000921126     C                   MOVEL     STALNA        COMLNA            3 0
169100921126     C                   END
169200911015     C* LINEA PARTENZA
169300921126     C     STALNP        IFNE      SAVLNP
169400940330     C     STALNP        CHAIN     AZORG                              39
169500940330     C  N39              MOVEL     ORGDES        DESLNP
169600940330     C   39              MOVEL     *BLANKS       DESLNP           10
169700921126     C                   MOVEL     STALNP        SAVLNP            3 0
169800000512     C  N39              MOVEL     ORGDE3        OG143
169900000512     C   39              CLEAR                   OG143
170000021122     c                   if        §ogntw='PPT'
170100021122     c                   movel     'S'           lnppt
170200021213     c                   else
170300021213     c                   clear                   LNPPT
170400021122     c                   endif
170500101202     c*
170600921126     C                   END
170700910522     C*
170800900622     C* STAMPO DETTAGLIO
170900040322     C                   EXCEPT    DET
171000900622     C*
171100900622     C                   ENDSR
171200950810     C*****************************************************************
171300950810     C* ESCLUDO LOCALI
171400950810     C*****************************************************************
171500950810     C     REDBRV        BEGSR
171600980807     C                   CLEAR                   WLNP
171700950810     C                   SETON                                        32
171800960212    1C     *IN32         DOWEQ     *ON
171900070110     C     KFVV2         READE     FNBRV01L                               31
172000960212     C   31              SETOFF                                       32
172100960606   1AC     *IN31         IFEQ      *OFF
172200960612     C*
172300020930     C     BRVLNP        LOOKUP    L1                                     32
172400020910     C*
172500020910     C* SE COLLO CON SERIE SE TROVO USO LNP BOLLA
172600150623     c*  ES- ora è da fare sempre perchè colli serie 00 possono partire da filiali diverse
172700150623    2C***  BRVNRS        IFGT      *ZEROS
172800960612     C*
172900020910     C                   MOVEL     '1'           WNOTR             1
173000020910     C* CERCO PRIMA IN BOLLE ARRIVI
173100020910     C     KSPP2         CHAIN     FNART27L                           35
173200020910     C  N35              CLEAR                   WNOTR
173300020910     C  N35ARTLNP        IFNE      BRVLNP
173400020910     C                   MOVE      ARTLNP        WLNP
173500020910     C                   END
173600960620     C* SE BOLLA NON TROVATA POTREBBE ESSERE EFFETTIVAMENTE MIA
173700960621     C     WNOTR         IFEQ      '1'
173800020919     C     KSPP2         CHAIN     FNBLT27L                           35
173900020919     C  N35              CLEAR                   WNOTR
174000020919     C  N35              MOVE      BLTLNP        WLNP
174100960620     C                   END
174200020910     C* ALTRIMENTI CERCO IN BOLLE TRANSITI
174300150623     C**** WNOTR         IFEQ      '1'
174400150623     C**** KSPP2         CHAIN     FNBTT27L                           35
174500150623     C**N35              CLEAR                   WNOTR
174600150623     C**N35              MOVE      BTTLNP        WLNP
174700150623     C*****              ENDIF
174800960621     C* SE BOLLA INESISTENTE CERCO SPUNTA PARTENZA
174900960621    5C     WNOTR         IFEQ      '1'
175000960621     C                   Z-ADD     1             KNPG
175100960621     C                   Z-ADD     BRVLNP        KLNP
175200960621     C                   Z-ADD     BRVLNA        KLNA
175300960621     C                   Z-ADD     BRVNRS        KNRS
175400960621     C                   Z-ADD     BRVNSC        KNSC
175500070110     C     KBRV4         CHAIN     FNBRV09L                           35
175600960621    6C     *IN35         DOWEQ     *OFF
175700960621     C     SPPATR        IFEQ      *BLANKS
175800960621     C     SPPLPV        IFNE      BRVLNP
175900960621     C                   MOVE      SPPLPV        WLNP
176000960703     C                   ELSE
176100960703     C                   CLEAR                   WLNP
176200960621     C                   END
176300960621    7C     SPPLPV        IFNE      SIMFEL
176400960621     C                   SETON                                        35
176500960621     C                   END
176600960621     C                   END
176700070110     C  N35KBRV4         READE     FNBRV09L                               35
176800960621    6C                   ENDDO
176900960621    5C                   END
177000960621     C*
177100960612     C     WLNP          IFGT      *ZEROS
177200960612     C     WLNP          LOOKUP    L1                                     32
177300960612     C                   END
177400150623    2C****               END
177500960606     C*
177600960212     C*
177700960606   1AC                   ENDIF
177800960212    1C                   ENDDO
177900960212     C*
178000950810     C                   ENDSR
178100950810     C**************************************************************************
178200950810     C* LETTURA DEI FOGLI CON CATEGORIA PASSATA E DATA = A DATA F.ARRVI
178300950810     C**************************************************************************
178400950810     C     REDFVV        BEGSR
178500950811     C     KFVV          SETLL     FNFVV002
178600950811     C     KFVV          READE     FNFVV002                               30
178700950810     C*
178800950810     C     *IN30         DOWEQ     *OFF
178900950811     C                   MOVEL     FVVNPG        KNPG3
179000000127     C                   Z-ADD     FVVNFV        KNFV3
179100950811     C                   MOVEL     FVVFGS        KFGS3
179200970602     C* RECUPERO LA FASE DEL FOGLIO
179300970602     C                   CLEAR                   DSUL11
179400970602     C                   MOVEL     FVVNPG        D11NPG
179500970602     C                   MOVEL     FVVSPG        D11SPG
179600970616     C                   CALL      'TRUL11R'
179700970602     C                   PARM                    DSUL11
179800970602     C                   MOVEL     D11FAS        A160FS            1
179900950810     C*
180000070110     C     KFVV2         SETLL     FNBRV000
180100950810     C                   EXSR      REDBRV
180200950810     C     *IN31         DOWEQ     *OFF
180300950810     C*
180400950810     C* VEDO SE ESISTE LA SPUNTA PARTENZA
180500000209     C     KSPP2         SETLL     FISPPWR                                34
180600950810     C* 34 OFF - NON TROVATA
180700950811     C     *IN34         IFEQ      *OFF
180800950811     C*
180900950811     C                   Z-ADD     BRVLNP        KLNP
181000950811     C                   Z-ADD     BRVLNA        KLNA
181100950811     C                   Z-ADD     BRVNRS        KNRS
181200950811     C                   Z-ADD     BRVNSC        KNSC
181300980414     C                   CLEAR                   LNPB
181400980807     C     WLNP          IFGT      0
181500980807     C                   Z-ADD     WLNP          LNPB
181600980807     C                   ELSE
181700980807     C                   Z-ADD     BRVLNP        LNPB
181800980807     C                   ENDIF
181900040611     C                   MOVEL     'MI'          WTIPO
182000950811     C                   EXSR      CHAFIL
182100980401     C** TERMINAL CON BOLLA E DATA ARRIVO = A DATA ELABORAZIONE
182200040531     C     *IN33         ifEQ      *ON
182300980401     C     *IN36         ANDEQ     *OFF
182400950811     C     ARTDAM        ANDEQ     WDFV
182500980401     C** TERMINAL SENZA BOLLA IN ARRIVO
182600980401     C     *IN33         OREQ      *ON
182700980401     C     *IN36         ANDEQ     *ON
182800980401     C** BOLLA TRANSITO CON DATA ENTRATA = A DATA ELABORAIZONE
182900950811     C     *IN33         OREQ      *OFF
183000980401     C     *IN36         ANDEQ     *OFF
183100950811     C     BTTDET        ANDEQ     WDFV
183200980401     C* SONO ESCLUSI I COLLI IN DISGUIDO
183300950811     C                   EXSR      MINORE
183400950811     C                   ENDIF
183500950811     C                   ENDIF
183600950810     C*
183700950810     C                   EXSR      REDBRV
183800950810     C                   ENDDO
183900950810     C*
184000950811     C     KFVV          READE     FNFVV002                               30
184100950810     C                   ENDDO
184200950810     C*
184300950810     C                   ENDSR
184400960410     C**************************************************************************
184500960410     C* CREAZIONE SPUNTA ARRIVI PISTOLA 89
184600960410     C**************************************************************************
184700960410     C     CRTSPU        BEGSR
184800070110     C                   CLEAR                   FNBRV000
184900960410     C                   MOVE      DLRNPG        BRVNPG
185000000127     C                   Z-ADD     DLRNFV        BRVNFV
185100960410     C                   MOVE      DLRFGS        BRVFGS
185200000127     C                   Z-ADD     DLRFGS        BRVPES
185300960410     C                   MOVE      SPPLNP        BRVLNP
185400960410     C                   MOVE      SPPLNA        BRVLNA
185500960410     C                   MOVE      SPPNRS        BRVNRS
185600960410     C                   MOVE      SPPNSC        BRVNSC
185700960410     C                   MOVE      SPPZNC        BRVZNC
185800960410     C                   MOVE      89            BRVNPS
185900070215     c                   time                    wtimez
186000070215     c* millisecondi
186100070215     c                   extrct    wtimez:*ms    millisec          6 0
186200070215     c                   movel     millisec      brvmis
186300070215     c* data immissione
186400070215     c                   movel     wtimez        dataiso
186500070215     c                   movel     dataiso       brvdfs
186600070215     c* ora immissione
186700070215     c                   move      wtimez        oraiso
186800070215     c                   movel     oraiso        brvhms
186900070215     c*
187000960410     C                   MOVE      SIMFEL        BRVFLE
187100070215     c                   movel     'A'           brvtap
187200070215     c                   eval      brvpru='§'+ nompgm
187300070110     C                   WRITE     FNBRV000                             34
187400960410     C     *IN34         IFEQ      *OFF
187500960430     C* Richiamo pgm per generare anomalia di disguido se si tratta di
187600960430     C* disguido; lo richiamo per aggiornare la bolla transito se si
187700960430     C* tratta di transito.
187800070222     c                   clear                   fnls45ds
187900070222     c                   movel     'S'           dls45ric
188000070222     c                   move      brvpes        dls45pes
188100070222     c                   call      'FNLS45R'
188200070222     c                   parm                    fnls45ds
188300070222     c                   parm                    dsbrv
188400970509     C**
188500970509     C                   MOVEL     'S'           WFC44             1
188600960410     C                   END
188700960410     C                   ENDSR
188800950810     C**************************************************************************
188900950810     C     *INZSR        BEGSR
189000101202     C                   Z-ADD     1             CODUT             1 0
189100101202      *
189200101202      * Reperimento dati utente/aziendali
189300101202     c                   exsr      sr_DatiJob
189400101202      *
189500040603     C* RILEVO DATA GIORNO IN AAAA/MM/GG  E ORA
189600040603     C                   TIME                    WTIME            14 0
189700040603     C                   MOVE      WTIME         WDATE                          GG/MM/AAAA
189800040603     C                   CLEAR                   WLBDA8
189900040603     C                   MOVE      WDATE         G08DAT
190000040603     C                   CALL      'XSRDA8'
190100040603     C                   PARM                    WLBDA8
190200040603     C                   MOVE      G08INV        DATEU8
190300040603     C                   MOVEL     WTIME         UTIME             6 0
190400950810     C*-----------------------------------------------------
190500030114     C     *LIKE         DEFINE    ANMCDU        WSACDU
190600030114     C     *LIKE         DEFINE    ANMNFV        WSANFV
190700030114     C     *LIKE         DEFINE    ANMNPS        WSANPS
190800030114     C     *LIKE         DEFINE    ANMFL1        WSAFL1
190900030114     C     *LIKE         DEFINE    ANMFT1        WSAFT1
191000030114     C     *LIKE         DEFINE    ANMFL2        WSAFL2
191100030114     C     *LIKE         DEFINE    BRVNPG        KNPG                           CATEGORIA
191200030114     C     *LIKE         DEFINE    BRVNPG        KNPG2                          CATEGORIA
191300030114     C     *LIKE         DEFINE    BRVNPG        KNPG3                          CATEGORIA
191400030114     C     *LIKE         DEFINE    BRVLNP        WLNP                           LNP
191500030114     C     *LIKE         DEFINE    BRVLNP        LNPB
191600030114     C     *LIKE         DEFINE    BRVLNP        KLNP                           LNP
191700030114     C     *LIKE         DEFINE    BRVLNA        KLNA                           LNA
191800030114     C     *LIKE         DEFINE    BRVNRS        KNRS                           SERIE
191900030114     C     *LIKE         DEFINE    BRVNSC        KNSC                           SEGNACOLLO
192000030114     C     *LIKE         DEFINE    BRVFGS        KFGS                           FIL.GESTIONE
192100030114     C     *LIKE         DEFINE    BRVFGS        KFGS3
192200030114     C     *LIKE         DEFINE    FVANFV        KNFV                           NR.FOGLIO
192300030114     C     *LIKE         DEFINE    FVANFV        WNFV                           NR.FOGLIO
192400030114     C     *LIKE         DEFINE    BRVNFV        KNFV3                          NR.FOGLIO
192500030114     C     *LIKE         DEFINE    FVVDFV        STADFV
192600030114     C     *LIKE         DEFINE    FVVDFV        WDFV
192700030114     C     *LIKE         DEFINE    FVVFGS        WFGS
192800030114     C     *LIKE         DEFINE    ANMFL1        WFL1
192900030114     C     *LIKE         DEFINE    ANMFL2        WFL2
193000030114     C     *LIKE         DEFINE    D55TFP        WTFP1
193100030114     C     *LIKE         DEFINE    D55TFP        WTFP2
193200030114     C     *LIKE         DEFINE    FVALAI        SAVLAI
193300030114     C     *LIKE         DEFINE    FVALAI        WFLP
193400030114     C     *LIKE         DEFINE    G08INV        DATEU8
193500030114     C     *LIKE         DEFINE    G08INV        WDATE
193600030114     C     *LIKE         DEFINE    D55TFA        TFASAV
193700030114     C     *LIKE         DEFINE    CAEEPA        KEPA
193800030114     C*-----------------------------------------------------
193900070110     C     KBRV2         KLIST                                                  FNBRV02L
194000950810     C                   KFLD                    KNPG                           ..CATEGORIA
194100000127     C                   KFLD                    KNFV3                          ..NR.FOGLIO
194200950810     C                   KFLD                    FVALNP                         ..FIL.GESTIONE
194300950810     C                   KFLD                    SAVLNA            3 0          ..LNA
194400070110     C     KBRV4         KLIST                                                  FNBRV09L
194500950810     C                   KFLD                    KNPG                           ..CATEGORIA
194600950810     C                   KFLD                    KLNP                           ..LNP
194700950810     C                   KFLD                    KLNA                           ..LNA
194800950810     C                   KFLD                    KNRS                           ..SERIE
194900950810     C                   KFLD                    KNSC                           ..SEGNACOLLO
195000070110     C     KBRV5         KLIST                                                  FnBRV09L
195100950811     C                   KFLD                    KLNP                           ..LNP
195200950811     C                   KFLD                    KLNA                           ..LNA
195300950811     C                   KFLD                    KNRS                           ..SERIE
195400950811     C                   KFLD                    KNSC                           ..SEGNACOLLO
195500040109     C****               KFLD                    KFGS                           ..FIL.GESTIONE
195600950810     C     KFVA1         KLIST                                                  FNFVA02L
195700950810     C                   KFLD                    DLRNPG                         ..CATEGORIA
195800950810     C                   KFLD                    DLRNFV                         ..NR.FOGLIO
195900020917     C                   KFLD                    DLRFGS
196000950810     C     KSPP2         KLIST                                                  *
196100950810     C                   KFLD                    BRVLNP                         *
196200950810     C                   KFLD                    BRVLNA                         *
196300950810     C                   KFLD                    BRVNRS                         *
196400950810     C                   KFLD                    BRVNSC                         *
196500950810     C     KSPP3         KLIST                                                  *
196600950810     C                   KFLD                    SPPLPV                         *
196700000127     C                   KFLD                    WNFV                           *
196800950810     C     KANM          KLIST                                                  *
196900950810     C                   KFLD                    STALNP                         *
197000950810     C                   KFLD                    STALNA                         *
197100950810     C                   KFLD                    STANRS                         *
197200950810     C                   KFLD                    STASCN                         *
197300950810     C                   KFLD                    COMCAA                         *
197400950810     C     KART          KLIST                                                  *
197500950811     C                   KFLD                    KLNP
197600950811     C                   KFLD                    KLNA
197700950811     C                   KFLD                    KNRS
197800950811     C                   KFLD                    KNSC
197900021122     C     KBTT          KLIST                                                  *
198000021122     C                   KFLD                    DLRFGS
198100021122     C                   KFLD                    KLNP
198200021122     C                   KFLD                    KLNA
198300021122     C                   KFLD                    KNRS
198400021122     C                   KFLD                    KNSC
198500950810     C     KARB          KLIST                                                  *
198600950810     C                   KFLD                    ARTAAS                         *
198700950810     C                   KFLD                    ARTLNP                         *
198800950810     C                   KFLD                    ARTNRS                         *
198900950810     C                   KFLD                    ARTNSP                         *
199000040531     C     KAR5          KLIST                                                  *
199100040531     C                   KFLD                    blTAAS                         *
199200040531     C                   KFLD                    blTLNP                         *
199300040531     C                   KFLD                    blTNRS                         *
199400040531     C                   KFLD                    blTNSP                         *
199500040531     C                   KFLD                    ktrd                           *
199600950810     C     KTAB          KLIST
199700950810     C                   KFLD                    CODUT
199800950810     C                   KFLD                    COD               2
199900950810     C                   KFLD                    KEY               8
200000950810     C     KFVV          KLIST                                                  FNFVV06L
200100950810     C                   KFLD                    KNPG2
200200950811     C                   KFLD                    WDFV
200300950817     C                   KFLD                    DLRFGS                                     NE
200400950810     C     KFVV1         KLIST                                                  FNFVV01L
200500950810     C                   KFLD                    KNPG                           ..CATEGORIA
200600950810     C                   KFLD                    KNFV                           ..NR.FOGLIO
200700950810     C                   KFLD                    KFGS                           ..FIL.GESTIONE
200800950810     C     KFVV2         KLIST                                                  FNFVV01L
200900950811     C                   KFLD                    KNPG3
201000950811     C                   KFLD                    KNFV3
201100950811     C                   KFLD                    KFGS3
201200980114     C*
201300980114     C     KCAE          KLIST
201400980114     C                   KFLD                    KEPA
201500980114     C                   KFLD                    FVVFGS
201600980114     C                   MOVEL     'A'           KEPA
201700000503     C*
201800000503     C     KFWA          KLIST
201900000503     C                   KFLD                    FVALNP
202000000503     C                   KFLD                    FVANFV
202100000503     C                   KFLD                    FVALAI
202200950810     C*
202300990623     C                   Z-ADD     0             COMDFS            8 0
202400990623     C                   Z-ADD     0             COMHMS            4 0
202500950810     C*---------------------------------------------------------------*
202600950810     C                   CLEAR                   COMMAN
202700950810     C                   MOVEA     CMD2          COMMAN
202800040322     C                   Z-ADD     51            LUNG             15 5
202900950810     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
203000040322     C                   PARM                    COMMAN           80
203100950810     C                   PARM                    LUNG
203200950810     C                   OPEN      QSYSPRT
203300160408     C***
203400160408     C*  Schiera codici pistola e relativa priorità
203500160408     C***
203600160408      /free
203700160408       ix=0;
203800160408       clear sps;
203900160408       clear sord ;
204000160408       setll (1:'7J') tabel00f;
204100160408       reade (1:'7J') tabel00f;
204200160408
204300160408  1    dow    not %eof(tabel00f)  ;
204400160408          ds7j=tbluni             ;
204500160408          ix=ix+1;
204600160408          sps(ix)=%dec(%subst(tblkey:1:2):2:0);
204700160408          sord(ix)=§7jord;
204800160408          reade (1:'7J') tabel00f  ;
204900160408  1    enddo     ;
205000160408      /end-free
205100030114      *
205200950810     C                   ENDSR
205300980401     C**************************************************************************
205400980401     C* TERMINAL DI ARRIVO DELLA FIL GESTIONE SPUNTA LETTA
205500980401     C**************************************************************************
205600980401     C     TERFGS        BEGSR
205700980401     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
205800980401     C                   CLEAR                   DSLV55
205900050929     C                   MOVEL     ' '           D55TPT
206000980401     C                   MOVEL     LNPB          D55LNP
206100980401     C                   MOVEL     FVADFV        D55DRF
206200980401     C                   MOVE      BRVFGS        D55LIN
206300980401     C                   CALL      'FNLV55R'
206400980401     C                   PARM                    DSLV55
206500980401     C**
206600980401     C     D55ERR        IFEQ      ' '
206700980401     C                   MOVE      D55TFA        FGSFE0            3 0
206800980401     C                   ELSE
206900980401     C                   MOVE      BRVFGS        FGSFE0
207000980401     C                   END
207100050929     c*
207200050929     c                   movel     d55tfp        fgstfp            3 0
207300980401     C                   ENDSR
207400101202
207500101202      *---------------------------------------------------------------*
207600101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
207700101202      *---------------------------------------------------------------*
207800101202     c     sr_DatiJob    BEGSR
207900101202      *
208000101202     c     *dtaara       define    §azute        AZUTEds
208100101202     c     *dtaara       define    §datiute      dDatiUte
208200101202      *
208300101202     c                   in(E)     *dtaara
208400101202     c                   IF        %Error or RSUT = *blanks
208500101202     c                   clear                   TIBS34ds
208600101202     c                   call      'TIBS34R'
208700101202     c                   parm                    TIBS34ds
208800101202     c                   in        *dtaara
208900101202     c                   ENDIF
209000101202      *
209100101202     c                   ENDSR
209200941222     C**************************************************************************
209300941222     OFNFVV000  E            UPDFVV
209400941222     O                       FVVFCF
209500941222     C**************************************************************************
209600911015     OQSYSPRT   E            TES              02
209700911015     O                       STA(1)              66
209800911015     O                       STA(2)             132
209900900626     O                       RSUT                20
210000911015     O                                           21 '/'
210100911015     O                       SIMFEL              24
210200040611     O                       WDATE              115 '  /  /    '
210300901115     O                       PAGE          Z    132
210400921124     O          E            TES         1
210500941222     O                       KNSIF               10
210600941222     O                       KNMUS               22
210700941222     O                       UTIME              115 '  :  :  '
210800921124     O          E            TES         1
210900911015     O                       STA(3)              66
211000911109     O               10      STA(9)             132
211100930205     O                       DESNPG              19
211200950817     O                       DLRFGS              26
211300950811     O                       DESFGS              42
211400941228     O                       DLRNPG        Z     48
211500941228     O                       DLRNFV        Z     54
211600941223     O                       STADFV              71 '  /  /    '
211700941222     O*-------------------------------------
211800921124     O          E            TES1        2
211900911015     O                       STA(4)              66
212000930205     O                       SAVLAI              40
212100921124     O                       DESLAI              61
212200941222     O*-------------------------------------
212300911015     O          E            TES2        1
212400921124     O                       STF                132
212500941222     O*-------------------------------------
212600920716     O          E            TES3        2
212700920716     O                       STA(6)              66
212800920716     O                       STA(11)            132
212900920716     O          E            TES3        1
213000920716     O                       STA(10)             66
213100920716     O                       STA(12)            132
213200941222     O*-------------------------------------
213300911015     O          E            DET         1
213400911015     O                       STALNP               3
213500921126     O                                         +  1 '-'
213600921126     O                       DESLNP            +  1
213700921126     O                       STALNA            +  2
213800921126     O                                         +  1 '-'
213900921126     O                       DESLNA            +  1
214000921126     O                       STANRS        Z   +  2
214100921126     O                       STASCN        Z   +  2
214200921126     O                       STAZNC            +  4
214300921126     O                       STALPV            +  2
214400921126     O                       STAFV         Z   +  0
214500921126     O                       STANPP            +  3
214600921126     O                       STANPA            +  6
214700000127     O                       STAERR            +  2
214800921126     O                  05   ERR(4)             122
214900000127     O                  05   SPPLPV             114
215000921126     O                  05   SPPNFV        Z    121
215100921126     O                  05                      123 '-'
215200941222     O                  05   STSDFV             132 '  /  /  '
215300941222     O**************************************************************************
215400960212**
215500941222XXXXXXXXXXXXXXXXXXXX/XXX        ** I.D.A. : INVENTARIO DISCORDANZE   1
215600950811 ARRIVI **                FNLR32R        XX/XX/XX        PAG. XXXX   2
215700941223FOGLIO XXXXXXXXXXXX DI XXX-XXXXXXXXXXXXXXX  N. X-XXXXX  DEL  XX/XX
215800921124FOGLI VIAGGIO PARTENZE ABBINATI PER: XXX-XXXXXXXXXXXXXXXXXXXX        4
215900921126XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XX   5
216000921126Linea Partenza    Linea Arrivo     Ser Segnacollo Zona F.Viaggio     6
216100921126XXX - XXXXXXXXXX  XXX - XXXXXXXXXX  xx  XXXXXXX    XX  AAAAXXXXX     7
216200920716I.D.A. NON ELABORATO A CAUSA DI ERRORI: RILANCIARE L'ELABORAZIONE!   8
216300920716               ***  C  O  N  F  E  R  M  A  ***                      9
216400000127                                                  Arr. Partenze     10
216500000127 Lett.BARCODE                                                       11
216600000127In Part In Arr                                                      12
216700921124XXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX     13
216800921126 XX      XX   XXXXXXXXXXAAAAAAAAAAAAAAAAAAAAAXXXXXXXXXXXXXXXXXXXXX  14
216900920716**
217000921126PARTITO  NON ARRIVATO                                                1
217100921126ARRIVATO NON PARTITO                                                 2
217200921126ARRIVATO  MA PARTITO                                                 3
217300000127SU F.V. N.XXX-XXXXXX - XX/XX/XX                                      4
217400941222**         CMD2
217500960212OVRPRTF FILE(QSYSPRT) USRDTA('I.D.A.')  HOLD(*YES)
