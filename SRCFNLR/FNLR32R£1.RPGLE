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
002200941222     FFNBTT27L  IF   E           K DISK
002300030114     FFNBTT37L  IF   E           K DISK    RENAME(FNbtt000:FNbtt037)
002400941222     FFNARB01L  UF   E           K DISK
002500980114     FAZCAE02L  IF   E           K DISK
002600030114     FQSYSPRT   O    F  132        PRINTER USROPN     OFLIND(*INOF)
002700030114
002800030114     D STA             S             66    DIM(14) CTDATA PERRCD(1)             SKIERE STAMPA
002900920716     D ERR             S             21    DIM(4) CTDATA PERRCD(1)              SKIERE ERRORE
003000960302     D CMD2            S             51    DIM(1) CTDATA PERRCD(1)              OVRPRTF
003100911015     D*
003200921124     D FVA             S             11  0 DIM(208)                             FV ABBINATI
003300921124     D STF             S              1    DIM(132)                             STAMPA FV ABB
003400030114     D L1              S              3  0 DIM(30)                              FIL GESTITE
003500921124     D*
003600960515     D SKI             S              3  0 DIM(4)                               LINEE TRANSITO
003700990511     D FNANM2          DS
003800960515     D  ANMNR2               397    400B 0
003900921124     D*LINEE   DI ARRIVO
004000921124     D                 DS
004100921124     D  FVALNA                 1      3  0
004200921124     D  FVAFFV                 4    243
004300921124     D  FVAFF2               244    450
004400000503     D  FWAFF3               451    690
004500000503     D  FWAFF4               691    900
004600000503     D  VUOTO                901    903
004700030114     D  FFV                    1    903    DIM(301)                             LINEE FOGLIO
004800921124     D                 DS
004900921124     D  FVALAI                 1      3  0
005000921124     D  FVALNP                 4      6  0
005100921124     D  FVANFV                 7     11  0
005200921124     D  FVAPAR                 1     11  0
005300921124     D                 DS
005400921124     D  ALF3                   1      3
005500921124     D  ALF1                   4      4
005600921124     D  ALF5                   5      9
005700921124     D  FVAALF                 1      9
005800921124     D*FILIALE DI SCARICO
005900921124     D                 DS
006000921124     D  LNA2                   1      3  0
006100921124     D  FVAFLP                 4    243
006200921124     D  FVAFL2               244    450
006300000503     D  FWAFL3               451    690
006400000503     D  FWAFL4               691    900
006500000503     D  VUOTO2               901    903
006600030114     D  FLP                    1    903    DIM(301)                             LINEE TRANSITO
006700911015     D WLBDAT          DS
006800941222     D  G02DAT                 1      8  0
006900941222     D  G02INV                 9     16  0
007000941222     D  G02ERR                17     17
007100941222     D  G02TGI                18     22  0
007200900622     D KPJBA         E DS
007300101202      *
007400101202      * - Parametri per richiamo al pgm di controllo profilo utenti
007500101202     d TIBS34ds      e ds
007600101202      * - Ds di riferimento al file esterno AzUte00F
007700101202     d AZUTEds       e ds                  ExtName(AzUte00F)
007800101202      * - Ds per dati organigramma
007900101202     d dDatiUte      e ds
008000101202      *
008100950810     D* DS PER TRUL06R - CARICAMENTO £X
008200030114     D TRUL06DS      E DS
008300030114     D  LIN                    1     90  0 DIM(30)                              FIL. COMODO
008400970602     D DSUL11          DS
008500970602     D  D11TLA                 1      1
008600970602     D  D11NPG                 2      2
008700970602     D  D11SPG                 3      3
008800970602     D  D11FAS                 4      4
008900040604     D* PASSAGGIO DATI A FNLR32R
009000040604     D PARA32          DS
009100040604     D  P32VALORE              1      1
009200040604     D  P32fgs                 2      4  0
009300040604     D  P32Tla                 5      5
009400040604     D* CAMPO DI OUTPUT
009500040604     D  P32MANCA               5      6
009600040604     D  P32AAS                 7     10  0
009700040604     D  P32LNP                11     13  0
009800040604     D  P32NRS                14     15  0
009900040604     D  P32NSP                16     22  0
010000040604     D  P32bva                23     23
010100040604     D* CAMPO DI input
010200040604     d* data foglio arrivi
010300040604     D  P32dfvv               24     31  0
010400941228     D DSLR01        E DS
010500000512     D OG143         E DS
010600960711     D DSTV          E DS
010700960711     D DS7A          E DS
010800911016     D DS7C          E DS
010900040531     D dar5gen       E DS
011000040109     D DSLV53        E DS                  EXTNAME(FNLV53DS)
011100971009     D DSLV55        E DS                  EXTNAME(FNLV55DS)
011200070222     D FNLS45DS      E DS
011300070110     D DSbrv         E DS                  EXTNAME(fnbrv00f)
011400960410     D WLBDA8          DS
011500960410     D  G08DAT                 1      8  0
011600960410     D  G08INV                 9     16  0
011700960410     D  G08ERR                17     17
011800960410     D  G08TGI                18     22  0
011900021214      *
012000030109     D Wanm005         s                   like(ANMCaa) inz(005)
012100030109     D Wanm050         s                   like(ANMCaa) inz(050)
012200030109     D Wanm090         s                   like(ANMCaa) inz(090)
012300030109     D Wanm160         s                   like(ANMCaa) inz(160)
012400040109     D SIogas          s                   like(§ogas)
012500040109     D MYogas          s                   like(§ogas)
012600040109     D TPogas          s                   like(§ogas)
012700030114     D W05cpt          s                   like(§7Ccpt)
012800030114     D W05spt          s                   like(§7Cspt)
012900030114     D laitfp          s                   like(d55tfp)
013000030114     D flptfp          s                   like(d55tfp)
013100040531     D bolaas          s                   like(bltaas)
013200040531     D bollnp          s                   like(bltlnp)
013300040531     D bolnrs          s                   like(bltnrs)
013400040531     D bolnsp          s                   like(bltnsp)
013500040531     D ktrd            s                   like(ar5trd) inz('GEN')
013600040611     D Wvalore         s                   like(p32valore)
013700040611     D PARBRV          s                   like(DSBRV)
013800050929     D katr            s                   like(sppatr)
013900070215     D wtimez          S               z
014000070215     D dataiso         S               d   datfmt(*iso)
014100070215     D oraiso          S               t   timfmt(*iso)
014200070215
014300070215     D                SDS
014400070215     D  NOMPGM                 1     10
014500030114
014600950816     I*
014700070110     I* PER CHIAREZZA DI PROGRAMMA (E PER NON SPORCARE I CAMPI DI FNBRV
014800070110     I* AL MOMENTO DI UTILIZZO DI FNBRV09L) RIDENOMINO TUTTI I CAMPI
014900070110     I* DI FNBRV09L, CHIAMANDOLI COME FISPP
015000070110     IFNBRV009
015100070110     I              BRVNVR                      SPPNVR
015200941223     I              BRVFGS                      SPPLPV
015300941223     I              BRVNPG                      SPPNPG
015400941223     I              BRVNFV                      SPPNFV
015500000127     I              BRVPES                      SPPPES
015600941223     I              BRVLNP                      SPPLNP
015700941223     I              BRVLNA                      SPPLNA
015800941223     I              BRVNRS                      SPPNRS
015900941223     I              BRVNSC                      SPPNSC
016000941223     I              BRVZNC                      SPPZNC
016100941223     I              BRVNPS                      SPPNPS
016200941223     I              BRVCAN                      SPPCAN
016300941223     I              BRVVUC                      SPPVUC
016400941223     I              BRVPUC                      SPPPUC
016500941223     I              BRVDCS                      SPPDCS
016600941223     I              BRVHCS                      SPPHCS
016700000127     I              BRVDFS                      SPPDFS
016800000127     I              BRVHMS                      SPPHMS
016900941223     I              BRVATR                      SPPATR
017000070110     I              BRVTSU                      SPPTSU
017100941223     I              BRVFTR                      SPPFTR
017200941223     I              BRVDTR                      SPPDTR
017300000127     I              BRVHTR                      SPPHTR
017400000127     I              BRVFLE                      SPPFLE
017500070110     I              BRVPRU                      SPPPRU
017600070110     I              BRVMIS                      SPPMIS
017700070110     I              BRVTAP                      SPPTAP
017800921124     I*---------------------------------------------------------------*
017900921124     I*   RIEPILOGO INDICATORI
018000921124     I*---------------------------------------------------------------*
018100950810     I* 05    - COLLO ARRIVATO SU ALTRO FOGLIO VIAGGIO
018200921126     I* 10    - CONFERMA IDA
018300980114     I* 23    - FILIALE DEL FOGLIO VIAGGIO 2 LIVELLO IN ARRIVO IN ALTRA
018400040109     I*         AREA DI PARTENZA  e <> OGAS
018500070110     I* 30    - LETTURA FNBRV
018600000209     I* 31    - LETTURA FISPP
018700921126     I* 32    - DI COMODO
018800950811     I* 33    - ON - LNA DELL'AREA  OFF-COLLO IN TRANSITO
018900921126     I* 34    - DI COMODO
019000950811     I* 36    - OFF  ESISTE BOLLA ARRIVO O BOLLA TRANSITO
019100921126     I* 37    - STAMPA ANOMALIA
019200950811     I* 38    - LNA DI CUI SONO SOLO TERMINAL
019300940330     I* 39    - DI COMODO
019400950811     I* 40    - LNA DI CUI SONO SOLO TERMINAL
019500100712     I* 41    - STAMPA ANOMALIA  senza crearla...
019600921124     I*---------------------------------------------------------------*
019700030114
019800900622     C     *ENTRY        PLIST
019900900622     C                   PARM                    KPJBA
020000040603     C                   PARM                    PARbrv
020100040604     C                   PARM                    PARa32
020200040604     c
020300040604     c                   select
020400040603     c* Se pgm richiamo per anom 05 --> eseguo solo routine majore
020500040604    0c                   when      %PARMS>1   and p32tla<>'C'
020600040603     c* Imposto il flag come definitivo
020700040603     c                   eval      dlrfs1='S'
020800040604     c                   eval      dlrfgs=p32fgs
020900040604     c                   eval      wdfv=p32dfvv
021000040611     c                   eval      Wvalore=p32valore
021100040611     c                   eval      DSBRV=PARBRV
021200040603     c*
021300040603     c* Imposto i campi della spunta nei campi di SPP
021400040603     c                   eval      spplnp=brvlnp
021500040603     c                   eval      spplna=brvlna
021600040603     c                   eval      sppnrs=brvnrs
021700040603     c                   eval      sppnsc=brvnsc
021800040603     c                   eval      sppnfv=brvnfv
021900040603     c                   eval      sppznc=brvznc
022000040603     c                   eval      spplpv=brvfgs
022100040603     c                   eval      sppnps=brvnps
022200040604     c* vedo se creare anomalia collo partito non arrivao
022300040603     c                   exsr      MAJORE
022400040604     c* chiusura files
022500040604    0c                   when      %PARMS>1   and p32tla='C'
022600040604     c
022700040604   x0c                   other
022800040604     c* pgm non richiamato
022900040603     c*
023000040611     c                   clear                   Wvalore
023100941228     C                   MOVEL     KPJBU         DSLR01
023200950811     C* IMPOSTAZIONI INIZIALI
023300911015     C                   EXSR      IMPO
023400950811     C* CREAZIONE FILE DI WORK, LEGGENDO LE SPUNTE PARTENZA DEI FVP ABB
023500911015     C                   EXSR      CRTWOK
023600950810     C*
023700950810     C* SEGNALO ANOMALIE  -->  PARTITO NON ARRIVATO
023800950817     C*
023900950817     C* NEI 2 LIVELLO IN ARRIVO TALE ANOMALIA NON VIENE CREATA IN
024000950817     C* QUANTO E'VALIDA QUELLA DEL TERMINAL DI ARRIVO
024100950817     C* INFATTI DI SPUNTE PARTENZA NON NE VIENE CARICATA NESSUNA
024200000209     C     *LOVAL        SETLL     FISPPWR
024300000209     C                   READ      FISPPWR                                31
024400950810     C*
024500000512    1C     *IN31         DOWEQ     *OFF
024600000512     C*
024700000512     C* SE PER LE BOLLE LNP POSTE DEVO SOLO O STAMPARE O CREARE
024800000512     C*  O NULLA, VEDO SE LA LNP E' POSTE
024900000512    2C     W05CPT        IFEQ      'S'
025000000512     C     W05SPT        ANDEQ     'S'
025100000512   X2C                   ELSE
025200000512    3C     SPPLNP        IFNE      SAVLNP
025300000512     C     SPPLNP        CHAIN     AZORG                              39
025400000512     C  N39              MOVEL     ORGDES        DESLNP
025500000512     C   39              MOVEL     *BLANKS       DESLNP           10
025600000512     C*
025700000512     C  N39              MOVEL     ORGDE3        OG143
025800000512     C   39              CLEAR                   OG143
025900021122     c                   if        §ogntw='PPT'
026000021122     c                   movel     'S'           LNPPT             1
026100021213     c                   else
026200021213     c                   clear                   LNPPT
026300021122     c                   endif
026400000512     C*
026500000512     C                   MOVEL     SPPLNP        SAVLNP            3 0
026600000512    3C                   ENDIF
026700000512    2C                   ENDIF
026800000512     C*
026900000512    2C     LNPPT         IFNE      'S'
027000000512     C     W05CPT        OREQ      'S'
027100000512     C     W05SPT        OREQ      'S'
027200950810     C                   EXSR      MAJORE
027300000512    2C                   ENDIF
027400000515     C                   READ      FISPPWR                                31
027500000512    1C                   ENDDO
027600950810     C*
027700950810     C* SEGNALO ANOMALIE  -->  ARRIVATO NON PARTITO
027800950810     C* LEGGO LE SPUNTE ARRIVI CONSEGNA E SPUNTA IMA DEI FOGLI CON DATA
027900950810     C*  UGUALE AL FOGLIO ARRIVI
028000950816     C*
028100950810     C                   Z-ADD     2             KNPG2
028200950810     C                   EXSR      REDFVV
028300950811     C*
028400950810     C                   Z-ADD     3             KNPG2
028500950810     C                   EXSR      REDFVV
028600950810     C*
028700950810     C                   Z-ADD     4             KNPG2
028800950810     C                   EXSR      REDFVV
028900910320     C*
029000921124     C* SE DEFINITIVO CHIUDO IL FOGLIO
029100941228     C     DLRFS1        IFEQ      'S'
029200911015     C                   EXSR      UPDFV
029300950810     C                   ENDIF
029400040603     c
029500040604    0C                   ENDSL
029600101202     c*
029700970509     C** CHIUSURA PGM
029800040604     c                   if        %parms=1 or p32tla='C'
029900970509     C     WFC44         IFEQ      'S'
030000070222     C                   MOVEL     'C'           dls45flg
030100070222     C                   CALL      'FNLS45R'
030200070222     c                   parm                    fnls45ds
030300070222     c                   parm                    dsbrv
030400970509     C                   ENDIF
030500971128     C*
030600971128     C                   MOVEL     'C'           D55TLA
030700971128     C                   CALL      'FNLV55R'
030800971128     C                   PARM                    DSLV55
030900040604     c                   endif
031000911015     C*
031100040604     c* Verifico come chiudere il pgm
031200040604     c                   if        %parms=1 or p32tla<>' '
031300000000     C                   SETON                                        LR
031400040604     c                   else
031500040604     C                   SETON                                        RT
031600040604     c                   endif
031700941222     C**************************************************************************
031800941222     C*    CREO FILE DI WORK
031900941222     C**************************************************************************
032000911015     C     CRTWOK        BEGSR
032100911015     C*
032200911015     C                   MOVEA     *ZEROS        FVA
032300921124     C                   Z-ADD     0             I
032400921124     C*
032500941227     C* ELABORO SOLTANTO I FOGLI VIAGGIO DIRETTI(FVAATR=' ') ESCLUDENDO
032600960711     C*  I FV DI DEFLUENZA DEL MIO TERMINAL
032700941222     C     KFVA1         SETLL     FNFVA02L
032800941222     C     KFVA1         READE     FNFVA02L                               30
032900911015     C*
033000940606    2C     *IN30         DOWEQ     '0'
033100980114     C*  DETERMINO IL TERMINAL DI ARRIVO DELLA FILIALE GESTIONE
033200980114     C                   CLEAR                   DSLV55
033300980114     C                   MOVEL     'A'           D55TPT
033400980114     C                   Z-ADD     WFGS          D55LIN
033500980114     C                   Z-ADD     WDFV          D55DRF
033600980114     C                   Z-ADD     FVALNP        D55LNP
033700980114     C                   CALL      'FNLV55R'
033800980114     C                   PARM                    DSLV55
033900960711     C*  VEDO SE FV DI DEFLUENZA
034000960711     C     *IN23         IFEQ      *ON
034100980114     C     FVALNP        ANDEQ     D55TFA
034200960711     C                   EXSR      CTRDEF
034300960711     C                   ENDIF
034400940607     C*
034500940607     C* SE NON SONO 2 LIV  IN ARRIVO                       -->ELABORO
034600040604     C* ELABORO I FOGLI DIRETTI (FVAATR =' ') ora i transiti sono a posto
034700040604     c*  al 100% anche per la sardegna per cui elaboro solo i fogli diretti
034800040604     c
034900980421     C* ELABORO ANCHE I FOGLI ILLUMINATI SE LA FIL DI TRANSITO E'
035000980421     C*  SULLO STESSO AS  RISPETTO A FVALAI
035100940607    4C     *IN23         IFEQ      *OFF
035200040604    3C     fvaatr        andeq     *blanks
035300070423     c* Elaboro anche i fogli illuminati se LOCALI
035400070423    4C     *IN23         oreq      *OFF
035500070423    3C     fvaatr        andeq     'T'
035600070423    3C     fvaTTR        andeq     'L'
035700940607     C* SE SONO 2 LIV IN ARR E FOGLIO NON DAL MIO TERMINAL -->ELABORO
035800940607     C     *IN23         OREQ      *ON
035900980114     C     FVALNP        ANDNE     D55TFA
036000960711    3C     FVAATR        ANDEQ     ' '
036100960711     C*  O DAL MIO TERMINAL MA FV LOCALE
036200960711     C     *IN23         OREQ      *ON
036300980114     C     FVALNP        ANDEQ     D55TFA
036400960711     C     §TVDEF        ANDNE     'S'
036500940607     C*
036600000503     C     KFWA          CHAIN     FNFWA01L                           39
036700000503     C     *IN39         IFEQ      *ON
036800000503     C     FWAATB        ORNE      ' '
036900000503     C                   CLEAR                   FWAFF3
037000000503     C                   CLEAR                   FWAFF4
037100000503     C                   CLEAR                   FWAFL3
037200000503     C                   CLEAR                   FWAFL4
037300000503     C                   ENDIF
037400000503     C*
037500921124     C                   ADD       1             I                 3 0
037600921124     C                   Z-ADD     FVAPAR        FVA(I)
037700921124     C*
037800921124     C                   Z-ADD     FVALNA        LNA2
037900911015     C*
038000911015     C* CREO FILE DI WORK
038100921124     C                   Z-ADD     1             X
038200921124     C                   MOVEL     FVALAI        ALF3              3
038300921126     C     ALF3          LOOKUP    FLP(X)                                 31
038400921124     C*
038500940607    5C     *IN31         DOWEQ     '1'
038600921222     C*
038700921222     C* CARICO LE SPUNTE PARTENZE
038800921222     C                   EXSR      CARSPP
038900921222     C*
039000921124     C                   ADD       1             X                 3 0
039100921124     C     ALF3          LOOKUP    FLP(X)                                 31
039200940607    5C                   END
039300921222     C*
039400921222     C* ELABORO LA LINEA DI INVIO SE NON ANCORA ELABORATA
039500921222     C                   Z-ADD     1             X
039600921222     C     ALF3          LOOKUP    FFV(X)                                 31
039700940607    5C   31ALF3          IFNE      FLP(X)
039800980421     C* SE FOGLIO ILLUMINATO E NON E' 2 LIVELLO IN ARRIVO,
039900980421     C*  ELABORO SOLO SE IL P.O. DI SCARICO HA LO STESSO TERMINAL PART
040000980421     C*  DI FVALAI
040100980421     C                   CLEAR                   WELAB
040200980421     C     *IN23         IFEQ      *OFF
040300980421     C                   CLEAR                   DSLV55
040400980421     C                   MOVEL     'P'           D55TPT
040500980421     C                   Z-ADD     FVALAI        D55LIN
040600980421     C                   Z-ADD     WDFV          D55DRF
040700980421     C                   CALL      'FNLV55R'
040800980421     C                   PARM                    DSLV55
040900980421     C                   MOVEL     D55TFP        LAITFP
041000980421     C**
041100980421     C                   CLEAR                   DSLV55
041200980421     C                   MOVEL     'P'           D55TPT
041300980421     C                   MOVEL     FLP(X)        D55LIN
041400980421     C                   Z-ADD     WDFV          D55DRF
041500980421     C                   CALL      'FNLV55R'
041600980421     C                   PARM                    DSLV55
041700980421     C                   MOVEL     D55TFP        FLPTFP
041800980421     C**
041900980421     C** CARICO LE SPUNTE PERCHE' ALTRIMENTI NON SI CREEREBBERO LE ANOM
042000980421     C*  05 DAL MOMENTO CHE IL TRANSITO (E LE BOLLE TRANSITO NON CI SON
042100980421     C*  O) E' SULLO STESSO AS E C'E' RIPARTENZA MA NON C'E' ABBINAMENT
042200980421     C*  IN ARRIVO E MANCA L'AUTOGENERAZIONE SPUNTA PARTENZA DA TRANS.
042300980421     C*  VISTO CHE LE BOLLE NON CI SONO. L'ANOM 05 DOVREBBE ESSERE
042400980421     C*  CREATA DALLE SPUNTE PARTENZA TRANSITO, MA NON AVENDOLE
042500980421     C*  MEGLIO CREARLE SULLE SPUNTE ORIGINALI DI PARTENZA CHE NIENTE
042600980421     C*  ES. 63 PER 14 TRANSITO 15 . SE MANCA UN COLLO LO CREO QUANDO
042700980421     C*  14 FA L'IDA DEL FOGLIO DI 63 ANCHE SE TEORICAMENTE DOVREBBE
042800980421     C*  FARLO SUL FOGLIO DI 15
042900980421     C     FLPTFP        IFEQ      LAITFP
043000980421     C                   MOVEL     'S'           WELAB
043100980421     C                   ENDIF
043200070423     c* Se si tratta di foglio locale, elaboro sempre la linea di
043300070423     c*  invio
043400070423     c                   if        fvattr='L'
043500070423     C                   MOVEL     'S'           WELAB
043600070423     C                   ENDIF
043700070423     c
043800980421     C                   ELSE
043900980421     C**
044000980421     C                   MOVEL     'S'           WELAB
044100980421     C                   ENDIF
044200921222     C*
044300921222     C* CARICO LE SPUNTE PARTENZE
044400980421     C     WELAB         IFEQ      'S'
044500921222     C                   EXSR      CARSPP
044600980421    5C                   ENDIF
044700921222     C*
044800940607    5C                   END
044900940607    4C                   END
045000921124     C**
045100921124     C**
045200941222     C     KFVA1         READE     FNFVA02L                               30
045300940606    2C                   END
045400921124     C**
045500921124     C*
045600921124     C                   SORTA     FVA
045700921124     C*
045800950810     C                   Z-ADD     1             B                 3 0
045900921124     C     FVA(208)      IFEQ      0
046000921124     C                   Z-ADD     209           B
046100921124     C                   ELSE
046200921124     C     FVA(B)        DOWEQ     0
046300921124     C                   ADD       1             B
046400921124     C                   END
046500921124     C                   END
046600910321     C*
046700910321     C                   ENDSR
046800960711     C**************************************************************************
046900960711     C*    CONTROLLO SE FV DI DEFLUENZA
047000960711     C**************************************************************************
047100960711     C     CTRDEF        BEGSR
047200960711     C                   MOVEL(P)  FVATTR        KEY
047300960711     C                   MOVEL     'TV'          COD
047400960711     C     KTAB          CHAIN     TABEL                              32
047500960711     C  N32              MOVEL     TBLUNI        DSTV
047600960711     C   32              CLEAR                   DSTV
047700960711     C                   ENDSR
047800941222     C**************************************************************************
047900941222     C*    CARICO SPUNTE DELLE LINEE SELEZIONATE DEL FOGLIO
048000941222     C**************************************************************************
048100921222     C     CARSPP        BEGSR
048200941222     C*
048300921222     C                   MOVEL     FFV(X)        SAVLNA
048400921222     C*
048500941222     C                   Z-ADD     1             KNPG                           1=PARTENZE
048600000127     C                   Z-ADD     FVANFV        KNFV3
048700070110     C     KBRV2         SETLL     FNBRV02L
048800070110     C     KBRV2         READE     FNBRV02L                               32
048900921222     C*
049000960506     C     *IN32         DOWEQ     *OFF
049100960506     C***** NON ESCLUDO PIU' I LOCALI PERCHE' ADESSO LA FILIALE SEGNAC
049200960506     C***** PUO' ESSERE <> DALLA FILIALE BOLLA E QUINDI ESSERE IN L1
049300960506     C***** MA NON ESSERE LOCALE PERCHE' PARTITO DA ALTRO AS
049400960506     C*****      BRVLNP    LOKUPL1                       31
049500960506     C**N31                DO
049600941223     C                   Z-ADD     BRVLNP        SPPLNP
049700941223     C                   Z-ADD     BRVLNA        SPPLNA
049800941223     C                   Z-ADD     BRVNRS        SPPNRS
049900941223     C                   Z-ADD     BRVNSC        SPPNSC
050000941223     C                   Z-ADD     BRVNFV        SPPNFV
050100941223     C                   Z-ADD     BRVZNC        SPPZNC
050200941223     C                   Z-ADD     BRVFGS        SPPLPV
050300941223     C                   Z-ADD     BRVNPS        SPPNPS
050400050929     C                   movel     BRVatr        SPPatr
050500000209     C                   WRITE     FISPPWR
050600921222     C*
050700070110     C     KBRV2         READE     FNBRV02L                               32
050800921222     C                   END
050900941222     C*
051000921222     C                   ENDSR
051100941222     C**************************************************************************
051200941222     C*    SEGNACOLLO PARTITO MA NON ARRIVATO
051300941222     C**************************************************************************
051400911015     C     MAJORE        BEGSR
051500100712     C                   SETOFF                                       3741
051600950811     C* CONTROLLO L'ESISTENZA DELLE BOLLE
051700950811     C                   Z-ADD     SPPLNP        KLNP
051800950811     C                   Z-ADD     SPPLNA        KLNA
051900950811     C                   Z-ADD     SPPNRS        KNRS
052000950811     C                   Z-ADD     SPPNSC        KNSC
052100980401     C                   Z-ADD     SIMFEL        KFGS
052200980414     C                   Z-ADD     SPPLPV        LNPB
052300050929     C                   movel     SPPatr        katr
052400040611     C                   MOVEL     'MA'          WTIPO             2
052500950811     C*
052600950811     C                   EXSR      CHAFIL
052700040603     c* solo se richiresto colli di valore, lo faccio solo per loro
052800040611    0c                   if        Wvalore='S' and §ar5bva='V' or
052900040611     c                             Wvalore=' '
053000911127     C*
053100980401     C                   SELECT
053200960513     C* VEDO SE ESISTE DATA ARRIVO --> NO ANOMALIA
053300950811     C* LNA DELL'AREA
053400980401    1C     *IN33         WHENEQ    *ON
053500980401     C     *IN36         ANDEQ     *OFF
053600980401     C**
053700980401    2C     ARTDCM        IFEQ      0
053800920207     C     ARTDAM        ANDEQ     0
053900911127     C                   SETON                                        37
054000980401    2C                   ENDIF
054100950811     C*
054200980401     C* NON TROVATA BOLLA --> FACCIO CHAIN SULLE SPUNTE
054300980401    1C     *IN33         WHENEQ    *ON
054400980401     C     *IN36         ANDEQ     *ON
054500980401     C* CERCO UNA SPUNTA NELL'AREA DI ARRIVO
054600980401     C                   EXSR      CERSPU
054700980401     C     WTROV         IFEQ      ' '
054800980401     C                   SETON                                        37
054900980401     C                   ENDIF
055000930617     C*
055100980401     C*    TRANSITO (NON GENERO ANOMALIA MA CREO SPUNTA ARR.PIST.89 SE
055200980401     C*              IDA DEFINITIVO)
055300980401    1C     *IN33         WHENEQ    *OFF
055400040611     c* i transiti li controllo solo se elaborazione da ida e non richiamato
055500040611   1ac                   if        %parms=1
055600040611    2C     *IN36         IFEQ      *OFF
055700950608     C*
055800980401    3C     BTTDET        IFEQ      0
055900000127     C     BTTDUT        ANDEQ     0
056000100712     c*
056100100712     c     bttdet        orgt      0
056200100712     c     bttpet        andeq     89
056300100712     C                   SETON                                        3741
056400100712     c
056500100712     C     DLRFS1        ifeq      'S'
056600100712    3C     BTTDET        andeq     0
056700100712     C     BTTDUT        ANDEQ     0
056800960430     C                   EXSR      CRTSPU
056900980401    3C                   ENDIF
057000100712     c
057100100712    3C                   ENDIF
057200980401   X2C                   ELSE
057300950608     C*
057400040531     C* 36 ON -        --> DISGUIDO (NON E' ANOMALIA PARTITO NON ARR.)
057500980401     C*                    CREO SPUNTA ARR. SOLO SE ELABORAZ.DEFINITIVA
057600980401     C**
057700980401    3C     DLRFS1        IFEQ      'S'
057800980401     C* CERCO UNA SPUNTA NELL'AREA DI ARRIVO
057900980401     C                   EXSR      CERSPU
058000980401    4C     WTROV         IFEQ      ' '
058100980401     C                   EXSR      CRTSPU
058200980401    4C                   ENDIF
058300980401    3C                   ENDIF
058400980401    2C                   ENDIF
058500040611   1aC                   ENDIF
058600980401     C**
058700980401     C                   ENDSL
058800950608     C*
058900950608     C*  3 7   O N   -   A N O M AL I A
059000930618    1C     *IN37         IFEQ      *ON
059100920716     C                   MOVEL     ERR(1)        STAERR           21
059200911015     C                   MOVEL     SPPLNP        STALNP            3 0
059300911015     C                   MOVEL     SPPLNA        STALNA            3 0
059400911015     C                   MOVEL     SPPNRS        STANRS            2 0
059500911015     C                   MOVEL     SPPNSC        STASCN            7 0
059600911129     C                   MOVEL     SPPZNC        STAZNC            2 0
059700000127     C                   Z-ADD     SPPNFV        STAFV             6 0
059800921126     C                   MOVEL     SPPLPV        STALPV            4
059900921126     C                   MOVE      '-'           STALPV
060000920716     C                   MOVEL     SPPNPS        STANPP
060100920716     C                   MOVEL     '  '          STANPA
060200911015     C*
060300000512     C* STAMPO PER NO POSTE E PER LNP POSTE, SE PREVISTO
060400000512     C     LNPPT         IFNE      'S'
060500000512     C     W05SPT        OREQ      'S'
060600040603     c* se pgm richiamato non stampa
060700040603     c                   if        %parms=1
060800911015     C                   EXSR      STAMPA
060900000512     C                   ENDIF
061000040603     C                   ENDIF
061100911015     C*
061200950707     C* SOLO SE ELABORAZIONE DEFINITIVA CREO L'ANOMALIA
061300100712     c*  per i transiti (41 on) non creo mai...
061400941228    2C     DLRFS1        IFEQ      'S'
061500100712    2C     *in41         andeq     *off
061600100712     c
061700000512   2AC     LNPPT         IFNE      'S'
061800000512     C     W05CPT        OREQ      'S'
061900040604     c
062000040604     c* Memorizzo che il collo manca se pgm richiamato
062100040604     c                   if        %parms>1
062200040604     c                   eval      p32manca='S'
062300040604     c                   eval      p32aas=bolaas
062400040604     c                   eval      p32lnp=bollnp
062500040604     c                   eval      p32nrs=bolnrs
062600040604     c                   eval      p32nsp=bolnsp
062700040604     c                   eval      p32bva=§ar5bva
062800040604     c                   endif
062900040604     c
063000021214     C                   Z-ADD     Wanm005       COMCAA            3 0
063100960515     C                   MOVEL     'S'           WELAB             1
063200960514     C                   CLEAR                   WANOM
063300960515     C*
063400990511     C     KANM          CHAIN     FNANM000                           32
063500960514     C*
063600960514     C* ELABORO SE NON TROVO L'ANOMALIA O IL FV E' DIVERSO
063700960514    3C     *IN32         DOWEQ     *OFF
063800960514     C* SE E' LO STESSO FOGLIO NON FACCIO NULLA
063900960514    4C     ANMNFV        IFEQ      SPPNFV
064000960514     C     ANMCDU        ANDEQ     SPPLPV
064100960514     C                   MOVEL     'N'           WELAB
064200960514     C                   SETON                                        32
064300960514     C                   ELSE
064400960515     C* SALVO I CAMPI CHE SERVONO E MEMORIZZO CHE C'E' GIA' L'ANOMALIA
064500960514     C                   MOVEL     'S'           WANOM             1
064600960514     C                   MOVEL     ANMFL1        WSAFL1
064700990618     C                   MOVEL     ANMFT1        WSAFT1
064800960514     C                   MOVEL     ANMFL2        WSAFL2
064900960514     C                   Z-ADD     ANMNR2        WSANR2            9 0
065000970529     C                   MOVEL     ANMNFV        WSANFV
065100970529     C                   Z-ADD     ANMCDU        WSACDU
065200980317     C                   Z-ADD     ANMNPS        WSANPS
065300960514     C*
065400990511     C     KANM          READE     FNANM000                               32
065500960514    4C                   ENDIF
065600960514    3C                   ENDDO
065700960514     C*
065800960514     C* SE L'ANOMALIA ESISTE
065900960514     C*  MEMORIZZO LE FILIALI TRASMISSIONI, IL LIVELLO IDD E IL NFV
066000960514    3C     WELAB         IFEQ      'S'
066100960514     C* SE HO TROVATO UN'ALTRA ANOMALIA LA RICHAINO
066200960514     C     WANOM         IFEQ      'S'
066300990511     C     WSANR2        CHAIN     FNANM00F                           32
066400960514     C                   ENDIF
066500960514     C*
066600040531     C                   MOVEL     bolaas        ANMaas
066700040531     C                   MOVEL     bollnp        ANMlnp
066800040531     C                   MOVEL     bolnrs        ANMnrs
066900040531     C                   MOVEL     bolnsp        ANMnsp
067000040531     c* memorizzo n anmft4 se collo di valore
067100040531     c                   eval      anmft4=       §ar5bva
067200040531     c
067300040531     C                   MOVEL     SPPLNP        ANMFLS
067400911015     C                   MOVEL     SPPLNA        ANMLNA
067500911015     C                   MOVEL     SPPNRS        ANMNRS
067600911015     C                   MOVEL     SPPNSC        ANMSCN
067700950608     C                   Z-ADD     SPPNPS        ANMNPS
067800990511     C                   Z-ADD     SPPNFV        ANMNFV
067900950810     C                   MOVEL     SPPLPV        ANMCDU
068000950816     C                   Z-ADD     WDFV          ANMDAO
068100960514     C                   MOVEL     DLRFGS        ANMFLE
068200950816     C                   MOVEL     SPPLPV        WFL2
068300950816     C     ANMLNP        IFGT      0
068400950816     C                   MOVEL     ANMLNP        WFL1
068500950816     C                   ELSE
068600950816     C                   MOVEL     ANMFLS        WFL1
068700950816     C                   ENDIF
068800970529     C* SE IL FOGLIO PER CUI STO CREANDO E' LA LNP DELLA BOLLA O COLLO
068900970529     C*  TENGO IL FOGLIO PRESENTE NELL'ANOMALIA SE NON PROVENIVA
069000970529     C*  DALLA PARTENZA
069100970616     C     WANOM         IFEQ      'S'
069200970616     C     SPPLPV        ANDEQ     WFL1
069300970529     C     WSACDU        ANDNE     WFL1
069400970529     C                   MOVEL     WSACDU        ANMCDU
069500970529     C                   MOVEL     WSANFV        ANMNFV
069600980317     C                   MOVEL     WSANPS        ANMNPS
069700970529     C                   ENDIF
069800911016     C*
069900911016     C                   EXSR      CRTANM
070000930618    3C                   END
070100911015     C*
070200040603     c* Eseguo aggiornamento bolla, solo se non richiamato
070300040603   3ac                   if        %parms=1
070400930618     C   33
070500930618    3CANN36              DO
070600941222     C     KARB          CHAIN     FNARB01L                           34
070700930621    4C  N34ARBAMA        IFLT      2
070800911015     C                   Z-ADD     0             ARBAMA
070900950608    4C                   ENDIF
071000911127     C*
071100941222     C  N34              UPDATE    FNARB000
071200950608    3C                   ENDDO
071300040603   3aC                   ENDif
071400101202     c*
071500000512   2AC                   END
071600000512    2C                   END
071700911015     C*
071800930618    1C                   END
071900040603    0C                   ENDif
072000941222     C*
072100911015     C                   ENDSR
072200980401     C**************************************************************************
072300980401     C* CERCA UNA SPUNTA DEL TERMINAL  O 2 LIVELLO CON STESSO TERMINAL
072400980401     C**************************************************************************
072500980401     C     CERSPU        BEGSR
072600980401     C                   CLEAR                   WTROV
072700070110     C     KBRV5         SETLL     FNBRV07L
072800070110     C     KBRV5         READE     FNBRV07L                               40
072900050929    1C     *IN40         DOWEQ     *OFF
073000980401     C                   EXSR      TERFGS
073100101202     c*
073200050929    2C     FGSFE0        IFEQ      DLRFGS
073300040109     c     brvfgs        oreq      dlrfgs
073400101202     c*
073500050929     c* Per le spunte tipiche partenza, imp entrata e partenza, considero
073600050929     c*  come non trovata se la spunta ha come terminal partenza il
073700050929     c*  p.o  gestione della spunta partenza per la quale devo creare l'anom
073800050929    3c                   if        katr=' ' and fgstfp=lnpb
073900050929    4c                   if        brvnpg=3 or brvnpg=1
074000050929     c* cerco con fnlv53 se foglio imp o defluenza
074100050929     C                   CLEAR                   DSLV53
074200050929     C                   Z-ADD     SIMFEL        D53FEL
074300050929     C                   Z-ADD     brvfgs        D53FGS
074400050929     C                   Z-ADD     brvnfv        D53NFV
074500050929     C                   MOVEL     brvnpg        D53NPG
074600050929     C                   CALL      'FNLV53R'
074700050929     C                   PARM                    DSLV53
074800050929    4c                   endif
074900101202     c*
075000101202     c*
075100050929    4c                   if        brvnpg=5 or
075200050929     c                             (brvnpg=3 and d53spg='P') or
075300050929     c                             (brvnpg=1 and d53def=' ')
075400050929   x4c                   else
075500050929     C                   MOVEL     'S'           WTROV             1
075600050929     C                   SETON                                        40
075700050929    4c                   endif
075800101202     c*
075900050929   x3c                   else
076000980401     C                   MOVEL     'S'           WTROV             1
076100980401     C                   SETON                                        40
076200050929    3c                   endif
076300050929    2C                   ENDIF
076400101202     c*
076500070110     C  n40KBRV5         READE     FNBRV07L                               40
076600050929    1C                   ENDDO
076700980401     C                   ENDSR
076800941222     C**************************************************************************
076900941222     C*    SEGNACOLLO ARRIVATO MA NON PARTITO
077000941222     C**************************************************************************
077100911015     C     MINORE        BEGSR
077200970602     C                   SETOFF                                       05
077300950707     C                   MOVEL     *BLANKS       WFLAG             1            DI COMODO
077400950811     C*
077500950811     C* PER VEDERE SE E' PARTITO SU ALTRO FV
077600070110     C* DEVO LEGGERE DA FNBRV LA PRIMA SPUNTA LA CUI FGS NON E' SIMFEL
077700950811     C                   Z-ADD     1             KNPG                           1=PARTENZE
077800941222     C                   Z-ADD     BRVLNP        KLNP                           LNP
077900941222     C                   Z-ADD     BRVLNA        KLNA                           LNA
078000941222     C                   Z-ADD     BRVNRS        KNRS                           SERIE
078100941222     C                   Z-ADD     BRVNSC        KNSC                           SEGNACOLLO
078200070110     C     KBRV4         SETGT     FNBRV09L
078300070110     C     KBRV4         READPE    FNBRV09L                               32
078400941227     C     SPPLPV        DOWEQ     SIMFEL
078500941223     C     *IN32         ANDEQ     *OFF
078600070110     C     KBRV4         READPE    FNBRV09L                               32
078700941222     C                   ENDDO
078800950811     C*
078900950811     C* 32 OFF - SPUNTA TROVATA: VERIFICO IL FOGLIO DEVE ESSERE  ESIST
079000940330    1C     *IN32         IFEQ      *OFF
079100970602     C                   SETON                                        05
079200101202     c*
079300040109     c* Per controllare se è abbinato, richiamo fnlv53
079400040109     C                   CLEAR                   DSLV53
079500040109     C                   MOVEL     'A'           D53TFO
079600040109     C                   Z-ADD     SIMFEL        D53FEL
079700040109     C                   Z-ADD     spplpv        D53FGS
079800040109     C                   Z-ADD     sppnfv        D53NFV
079900040109     C                   MOVEL     '1'           D53NPG
080000040109     C                   MOVEL     sppFLE        D53FLF
080100040109     C                   MOVEL     'B'           D53ABB
080200040109     C                   MOVEL     sppLNA        D53LNA
080300040109     C                   CALL      'FNLV53R'
080400040109     C                   PARM                    DSLV53
080500040109     c* se c'e' errore non trovato il foglio giusto - 32 on
080600040109     c                   if        d53err<>*blanks
080700040109     c                   seton                                        32
080800040109     c                   endif
080900930318     C*
081000950811     C* NON SEGNALO L'ANOMALIA DI PARTITO SU ALTRO FV SE:
081100950811     C* 1) LA LINEA DI INVIO DEL FOGLIO E' DIVERSA DALLA LINEA DEL
081200950811     C*  FOGLIO IDA CHE STO ELABORANDO
081300950811    2C     *IN32         IFEQ      *OFF
081400950811     C*
081500040109    3C     d53LAI        IFNE      DLRFGS
081600950811     C                   MOVEL     '1'           WFLAG
081700950811    3C                   ENDIF
081800950811     C*
081900950811     C* 2) LA LNP DEL F.V.  E' = TERM.ARR.  LNA SPUNTA
082000971128     C                   CLEAR                   DSLV55
082100971128     C                   MOVEL     'A'           D55TPT
082200971128     C                   MOVE      SPPLNA        D55LIN
082300040109     C                   Z-ADD     d53DFV        D55DRF
082400971128     C                   CALL      'FNLV55R'
082500971128     C                   PARM                    DSLV55
082600971128    3C     D55ERR        IFEQ      *BLANKS
082700040109     C     spplpv        ANDEQ     D55TFA
082800950811     C                   MOVEL     '1'           WFLAG
082900950811    3C                   ENDIF
083000950811    2C                   ENDIF
083100040109     c
083200040109     c* chain su fnfva per avere fvaatr non passato da lv53
083300040109     C  n32              Z-ADD     SPPNFV        WNFV
083400040109     c  n32KSPP3         chain     FNFVA01L
083500040109    1C                   ENDIF
083600930318     C**
083700950608     C* A R R I V A T O   M A   P A R T I T O   S U   A L T R O   F V
083800950608    1C     *IN32         IFEQ      *OFF
083900941227     C     FVAATR        ANDEQ     ' '                                          ' '=NO TRANSITO
084000950215     C     WFLAG         ANDEQ     *BLANKS
084100040109     C     d53DFA        ANDNE     WDFV
084200921222     C*
084300950811     C* A R R I V A T O   E   N O N   P A R T I T O
084400950608     C     *IN32         OREQ      *ON
084500921222     C*
084600921222     C* ERRORE: ARRIVATO MA PARTITO SU ALTRO FOGLIO VIAGGIO
084700940330    2C     *IN32         IFEQ      *OFF
084800920716     C                   MOVEL     ERR(3)        STAERR
084900040109     C                   MOVE      d53DFV        G02INV
085000911127     C                   MOVE      *ZEROS        G02DAT
085100911127     C                   MOVEL     3             G02ERR
085200941222     C                   CALL      'XSRDA8'
085300911127     C                   PARM                    WLBDAT
085400941223     C                   MOVEL     G02DAT        STSDFV            6 0          GG/MM/AA (6)
085500941222     C                   MOVE      G02DAT        DUE               2 0          ANNO (2)
085600941222     C                   MOVE      DUE           STSDFV
085700921222     C                   MOVEL     SPPNPS        STANPP                          PIST PARTEN
085800021214     C                   Z-ADD     Wanm160       COMCAA
085900940330    2C                   END
086000911127     C*
086100921222     C* ERRORE: ARRIVATO NON PARTITO
086200930318     C   32              MOVEL     ERR(2)        STAERR
086300950811     C   32              MOVEL     '  '          STANPP            2
086400021214     C   32              Z-ADD     wanm050       COMCAA
086500921222     C* CAMPI PER STAMPA ERRORE
086600911015     C                   MOVEL     BRVLNP        STALNP            3 0
086700911015     C                   MOVEL     BRVLNA        STALNA            3 0
086800911015     C                   MOVEL     BRVNRS        STANRS            2 0
086900911015     C                   MOVEL     BRVNSC        STASCN            7 0
087000911129     C                   MOVEL     BRVZNC        STAZNC            2 0
087100000127     C                   MOVEL     *ZEROS        STAFV             6 0
087200921126     C                   MOVEL     *BLANKS       STALPV            4
087300920716     C                   MOVEL     BRVNPS        STANPA            2
087400000512     C*
087500040109     C* CHAIN PER VEDERE COME E' L'ANOMALIA
087600000512    2C     COMCAA        IFNE      SAVCAA
087700000512     C                   MOVEL     '7C'          COD
087800000512     C                   MOVEL(P)  COMCAA        KEY
087900000512     C     KTAB          CHAIN     TABEL                              39
088000000512     C  N39              MOVEL     TBLUNI        DS7C
088100000512     C   39              CLEAR                   DS7C
088200000512     C                   MOVEL     COMCAA        SAVCAA            3 0
088300000512    2C                   ENDIF
088400000512     C*
088500000512    3C     §7CCPT        IFEQ      'S'
088600000512     C     §7CSPT        ANDEQ     'S'
088700000512   X3C                   ELSE
088800000512    4C     STALNP        IFNE      SAVLNP
088900000512     C     STALNP        CHAIN     AZORG                              39
089000000512     C  N39              MOVEL     ORGDES        DESLNP
089100000512     C   39              MOVEL     *BLANKS       DESLNP           10
089200000512     C                   MOVEL     STALNP        SAVLNP            3 0
089300000512     C  N39              MOVEL     ORGDE3        OG143
089400000512     C   39              CLEAR                   OG143
089500021122     c                   if        §ogntw='PPT'
089600021122     c                   movel     'S'           LNPPT
089700021213     c                   else
089800021213     c                   clear                   LNPPT
089900021122     c                   endif
090000000512    4C                   END
090100000512    2C                   ENDIF
090200000512     C*
090300940330     C*
090400940330     C* A N O M A L I A : COLLO ARRIVATO NON PARTITO
090500951002     C*                   NON LA CREO --> GIA' CREATA A SCARICO SPUNTE
090600951002     C*
090700951002     C*                   ARRIVATO MA PARTITO SU ALTRO FV
090800951002     C*                       LA CREO --> SOLO SE CHIUSURA IDA
090900951002    2C     *IN32         IFEQ      *OFF
091000951002     C     DLRFS1        ANDEQ     'S'
091100040109     C* VEDO SE ESCLUDERE BOLLE LNP POSTE
091200000512   2AC     LNPPT         IFNE      'S'
091300000515     C     §7CCPT        OREQ      'S'
091400940426     C*
091500040109    3C     d53NFA        IFNE      0
091600940426     C*
091700941223     C* LEGGO IL FOGLIO ABBINATO
091800040109     C                   Z-ADD     d53NPa        KNPG                           CATEGORIA F.ABBINATO
091900040109     C                   Z-ADD     d53NFA        KNFV                           NR.FOGLIO ABBINATO
092000040109     C                   Z-ADD     d53LAI        KFGS
092100020917     C     KFVV1         CHAIN(N)  FNFVV01L                           35
092200950811     C   35              CLEAR                   FVVDFV
092300941223     C*
092400040109    4C     d53DFV        IFNE      FVVDFV
092500940426     C*
092600021214     C                   Z-ADD     wanm160       COMCAA
092700990511     C     KANM          SETLL     FNANM000                               39
092800951002    5C  N39              DO
092900040531     C                   MOVEL     bolaas        ANMaas
093000040531     C                   MOVEL     bollnp        ANMlnp
093100040531     C                   MOVEL     bolnrs        ANMnrs
093200050707     C                   MOVEL     bolnsp        ANMnsp
093300040531     c
093400040531     c* collo di valore
093500040531     c                   eval      anmft4=§ar5bva
093600040531     C                   MOVEL     BRVLNP        ANMFLS
093700920429     C                   MOVEL     BRVLNA        ANMLNA
093800920429     C                   MOVEL     BRVNRS        ANMNRS
093900920429     C                   MOVEL     BRVNSC        ANMSCN
094000920429     C                   MOVEL     BRVNPS        ANMNPS
094100990511     C                   Z-ADD     BRVNFV        ANMNFV
094200950811     C                   MOVEL     BRVFGS        ANMCDU
094300950818     C                   MOVEL     SIMFEL        ANMFLE
094400950818     C                   Z-ADD     WDFV          ANMDAO
094500950818     C                   MOVEL     ANMCDU        WFL1
094600920429     C*
094700920429     C                   EXSR      CRTANM
094800951002    5C                   ENDDO
094900951002    4C                   ENDIF
095000951002    3C                   ENDIF
095100940426     C*
095200000512   2AC                   END
095300000512    2C                   END
095400911016     C*
095500911126     C* STAMPO DISCORDANZA
095600000512     C     LNPPT         IFNE      'S'
095700000512     C     §7CSPT        OREQ      'S'
095800911016     C                   EXSR      STAMPA
095900940330    1C                   END
096000000512    1C                   END
096100911015     C*
096200970602     C                   SETOFF                                       05
096300950810     C* SCRIVO LA SPUNTA MANCANTE IN MODO CHE NON TORNI A DARE ANOMALIA
096400950810     C*  SE E' PRESENTE IN ALTRA CATEGORIA
096500000209     C                   CLEAR                   FISPPWR
096600950810     C                   Z-ADD     BRVLNP        SPPLNP
096700950810     C                   Z-ADD     BRVLNA        SPPLNA
096800950810     C                   Z-ADD     BRVNRS        SPPNRS
096900950810     C                   Z-ADD     BRVNSC        SPPNSC
097000950810     C                   Z-ADD     BRVZNC        SPPZNC
097100000209     C                   WRITE     FISPPWR
097200911015     C                   ENDSR
097300941222     C**************************************************************************
097400941222     C* IMPOSTO CAMPI FISSI
097500941222     C**************************************************************************
097600911015     C     IMPO          BEGSR
097700930526     C*
097800950810     C* FOGLIO ARRIVI
097900941228     C                   Z-ADD     DLRNPG        KNPG                           CATEGORIA
098000941228     C                   Z-ADD     DLRNFV        KNFV                           NR.FOGLIO
098100020917     C                   Z-ADD     DLRFGS        KFGS                           FIL.GESTIONE
098200020917     C     KFVV1         CHAIN(N)  FNFVV01L                           32
098300930526     C*
098400930526     C                   MOVE      FVVDFV        G02INV
098500930526     C                   MOVE      *ZEROS        G02DAT
098600930526     C                   MOVEL     3             G02ERR
098700941222     C                   CALL      'XSRDA8'
098800930526     C                   PARM                    WLBDAT
098900941222     C                   Z-ADD     G02DAT        STADFV
099000950810     C*
099100950811     C                   MOVEL     FVVDFV        WDFV
099200980114     C                   MOVEL     FVVFGS        WFGS
099300930526     C*
099400930526     C*                FILIALE APERTURA
099500941223     C     FVVFGS        CHAIN     AZORG                              32
099600950811     C  N32              MOVEL     ORGDES        DESFGS           15
099700940607     C*
099800040109     c* controllo OGAS di simfel
099900040109     c     simfel        chain     azorg01l
100000040109     C                   MOVEL     ORGDE3        OG143
100100040109     C                   MOVEL     §OGAS         siOGAS
100200980114     C* DETERMINO TERMINAL DI ARRIVO DELLA FIL.GESTIONE
100300971009     C                   CLEAR                   DSLV55
100400980114     C                   MOVEL     'A'           D55TPT
100500971009     C                   Z-ADD     FVVFGS        D55LIN
100600971009     C                   Z-ADD     FVVDFV        D55DRF
100700971009     C                   CALL      'FNLV55R'
100800971009     C                   PARM                    DSLV55
100900980114     C                   Z-ADD     D55TFA        TFASAV
101000980114     C* 23 ON - E' UN SECONDO LIVELLO IN ARRIVO AVENTE UN TER-PART <>
101100040109     C*         DAL TER-PART. DEL TER-ARR  e <> OGAS
101200971009     C     D55TFA        IFNE      FVVFGS
101300980114     C                   CLEAR                   DSLV55
101400980114     C                   MOVEL     'P'           D55TPT
101500980114     C                   Z-ADD     TFASAV        D55LIN
101600980114     C                   Z-ADD     FVVDFV        D55DRF
101700980114     C                   CALL      'FNLV55R'
101800980114     C                   PARM                    DSLV55
101900980114     C     D55TFP        IFNE      SIMFEL
102000040109     C*
102100040109     c* controllo OGAS di simfel
102200040109     c     d55tfp        chain     azorg01l
102300040109     C                   MOVEL     ORGDE3        OG143
102400040109     C                   MOVEL     §OGAS         tpOGAS
102500040109     c                   if        tpogas<>siogas
102600940607     C                   SETON                                        23
102700940607     C                   ENDIF
102800040109     C                   ENDIF
102900980114     C                   ENDIF
103000980114     C* SE 23 OFF VERIFICO SE ESISTE ALMENO UN ECCEZIONE TER-ARR PER
103100980114     C* CUI LA FILIALE GESTIONE RISULTA UN 2° LIV IN ARRIVO AVENTE UN
103200980114     C* TER-PART <> DAL TER-PART DEL TER-ARRIVO
103300980114     C     *IN23         IFEQ      *OFF
103400980114     C     KCAE          SETLL     AZCAE02L
103500980114     C     KCAE          READE     AZCAE02L                               32
103600980114     C     *IN32         DOWEQ     *OFF
103700980114     C     *IN23         ANDEQ     *OFF
103800980114     C     CAETFP        IFGT      *ZEROS
103900980114     C     CAEATB        ANDEQ     *BLANKS
104000980114     C     CAEDDE        ANDLE     FVVDFV
104100980114     C     CAEDSC        ANDGE     FVVDFV
104200980114     C     CAETFE        ANDNE     FVVFGS
104300980114     C                   CLEAR                   DSLV55
104400980114     C                   MOVEL     'P'           D55TPT
104500980114     C                   Z-ADD     CAETFE        D55LIN
104600980114     C                   Z-ADD     FVVDFV        D55DRF
104700980114     C                   CALL      'FNLV55R'
104800980114     C                   PARM                    DSLV55
104900980114     C     D55TFP        IFNE      SIMFEL
105000040109     C*
105100040109     c* controllo OGAS di simfel
105200040109     c     d55tfp        chain     azorg01l
105300040109     C                   MOVEL     ORGDE3        OG143
105400040109     C                   MOVEL     §OGAS         tpOGAS
105500040109     c                   if        tpogas<>siogas
105600980114     C                   SETON                                        23
105700980114     C                   END
105800040109     C                   END
105900980114     C                   END
106000040109     c*
106100980114     C     KCAE          READE     AZCAE02L                               32
106200980114     C                   ENDDO
106300980114     C                   ENDIF
106400940607     C*
106500930526     C* CAMPI FISSI ANOMALIA
106600990511     C                   CLEAR                   FNANM000
106700921022     C                   MOVEL     'I'           ANMFSA
106800911015     C*
106900921124     C                   MOVEL     '-'           ALF1
107000960410     C*
107100911015     C                   SETON                                        OF
107200911109     C*
107300941228     C     DLRFS1        IFEQ      'S'
107400911109     C                   SETON                                        10
107500950810     C                   ENDIF
107600930205     C*
107700930205     C* DECODIFICA   CATEGORIA FOGLIO
107800930205     C                   MOVEL     '7N'          COD
107900950810     C                   MOVEL(P)  DLRNPG        KEY
108000930205     C     KTAB          CHAIN     TABEL                              32
108100930205     C  N32              MOVEL     TBLUNI        DESNPG           12
108200930526     C*
108300930526     C* CARICO TABELLA FILIALI GESTITE £1
108400030114     C                   CLEAR                   TRUL06ds
108500950810     C                   MOVE      '£1'          D06COD
108600950810     C                   MOVEL     'L'           D06TLA
108700950810     C                   MOVEL     SIMFEL        D06KEY
108800030114     C                   MOVEL     TRUL06ds      KPJBU
108900950810     C*
109000950810     C                   CALL      'TRUL06R'
109100950810     C                   PARM                    KPJBA
109200030114     C                   MOVEL     KPJBU         TRUL06ds
109300950810     C                   MOVEA     LIN           L1
109400000512     C** VEDO SE L'ANOMALIA 05 DEVE ESSERE CREATA E/O STMAPATA ANCHE
109500000512     C**  PER LE POSTE
109600021214     C                   Z-ADD     Wanm005       COMCAA
109700000512    1C     COMCAA        IFNE      SAVCAA
109800000512     C                   MOVEL     '7C'          COD
109900000512     C                   MOVEL(P)  COMCAA        KEY
110000000512     C     KTAB          CHAIN     TABEL                              39
110100000512     C  N39              MOVEL     TBLUNI        DS7C
110200000512     C   39              CLEAR                   DS7C
110300000512     C                   MOVEL     COMCAA        SAVCAA            3 0
110400000512    1C                   ENDIF
110500000512     C*
110600000512     C                   MOVEL     §7CCPT        W05CPT
110700000512     C                   MOVEL     §7CSPT        W05SPT
110800030114      *
110900030114      * Determino AS del p.o. gestione
111000030114     C     dlrfgs        CHAIN     AZORG01L                           32
111100030114     C                   MOVEL     ORGDE3        OG143
111200030114     C                   MOVEL     §OGAS         MyOGAS
111300030114     c*
111400911015     C                   ENDSR
111500950811     C**************************************************************************
111600950811     C* CHAIN PER VEDERE SE ESISTE LA BOLLA IN ARRIVO O LA BOLLA TRANS
111700950811     C**************************************************************************
111800040109     C     CHAFIL        BEGSR
111900950811     C                   SETOFF                                       333638
112000040531     c                   clear                   dar5gen
112100980807     C* USO LNP BOLLA PER PRENDERE TERMINAL DI ARRIVO DELLA SPUNTA
112200980401     C*  CHE STO LEGGENDO
112300980414     C* NEL CASO DI COLLO PARTITO NON ARRIVATO USO COME LNPB SEMPRE
112400980414     C*  LA LINEA DI PARTENZA FOGLIO VIAGGIO DELLA SPUNTA PARTENZA
112500980414     C* NEL CASO DI COLLO ARRIVATO NON PARTITO INVECE CERCO LA LNP
112600980414     C*  ORIGINALE DELLA BOLLA
112700021122     C*
112800980401     C* CHAIN FVA PER PRENDERE LA DATA DI PARTENZA
112900980807     C*  SOLO SE VENGO DALLA ROUTINE DI MAJORE
113000980807     C     WTIPO         IFEQ      'MA'
113100000127     C                   Z-ADD     SPPNFV        WNFV
113200980401     C     KSPP3         CHAIN     FNFVA01L                           34
113300980807     C* PER MINORE HO GIA' LA DATA DI FVV
113400980807     C                   ELSE
113500980807     C                   SETOFF                                       34
113600980807     C                   ENDIF
113700980807     C**
113800980807    1C     *IN34         IFEQ      *OFF
113900980401     C                   CLEAR                   DSLV55
114000980401     C                   MOVEL     'A'           D55TPT
114100980401     C                   Z-ADD     KLNA          D55LIN
114200980401     C                   Z-ADD     LNPB          D55LNP
114300980807     C     WTIPO         IFEQ      'MA'
114400980401     C                   Z-ADD     FVADFV        D55DRF
114500980807     C                   ELSE
114600980807     C                   Z-ADD     FVVDFV        D55DRF
114700980807     C                   ENDIF
114800980401     C                   CALL      'FNLV55R'
114900980401     C                   PARM                    DSLV55
115000101202     c*
115100980401    2C     D55TFA        IFEQ      DLRFGS
115200980414     C     KLNA          OREQ      DLRFGS
115300040531     c                   seton                                        33
115400950811     C     KART          CHAIN     FNART27L                           36
115500980401     C**
115600980401   X2C                   ELSE
115700040531     c                   setoff                                       33
115800021122     C     KBTT          CHAIN     FNBTT37L                           36
115900980401    2C                   ENDIF
116000980401    1C                   ENDIF
116100040531     c
116200040531     c* chain comunque su FNBLT per avere num.spedizione anche dei disguidi
116300040531     C     KART          CHAIN     FNblt27L
116400040531     c                   if        %found(fnblt27l)
116500040531     c                   eval      bolaas=bltaas
116600040531     c                   eval      bollnp=bltlnp
116700040531     c                   eval      bolnrs=bltnrs
116800040531     c                   eval      bolnsp=bltnsp
116900040531     c* verifico se si tratta di collo di valore per memorizzarlo su anomali
117000040531     c     kar5          chain     fiar501l
117100040531     c                   if        %found(fiar501l)
117200040531     c                   movel     ar5uni        dar5gen
117300040531     c                   endif
117400040531     c                   else
117500040531     c                   clear                   bolaas
117600040531     c                   clear                   bollnp
117700040531     c                   clear                   bolnrs
117800040531     c                   clear                   bolnsp
117900040531     c                   endif
118000950811     C                   ENDSR
118100941222     C**************************************************************************
118200941222     C*    AGGIORNO FOGLI VIAGGIO
118300941222     C**************************************************************************
118400911015     C     UPDFV         BEGSR
118500941222     C*
118600941228     C                   Z-ADD     DLRNPG        KNPG                           CATEGORIA
118700941228     C                   Z-ADD     DLRNFV        KNFV                           NR.FOGLIO
118800020917     C                   Z-ADD     DLRFGS        KFGS                           FIL.GESTIONE
118900020917     C     KFVV1         CHAIN     FNFVV01L                           32
119000911127     C  N32              MOVEL     'S'           FVVFCF
119100941222     C  N32              EXCEPT    UPDFVV
119200921124     C*
119300921124     C* SOLO SE INVIO IDD CREO IL RECORD PER LA PARTENZA
119400911015     C*
119500921124     C                   MOVEL     '7A'          COD
119600921124     C                   MOVEL     '1       '    KEY
119700921124     C     KTAB          CHAIN     TABEL                              32
119800921124     C  N32              MOVEL     TBLUNI        DS7A
119900921124     C   32              MOVEL     ' '           §7AIDD
120000921124     C*
120100950811    1C     §7AIDD        IFEQ      'S'
120200950817     C*
120300950811     C                   MOVEL     *ZEROS        ANMNRS
120400950811     C                   MOVEL     *ZEROS        ANMSCN
120500021214     C                   Z-ADD     Wanm090       COMCAA
120600950811     C                   MOVEL     *ZEROS        ANMNPS
120700950811     C                   MOVEL     *ZEROS        ANMNSP
120800950811     C                   MOVEL     *ZEROS        ANMAAS
120900950811     C                   MOVEL     *ZEROS        ANMFLS
121000950818     C                   Z-ADD     WDFV          ANMDAO
121100000515     C*
121200000515     C                   Z-ADD     COMCAA        ANMCAA
121300000515    1C     COMCAA        IFNE      SAVCAA
121400000515     C                   MOVEL     '7C'          COD
121500000515     C                   MOVEL(P)  ANMCAA        KEY
121600000515     C     KTAB          CHAIN     TABEL                              39
121700000515     C  N39              MOVEL     TBLUNI        DS7C
121800000515     C   39              CLEAR                   DS7C
121900000515     C                   MOVEL     COMCAA        SAVCAA            3 0
122000000515    1C                   ENDIF
122100000515     C*
122200950811     C* COME FILIALE GESTIONE METTO NON SIMFEL MA LA FILIALE CHE
122300950818     C*  PROPRIO HA FATTO L'IDA
122400950811     C                   Z-ADD     DLRFGS        ANMFLE
122500950811     C* CREO RECORD ANOMALIA --> "TUTTO OK" PER LA  FIL PARTENZA
122600950811     C*
122700950811     C* RILEGGO TUTTI I FV, PERCHE' L'OK IDA LO DEVO DARE ANCHE
122800950817     C*  PER QUELLI TRANSITO
122900950817     C*  ESCLUDO SOLO I FOGLI ARRIVATI ILLUMINATI PER I 2 LIVELLO
123000001122     C*  IN ARRIVO  (ELABORO SOLO FOGLIO LOCALE)
123100950811     C     KFVA1         SETLL     FNFVA000
123200950811     C     KFVA1         READE     FNFVA000                               32
123300950811     C*
123400950811    2C     *IN32         DOWEQ     *OFF
123500950817     C                   CLEAR                   WFLAG
123600950817     C*
123700911016     C                   MOVEL     FVALNP        ANMLNP
123800960412     C                   MOVEL     FVALNP        ANMCDU
123900990511     C                   Z-ADD     FVANFV        ANMNFV
124000921124     C                   MOVEL     FVALAI        ANMLNA
124100950816     C                   MOVEL     FVALNP        WFL1
124200950816     C                   CLEAR                   WFL2
124300000503     C*
124400000503     C     KFWA          CHAIN     FNFWA01L                           39
124500000503     C     *IN39         IFEQ      *ON
124600000503     C     FWAATB        ORNE      ' '
124700000503     C                   CLEAR                   FWAFF3
124800000503     C                   CLEAR                   FWAFF4
124900000503     C                   CLEAR                   FWAFL3
125000000503     C                   CLEAR                   FWAFL4
125100000515     C                   CLEAR                   FWAFGS
125200000503     C                   ENDIF
125300000503     C*
125400950817     C*
125500001122     C* PER I 2 LIVELLO IN ARRIVO ELABORO SOLO FOGLIO LOCALE
125600950817    3C     FVAATR        IFEQ      'T'
125700950817     C     *IN23         ANDEQ     *ON
125800001122     C     FVATTR        ANDNE     'L'
125900950817     C                   EXSR      CTRFVA
126000950817    3C                   ENDIF
126100950817     C*
126200950817    3C     WFLAG         IFEQ      ' '
126300000515     C*
126400000515    4C     §7CCPT        IFNE      'S'
126500000515     C                   CLEAR                   SAVLNP
126600000515     C                   CLEAR                   OG143
126700000515     C                   CLEAR                   FGSPT
126800000515    5C     FWAFGS        IFGT      0
126900000515     C     FWAFGS        CHAIN     AZORG01L                           32
127000000515     C  N32              MOVEL     ORGDE3        OG143
127100000515     C   32              CLEAR                   OG143
127200021122     c                   if        §ogntw='PPT'
127300021122     c                   movel     'S'           FGSPT             1
127400021213     c                   else
127500021213     c                   clear                   FGSPT
127600021122     c                   endif
127700101202     c*
127800000515    5C                   ENDIF
127900000515     C*
128000000515     C     FVALNP        CHAIN     AZORG01L                           32
128100000515     C  N32              MOVEL     ORGDE3        OG143
128200000515     C   32              CLEAR                   OG143
128300000515    4C                   ENDIF
128400000515     C**
128500021122     c     §OGNTW        ifne      'PPT'
128600021122    4C**   §OGPT         IFNE      'S'
128700000515    4C     FGSPT         ANDNE     'S'
128800000515     C     §7CCPT        OREQ      'S'
128900911016     C                   EXSR      CRTANM
129000000515    4C                   ENDIF
129100000515    3C                   ENDIF
129200911015     C*
129300950811     C     KFVA1         READE     FNFVA000                               32
129400950811    2C                   ENDDO
129500950811    1C                   ENDIF
129600961021     C*
129700961021     C                   CLEAR                   ANMLNP
129800961021     C                   CLEAR                   ANMNFV
129900961021     C                   Z-ADD     ANMFLE        ANMLNA
130000961021     C                   CLEAR                   ANMCDU
130100961021     C                   CLEAR                   WFL1
130200961021     C                   CLEAR                   WFL2
130300961021     C                   EXSR      CRTANM
130400911015     C*
130500911015     C                   ENDSR
130600950817     C**************************************************************************
130700951002     C* CONTROLLO I FV PER VEDERE SE CREARE L'ANOMALIA OK IDA
130800950817     C**************************************************************************
130900950817     C     CTRFVA        BEGSR
131000950817     C* VERIFICO PER LA FILIALE DI GESTIONE FOGLIO QUALE E' LO SCARICO
131100950817     C* 1) SE = AL TERMINAL DI ARRIVO ESCLUDO
131200950817     C* 2) SE TRANSITO, VERIFICO IL FOGLIO DELLA FILIALE DI TRANSITO
131300950817     C*    DOVE SCARICA
131400950817     C                   MOVEL     FVALAI        SAVLAI
131500950817     C                   EXSR      CARFVA
131600980203     C*
131700980203     C                   MOVE      '1'           WTROV
131800950817     C*
131900960502    1C     WFLAG         DOWEQ     ' '
132000950817     C     FVAATR        ANDEQ     'T'
132100950817     C     WFLP          ANDGT     0
132200980203     C     WTROV         ANDEQ     '1'
132300980203     C                   MOVE      *BLANKS       WTROV
132400950817     C*
132500950817     C* LEGGO FNFVA01L PER VEDERE LA FILIALE DI SCARICO:
132600950817     C     WFLP          SETGT     FNFVA01L
132700950817     C     WFLP          READPE    FNFVA01L                               31
132800950817     C*
132900960502    2C     *IN31         DOWEQ     *OFF
133000000503     C*
133100000503     C     KFWA          CHAIN     FNFWA01L                           39
133200000503     C     *IN39         IFEQ      *ON
133300000503     C     FWAATB        ORNE      ' '
133400000503     C                   CLEAR                   FWAFF3
133500000503     C                   CLEAR                   FWAFF4
133600000503     C                   CLEAR                   FWAFL3
133700000503     C                   CLEAR                   FWAFL4
133800000503     C                   ENDIF
133900980203     C* MEMORIZZO CHE HO TROVATO ALMENO UN RECORD CON QUESTA CHIAVE
134000980203     C                   MOVE      '1'           WTROV             1
134100950817     C* L'INVIO DEVE CORRISPONDERE A QUELLO CHE STO ELABORANDO
134200960502    3C     FVALAI        IFEQ      SAVLAI
134300960502     C* SE FOGLIO DIRETTO NON HO BISOGNO DI CONTROLLARE
134400960502    4C     FVAATR        IFEQ      ' '
134500950817     C                   SETON                                        31
134600950817     C                   ELSE
134700950817     C                   EXSR      CARFVA
134800960502    4C                   ENDIF
134900960502     C*
135000960502   X3C                   ELSE
135100960502     C* SE L'INVIO NON CORRISPONDE MA E' UN DIRETTO VERIFICO SE
135200960502     C*  LA LINEA SCARICA ALLA FIL DI INVIO
135300960502    4C     FVAATR        IFEQ      ' '
135400960502     C                   Z-ADD     FVALNA        LNA2
135500960502     C                   MOVEL     SAVLAI        ALF3
135600960502     C                   Z-ADD     1             X
135700960502     C     ALF3          LOOKUP    FFV(X)                                 33
135800960502     C   33              MOVEL     FLP(X)        W0030             3 0
135900960502    5C   33W0030         IFEQ      FVALAI
136000960502     C                   SETON                                        31
136100960502     C                   MOVEL     '1'           WFLAG
136200960502    5C                   ENDIF
136300960502    4C                   ENDIF
136400960502    3C                   ENDIF
136500950817     C*
136600950817     C  N31WFLP          READPE    FNFVA01L                               31
136700960502    2C                   ENDDO
136800960502    1C                   ENDDO
136900950817     C*
137000950817     C                   ENDSR
137100950817     C**************************************************************************
137200950817     C* CARICO LE LINEE DEL FV
137300950817     C**************************************************************************
137400950817     C     CARFVA        BEGSR
137500950817     C                   Z-ADD     FVALNA        LNA2
137600950817     C                   CLEAR                   WFLP
137700950817     C*
137800950817     C                   MOVEL     SAVLAI        ALF3
137900950817     C                   Z-ADD     1             X
138000950817     C     ALF3          LOOKUP    FFV(X)                                 33
138100950817     C* SE NON LO TROVO, ELABORO
138200950817     C     *IN33         IFEQ      *ON
138300950817     C                   MOVEL     FLP(X)        WFLP
138400980114     C* DETERMINO IL TERMINAL DI ARRIVO DELLA FILIALE GESTIONE
138500980114     C                   CLEAR                   DSLV55
138600980114     C                   MOVEL     'A'           D55TPT
138700980114     C                   Z-ADD     WFGS          D55LIN
138800980114     C                   Z-ADD     WDFV          D55DRF
138900980114     C                   Z-ADD     FVALNP        D55LNP
139000980114     C                   CALL      'FNLV55R'
139100980114     C                   PARM                    DSLV55
139200980114     C     WFLP          IFEQ      D55TFA
139300950817     C                   MOVEL     '1'           WFLAG
139400950817     C                   SETON                                        31
139500950817     C                   ENDIF
139600950817     C                   ENDIF
139700950817     C                   ENDSR
139800941222     C**************************************************************************
139900941222     C* CREO ANOMALIA
140000941222     C**************************************************************************
140100911016     C     CRTANM        BEGSR
140200941222     C*
140300040531     c* imposto data e ora creazione anomalia, sempre
140400050707     C                   TIME                    Wtime            14 0
140500050707     c                   movel     wtime         wora4             4 0
140600040531     c                   move      wora4         anmflo
140700050707     c
140800050707     C                   CLEAR                   WLBDA8
140900050707     C                   MOVE      Wtime         G08DAT
141000050707     C                   CALL      'XSRDA8'
141100050707     C                   PARM                    WLBDA8
141200050707     C                   MOVE      G08INV        wdata             6 0
141300050707     C                   MOVEl     wdata         anmflo
141400050707     c
141500040531     c* non imposto la zona
141600040531     c                   eval      anmfl4=999
141700040531     c
141800930526     C                   Z-ADD     COMCAA        ANMCAA
141900960515    1C     COMCAA        IFNE      SAVCAA
142000911016     C                   MOVEL     '7C'          COD
142100950811     C                   MOVEL(P)  ANMCAA        KEY
142200940330     C     KTAB          CHAIN     TABEL                              39
142300940330     C  N39              MOVEL     TBLUNI        DS7C
142400960515     C   39              CLEAR                   DS7C
142500930526     C                   MOVEL     COMCAA        SAVCAA            3 0
142600960515    1C                   ENDIF
142700911016     C*
142800970602     C* PER L'ANOMALIA 160 IMPOSTO FASE E FOGLIO DELLA SPUNTA CHE ME
142900970602     C*  LA CREA. PER LE ALTRE ANOMALIA TENGO LA FASE IDA
143000021214     C     COMCAA        IFNE      wanm160
143100960514     C                   MOVEL     'I'           ANMFSA
143200970602     C                   ELSE
143300970602     C                   MOVEL     A160FS        ANMFSA
143400970602     C                   ENDIF
143500911016     C                   MOVEL     §7CTAN        ANMTAN
143600911016     C                   MOVEL     §7CSAN        ANMSAN
143700911016     C                   MOVEL     §7CAIE        ANMAIE
143800911016     C                   Z-ADD     §7CLID        ANMLID
143900921022     C                   Z-ADD     0             ANMDCH
144000921022     C                   MOVEL     ' '           ANMFSC
144100960514     C                   MOVEL     '  '          ANMCCH
144200960514     C                   CLEAR                   ANMATB
144300911018     C* CHIUSURA AUTOMATICA
144400960515    1C     §7CCHA        IFEQ      'S'
144500950811     C                   Z-ADD     WDFV          ANMDCH
144600921022     C                   MOVEL     'I'           ANMFSC
144700960515    1C                   END
144800911018     C                   Z-ADD     0             ANMFL1
144900911109     C                   Z-ADD     0             ANMFL2
145000911109     C* ESTERNA
145100960515    1C     §7CAIE        IFEQ      'E'
145200911109     C* VEDO A CHI TRASMETTERE
145300950816     C                   MOVEL     WFL1          ANMFL1
145400950816     C                   MOVEL     WFL2          ANMFL2
145500921022     C* SE UGUALI CANCELLO IL 2°
145600960515    2C     ANMFL1        IFEQ      ANMFL2
145700911109     C                   Z-ADD     0             ANMFL2
145800960515   X2C                   ELSE
145900971128     C                   CLEAR                   DSLV55
146000971128     C                   MOVEL     'P'           D55TPT
146100971128     C                   MOVE      ANMFL1        D55LIN
146200971128     C                   MOVE      DATEU8        D55DRF
146300971128     C                   CALL      'FNLV55R'
146400971128     C                   PARM                    DSLV55
146500971128     C     D55ERR        IFNE      *BLANKS
146600971128     C                   MOVE      ANMFL1        WTFP1
146700971128     C                   ELSE
146800971128     C                   MOVE      D55TFP        WTFP1
146900971128     C                   END
147000971128     C                   CLEAR                   DSLV55
147100971128     C                   MOVEL     'P'           D55TPT
147200971128     C                   MOVE      ANMFL2        D55LIN
147300971128     C                   MOVE      DATEU8        D55DRF
147400971128     C                   CALL      'FNLV55R'
147500971128     C                   PARM                    DSLV55
147600971128     C     D55ERR        IFNE      *BLANKS
147700971128     C                   MOVE      ANMFL2        WTFP2
147800971128     C                   ELSE
147900971128     C                   MOVE      D55TFP        WTFP2
148000971128     C                   END
148100971128    3C     WTFP1         IFEQ      WTFP2
148200960410     C                   CLEAR                   ANMFL2
148300960515    3C                   END
148400960515    2C                   ENDIF
148500911109     C*
148600960515    1C                   ENDIF
148700911016     C*
148800960514     C* PER ANOMALIA 005 VEDO, SE CE N'E' UN'ALTRA, COSA FARE
148900960515    1C     ANMCAA        IFEQ      05
149000960514     C     WANOM         ANDEQ     'S'
149100960514     C**
149200960514     C                   Z-ADD     0             WC                1 0
149300960806     C                   CLEAR                   SKI
149400960515     C** CONTO A QUANTE FILIALI DEVO TRASMETTERE
149500960515    2C     WSAFL1        IFGT      0
149600960514     C                   ADD       1             WC
149700960514     C                   MOVEL     WSAFL1        SKI(WC)
149800960515    2C                   ENDIF
149900960515    2C     WSAFL2        IFGT      0
150000960514     C                   ADD       1             WC
150100960514     C                   MOVEL     WSAFL2        SKI(WC)
150200960515    2C                   ENDIF
150300960514     C*
150400960515    2C     ANMFL1        IFGT      0
150500960514     C     ANMFL1        LOOKUP    SKI                                    39
150600960514     C  N39              ADD       1             WC
150700960516     C  N39              MOVEL     ANMFL1        SKI(WC)
150800960515    2C                   ENDIF
150900960515    2C     ANMFL2        IFGT      0
151000960514     C     ANMFL2        LOOKUP    SKI                                    39
151100960514     C  N39              ADD       1             WC
151200960516     C  N39              MOVEL     ANMFL2        SKI(WC)
151300960515    2C                   ENDIF
151400960514     C**
151500960514     C* SE WC <= 2 ALLORA TENGO SOLO L'ULTIMA ANOMALIA
151600960517     C*  SE SI TRATTA DI UNA MIA FILIALE - ALTRIMENTI DEVO
151700960517     C*  TRASMETTERE L'ANNULLAMENTO DELLA ANOMALIA E LA SCRITTURA DELLA
151800960517     C*  NUOVA
151900960515    2C     WC            IFLE      2
152000960517     C     *IN36         ANDEQ     *OFF
152100960517    2C     WC            ORLE      2
152200960517     C     *IN38         ANDEQ     *OFF
152300960515     C*
152400960515    3C     ANMFL2        IFEQ      0
152500960515    4C     WSAFL1        IFNE      ANMFL1
152600960515     C                   Z-ADD     WSAFL1        ANMFL2
152700960515     C                   ELSE
152800960515    5C     WSAFL2        IFNE      ANMFL1
152900960515     C                   Z-ADD     WSAFL2        ANMFL2
153000960515    5C                   ENDIF
153100960515    4C                   ENDIF
153200960515    3C                   ENDIF
153300960515     C*
153400990511     C                   UPDATE    FNANMF
153500960515   X2C                   ELSE
153600970529     C* ENTRO QUI SE L'ANOMLIA E' PER UN MIO 2 LIVELLO IN ARRIVO SU
153700970529     C*  ALTRO AS O SE HO PIU' DI 2 P.O. DI TRASMISSIONE
153800960514     C*
153900960514     C* SCRIVO LA SECONDA ANOMALIA
154000990618     C     ANMFL1        IFGT      0
154100990618     C                   MOVEL     WSAFT1        ANMFT1
154200990618     C                   ENDIF
154300990618     C     ANMFL2        IFGT      0
154400990618     C                   MOVEL     WSAFT1        ANMFT2
154500990618     C                   ENDIF
154600990511     C                   WRITE     FNANM000
154700960515     C**
154800960515     C* SALVO LE FILIALI TRASMISSIONI CHE HO SCRITTO
154900960515     C                   MOVEL     ANMFL1        WSAFL1
155000960515     C                   MOVEL     ANMFL2        WSAFL2
155100960514     C*
155200960515     C* TENGO LA PRIMA MA CANCELLANDO LE FILIALI TRASMISSIONI = ALLA 2
155300970529     C* IMPOSTO SOGGETTO ANOMALIA = E (PER NON STAMPARE 2 VOLTE L'ANOM
155400970529     C*    NELL'IDD O PER NON FARLA VEDERE 2 VOLTE IN GESTIONE ANOMAL)
155500960514     C* REIMPOSTO IL LIVELLO IDD DALL'INIZIO
155600960514     C* RICHAINO
155700990511     C     WSANR2        CHAIN     FNANM00F                           39
155800960517     C                   MOVEL     §7CAIE        ANMAIE
155900960517     C                   CLEAR                   ANMDCH
156000960517     C                   CLEAR                   ANMCCH
156100960517     C                   CLEAR                   ANMFSC
156200960517     C*
156300960517    3C     WC            IFGT      2
156400960514     C                   MOVEL     'E'           ANMSAN
156500960514     C                   MOVEL     §7CLID        ANMLID
156600960515     C*
156700960517    4C     ANMFL1        IFEQ      WSAFL1
156800960515     C     ANMFL1        OREQ      WSAFL2
156900960515     C                   CLEAR                   ANMFL1
157000960517    4C                   ENDIF
157100960517    4C     ANMFL2        IFEQ      WSAFL1
157200960515     C     ANMFL2        OREQ      WSAFL2
157300960515     C                   CLEAR                   ANMFL2
157400960517    4C                   ENDIF
157500960515     C* SE ENTRAMBE = 0 (IMPOSSIBILE!!!) CANCELLO L'ANOMALIA
157600960517    4C     ANMFL1        IFEQ      0
157700960515     C     ANMFL2        ANDEQ     0
157800990511     C                   DELETE    FNANMF
157900960517   X4C                   ELSE
158000960515     C* SE E' LA 1 FILIALE CANCELLATA IMPOSTO LA 2 NELLA PRIMA
158100960517    5C     ANMFL1        IFEQ      0
158200960515     C                   Z-ADD     ANMFL2        ANMFL1
158300960515     C                   CLEAR                   ANMFL2
158400960517    5C                   ENDIF
158500960517     C*
158600990511     C                   UPDATE    FNANMF
158700960517    4C                   ENDIF
158800960517     C**
158900960517   X3C                   ELSE
159000960517     C**
159100960517     C* ANNULLO SOLAMENTE L'ANOMALIA
159200960517     C                   MOVEL     'A'           ANMATB
159300990511     C                   UPDATE    FNANMF
159400960517    3C                   ENDIF
159500960515     C*
159600960515    2C                   ENDIF
159700960514     C**
159800960515   X1C                   ELSE
159900960618     C                   CLEAR                   ANMFT1
160000960618     C                   CLEAR                   ANMFT2
160100990511     C                   WRITE     FNANM000
160200960515    1C                   ENDIF
160300941222     C*
160400911016     C                   ENDSR
160500941222     C**************************************************************************
160600941222     C* STAMPO DISCORDANZA
160700941222     C**************************************************************************
160800900621     C     STAMPA        BEGSR
160900941222     C*
161000911015     C* STAMPA TESTATA
161100911015     C   OF              DO
161200921124     C* PRIMA PARTE TESTATA
161300921124     C                   EXCEPT    TES
161400911015     C*
161500921124     C                   Z-ADD     1             I
161600921124     C                   Z-ADD     B             Y                 3 0
161700921124     C                   MOVEA     *BLANKS       STF
161800921124     C                   Z-ADD     0             SAVLAI
161900911015     C*
162000921124     C     Y             DOWLE     208
162100921124     C                   MOVEL     FVA(Y)        FVAPAR
162200921124     C*
162300921124     C     FVALAI        IFNE      SAVLAI
162400921124     C     I             IFGT      1
162500921124     C                   EXCEPT    TES2
162600921124     C                   MOVEA     *BLANKS       STF
162700921124     C                   Z-ADD     1             I
162800921124     C                   END
162900921124     C* SALVO NUOVA LINEA
163000950817     C                   MOVEL     FVALAI        SAVLAI
163100921124     C* DECODIFICA  LINEA
163200940330     C     SAVLAI        CHAIN     AZORG                              39
163300940330     C  N39              MOVEL     ORGDES        DESLAI           20
163400940330     C   39              MOVEL     *BLANKS       DESLAI
163500921124     C*
163600921124     C* STAMPO RIGA DI TESTA DELLA LINEA DI INVIO
163700921124     C                   EXCEPT    TES1
163800921124     C                   END
163900921124     C*
164000921124     C     I             IFEQ      131
164100921124     C                   EXCEPT    TES2
164200921124     C                   MOVEA     *BLANKS       STF
164300921124     C                   Z-ADD     1             I
164400921124     C                   END
164500921124     C*
164600921124     C                   MOVEL     FVALNP        ALF3
164700921124     C                   MOVEL     FVANFV        ALF5
164800921124     C                   MOVEA     FVAALF        STF(I)
164900921124     C                   ADD       10            I
165000921124     C**
165100921124     C                   ADD       1             Y
165200921124     C                   END
165300921124     C*
165400921124     C     I             IFGT      1
165500921124     C                   EXCEPT    TES2
165600921124     C                   END
165700911015     C*
165800911015     C* STAMPO TERZA PARTE TESTATA
165900911015     C                   EXCEPT    TES3
166000911015     C*
166100921124     C                   SETOFF                                       OF
166200911015     C                   END
166300911015     C*
166400921126     C* LINEE
166500921126     C     STALNA        IFNE      COMLNA
166600921126     C* LINEA ARRIVO
166700940330     C     STALNA        CHAIN     AZORG                              39
166800940330     C  N39              MOVEL     ORGDES        DESLNA
166900940330     C   39              MOVEL     *BLANKS       DESLNA           10
167000921126     C                   MOVEL     STALNA        COMLNA            3 0
167100921126     C                   END
167200911015     C* LINEA PARTENZA
167300921126     C     STALNP        IFNE      SAVLNP
167400940330     C     STALNP        CHAIN     AZORG                              39
167500940330     C  N39              MOVEL     ORGDES        DESLNP
167600940330     C   39              MOVEL     *BLANKS       DESLNP           10
167700921126     C                   MOVEL     STALNP        SAVLNP            3 0
167800000512     C  N39              MOVEL     ORGDE3        OG143
167900000512     C   39              CLEAR                   OG143
168000021122     c                   if        §ogntw='PPT'
168100021122     c                   movel     'S'           lnppt
168200021213     c                   else
168300021213     c                   clear                   LNPPT
168400021122     c                   endif
168500101202     c*
168600921126     C                   END
168700910522     C*
168800900622     C* STAMPO DETTAGLIO
168900040322     C                   EXCEPT    DET
169000900622     C*
169100900622     C                   ENDSR
169200950810     C*****************************************************************
169300950810     C* ESCLUDO LOCALI
169400950810     C*****************************************************************
169500950810     C     REDBRV        BEGSR
169600980807     C                   CLEAR                   WLNP
169700950810     C                   SETON                                        32
169800960212    1C     *IN32         DOWEQ     *ON
169900070110     C     KFVV2         READE     FNBRV01L                               31
170000960212     C   31              SETOFF                                       32
170100960606   1AC     *IN31         IFEQ      *OFF
170200960612     C*
170300020930     C     BRVLNP        LOOKUP    L1                                     32
170400020910     C*
170500020910     C* SE COLLO CON SERIE SE TROVO USO LNP BOLLA
170600960612    2C     BRVNRS        IFGT      *ZEROS
170700960612     C*
170800020910     C                   MOVEL     '1'           WNOTR             1
170900020910     C* CERCO PRIMA IN BOLLE ARRIVI
171000020910     C     KSPP2         CHAIN     FNART27L                           35
171100020910     C  N35              CLEAR                   WNOTR
171200020910     C  N35ARTLNP        IFNE      BRVLNP
171300020910     C                   MOVE      ARTLNP        WLNP
171400020910     C                   END
171500960620     C* SE BOLLA NON TROVATA POTREBBE ESSERE EFFETTIVAMENTE MIA
171600960621     C     WNOTR         IFEQ      '1'
171700020919     C     KSPP2         CHAIN     FNBLT27L                           35
171800020919     C  N35              CLEAR                   WNOTR
171900020919     C  N35              MOVE      BLTLNP        WLNP
172000960620     C                   END
172100020910     C* ALTRIMENTI CERCO IN BOLLE TRANSITI
172200020910     C     WNOTR         IFEQ      '1'
172300020910     C     KSPP2         CHAIN     FNBTT27L                           35
172400020910     C  N35              CLEAR                   WNOTR
172500020910     C  N35              MOVE      BTTLNP        WLNP
172600020910     C                   ENDIF
172700960621     C* SE BOLLA INESISTENTE CERCO SPUNTA PARTENZA
172800960621    5C     WNOTR         IFEQ      '1'
172900960621     C                   Z-ADD     1             KNPG
173000960621     C                   Z-ADD     BRVLNP        KLNP
173100960621     C                   Z-ADD     BRVLNA        KLNA
173200960621     C                   Z-ADD     BRVNRS        KNRS
173300960621     C                   Z-ADD     BRVNSC        KNSC
173400070110     C     KBRV4         CHAIN     FNBRV09L                           35
173500960621    6C     *IN35         DOWEQ     *OFF
173600960621     C     SPPATR        IFEQ      *BLANKS
173700960621     C     SPPLPV        IFNE      BRVLNP
173800960621     C                   MOVE      SPPLPV        WLNP
173900960703     C                   ELSE
174000960703     C                   CLEAR                   WLNP
174100960621     C                   END
174200960621    7C     SPPLPV        IFNE      SIMFEL
174300960621     C                   SETON                                        35
174400960621     C                   END
174500960621     C                   END
174600070110     C  N35KBRV4         READE     FNBRV09L                               35
174700960621    6C                   ENDDO
174800960621    5C                   END
174900960621     C*
175000960612     C     WLNP          IFGT      *ZEROS
175100960612     C     WLNP          LOOKUP    L1                                     32
175200960612     C                   END
175300960612    2C                   END
175400960606     C*
175500960212     C*
175600960606   1AC                   ENDIF
175700960212    1C                   ENDDO
175800960212     C*
175900950810     C                   ENDSR
176000950810     C**************************************************************************
176100950810     C* LETTURA DEI FOGLI CON CATEGORIA PASSATA E DATA = A DATA F.ARRVI
176200950810     C**************************************************************************
176300950810     C     REDFVV        BEGSR
176400950811     C     KFVV          SETLL     FNFVV002
176500950811     C     KFVV          READE     FNFVV002                               30
176600950810     C*
176700950810     C     *IN30         DOWEQ     *OFF
176800950811     C                   MOVEL     FVVNPG        KNPG3
176900000127     C                   Z-ADD     FVVNFV        KNFV3
177000950811     C                   MOVEL     FVVFGS        KFGS3
177100970602     C* RECUPERO LA FASE DEL FOGLIO
177200970602     C                   CLEAR                   DSUL11
177300970602     C                   MOVEL     FVVNPG        D11NPG
177400970602     C                   MOVEL     FVVSPG        D11SPG
177500970616     C                   CALL      'TRUL11R'
177600970602     C                   PARM                    DSUL11
177700970602     C                   MOVEL     D11FAS        A160FS            1
177800950810     C*
177900070110     C     KFVV2         SETLL     FNBRV000
178000950810     C                   EXSR      REDBRV
178100950810     C     *IN31         DOWEQ     *OFF
178200950810     C*
178300950810     C* VEDO SE ESISTE LA SPUNTA PARTENZA
178400000209     C     KSPP2         SETLL     FISPPWR                                34
178500950810     C* 34 OFF - NON TROVATA
178600950811     C     *IN34         IFEQ      *OFF
178700950811     C*
178800950811     C                   Z-ADD     BRVLNP        KLNP
178900950811     C                   Z-ADD     BRVLNA        KLNA
179000950811     C                   Z-ADD     BRVNRS        KNRS
179100950811     C                   Z-ADD     BRVNSC        KNSC
179200980414     C                   CLEAR                   LNPB
179300980807     C     WLNP          IFGT      0
179400980807     C                   Z-ADD     WLNP          LNPB
179500980807     C                   ELSE
179600980807     C                   Z-ADD     BRVLNP        LNPB
179700980807     C                   ENDIF
179800040611     C                   MOVEL     'MI'          WTIPO
179900950811     C                   EXSR      CHAFIL
180000980401     C** TERMINAL CON BOLLA E DATA ARRIVO = A DATA ELABORAZIONE
180100040531     C     *IN33         ifEQ      *ON
180200980401     C     *IN36         ANDEQ     *OFF
180300950811     C     ARTDAM        ANDEQ     WDFV
180400980401     C** TERMINAL SENZA BOLLA IN ARRIVO
180500980401     C     *IN33         OREQ      *ON
180600980401     C     *IN36         ANDEQ     *ON
180700980401     C** BOLLA TRANSITO CON DATA ENTRATA = A DATA ELABORAIZONE
180800950811     C     *IN33         OREQ      *OFF
180900980401     C     *IN36         ANDEQ     *OFF
181000950811     C     BTTDET        ANDEQ     WDFV
181100980401     C* SONO ESCLUSI I COLLI IN DISGUIDO
181200950811     C                   EXSR      MINORE
181300950811     C                   ENDIF
181400950811     C                   ENDIF
181500950810     C*
181600950810     C                   EXSR      REDBRV
181700950810     C                   ENDDO
181800950810     C*
181900950811     C     KFVV          READE     FNFVV002                               30
182000950810     C                   ENDDO
182100950810     C*
182200950810     C                   ENDSR
182300960410     C**************************************************************************
182400960410     C* CREAZIONE SPUNTA ARRIVI PISTOLA 89
182500960410     C**************************************************************************
182600960410     C     CRTSPU        BEGSR
182700070110     C                   CLEAR                   FNBRV000
182800960410     C                   MOVE      DLRNPG        BRVNPG
182900000127     C                   Z-ADD     DLRNFV        BRVNFV
183000960410     C                   MOVE      DLRFGS        BRVFGS
183100000127     C                   Z-ADD     DLRFGS        BRVPES
183200960410     C                   MOVE      SPPLNP        BRVLNP
183300960410     C                   MOVE      SPPLNA        BRVLNA
183400960410     C                   MOVE      SPPNRS        BRVNRS
183500960410     C                   MOVE      SPPNSC        BRVNSC
183600960410     C                   MOVE      SPPZNC        BRVZNC
183700960410     C                   MOVE      89            BRVNPS
183800070215     c                   time                    wtimez
183900070215     c* millisecondi
184000070215     c                   extrct    wtimez:*ms    millisec          6 0
184100070215     c                   movel     millisec      brvmis
184200070215     c* data immissione
184300070215     c                   movel     wtimez        dataiso
184400070215     c                   movel     dataiso       brvdfs
184500070215     c* ora immissione
184600070215     c                   move      wtimez        oraiso
184700070215     c                   movel     oraiso        brvhms
184800070215     c*
184900960410     C                   MOVE      SIMFEL        BRVFLE
185000070215     c                   movel     'A'           brvtap
185100070215     c                   eval      brvpru='§'+ nompgm
185200070110     C                   WRITE     FNBRV000                             34
185300960410     C     *IN34         IFEQ      *OFF
185400960430     C* Richiamo pgm per generare anomalia di disguido se si tratta di
185500960430     C* disguido; lo richiamo per aggiornare la bolla transito se si
185600960430     C* tratta di transito.
185700070222     c                   clear                   fnls45ds
185800070222     c                   movel     'S'           dls45ric
185900070222     c                   move      brvpes        dls45pes
186000070222     c                   call      'FNLS45R'
186100070222     c                   parm                    fnls45ds
186200070222     c                   parm                    dsbrv
186300970509     C**
186400970509     C                   MOVEL     'S'           WFC44             1
186500960410     C                   END
186600960410     C                   ENDSR
186700950810     C**************************************************************************
186800950810     C     *INZSR        BEGSR
186900101202     C                   Z-ADD     1             CODUT             1 0
187000101202      *
187100101202      * Reperimento dati utente/aziendali
187200101202     c                   exsr      sr_DatiJob
187300101202      *
187400040603     C* RILEVO DATA GIORNO IN AAAA/MM/GG  E ORA
187500040603     C                   TIME                    WTIME            14 0
187600040603     C                   MOVE      WTIME         WDATE                          GG/MM/AAAA
187700040603     C                   CLEAR                   WLBDA8
187800040603     C                   MOVE      WDATE         G08DAT
187900040603     C                   CALL      'XSRDA8'
188000040603     C                   PARM                    WLBDA8
188100040603     C                   MOVE      G08INV        DATEU8
188200040603     C                   MOVEL     WTIME         UTIME             6 0
188300950810     C*-----------------------------------------------------
188400030114     C     *LIKE         DEFINE    ANMCDU        WSACDU
188500030114     C     *LIKE         DEFINE    ANMNFV        WSANFV
188600030114     C     *LIKE         DEFINE    ANMNPS        WSANPS
188700030114     C     *LIKE         DEFINE    ANMFL1        WSAFL1
188800030114     C     *LIKE         DEFINE    ANMFT1        WSAFT1
188900030114     C     *LIKE         DEFINE    ANMFL2        WSAFL2
189000030114     C     *LIKE         DEFINE    BRVNPG        KNPG                           CATEGORIA
189100030114     C     *LIKE         DEFINE    BRVNPG        KNPG2                          CATEGORIA
189200030114     C     *LIKE         DEFINE    BRVNPG        KNPG3                          CATEGORIA
189300030114     C     *LIKE         DEFINE    BRVLNP        WLNP                           LNP
189400030114     C     *LIKE         DEFINE    BRVLNP        LNPB
189500030114     C     *LIKE         DEFINE    BRVLNP        KLNP                           LNP
189600030114     C     *LIKE         DEFINE    BRVLNA        KLNA                           LNA
189700030114     C     *LIKE         DEFINE    BRVNRS        KNRS                           SERIE
189800030114     C     *LIKE         DEFINE    BRVNSC        KNSC                           SEGNACOLLO
189900030114     C     *LIKE         DEFINE    BRVFGS        KFGS                           FIL.GESTIONE
190000030114     C     *LIKE         DEFINE    BRVFGS        KFGS3
190100030114     C     *LIKE         DEFINE    FVANFV        KNFV                           NR.FOGLIO
190200030114     C     *LIKE         DEFINE    FVANFV        WNFV                           NR.FOGLIO
190300030114     C     *LIKE         DEFINE    BRVNFV        KNFV3                          NR.FOGLIO
190400030114     C     *LIKE         DEFINE    FVVDFV        STADFV
190500030114     C     *LIKE         DEFINE    FVVDFV        WDFV
190600030114     C     *LIKE         DEFINE    FVVFGS        WFGS
190700030114     C     *LIKE         DEFINE    ANMFL1        WFL1
190800030114     C     *LIKE         DEFINE    ANMFL2        WFL2
190900030114     C     *LIKE         DEFINE    D55TFP        WTFP1
191000030114     C     *LIKE         DEFINE    D55TFP        WTFP2
191100030114     C     *LIKE         DEFINE    FVALAI        SAVLAI
191200030114     C     *LIKE         DEFINE    FVALAI        WFLP
191300030114     C     *LIKE         DEFINE    G08INV        DATEU8
191400030114     C     *LIKE         DEFINE    G08INV        WDATE
191500030114     C     *LIKE         DEFINE    D55TFA        TFASAV
191600030114     C     *LIKE         DEFINE    CAEEPA        KEPA
191700030114     C*-----------------------------------------------------
191800070110     C     KBRV2         KLIST                                                  FNBRV02L
191900950810     C                   KFLD                    KNPG                           ..CATEGORIA
192000000127     C                   KFLD                    KNFV3                          ..NR.FOGLIO
192100950810     C                   KFLD                    FVALNP                         ..FIL.GESTIONE
192200950810     C                   KFLD                    SAVLNA            3 0          ..LNA
192300070110     C     KBRV4         KLIST                                                  FNBRV09L
192400950810     C                   KFLD                    KNPG                           ..CATEGORIA
192500950810     C                   KFLD                    KLNP                           ..LNP
192600950810     C                   KFLD                    KLNA                           ..LNA
192700950810     C                   KFLD                    KNRS                           ..SERIE
192800950810     C                   KFLD                    KNSC                           ..SEGNACOLLO
192900070110     C     KBRV5         KLIST                                                  FnBRV09L
193000950811     C                   KFLD                    KLNP                           ..LNP
193100950811     C                   KFLD                    KLNA                           ..LNA
193200950811     C                   KFLD                    KNRS                           ..SERIE
193300950811     C                   KFLD                    KNSC                           ..SEGNACOLLO
193400040109     C****               KFLD                    KFGS                           ..FIL.GESTIONE
193500950810     C     KFVA1         KLIST                                                  FNFVA02L
193600950810     C                   KFLD                    DLRNPG                         ..CATEGORIA
193700950810     C                   KFLD                    DLRNFV                         ..NR.FOGLIO
193800020917     C                   KFLD                    DLRFGS
193900950810     C     KSPP2         KLIST                                                  *
194000950810     C                   KFLD                    BRVLNP                         *
194100950810     C                   KFLD                    BRVLNA                         *
194200950810     C                   KFLD                    BRVNRS                         *
194300950810     C                   KFLD                    BRVNSC                         *
194400950810     C     KSPP3         KLIST                                                  *
194500950810     C                   KFLD                    SPPLPV                         *
194600000127     C                   KFLD                    WNFV                           *
194700950810     C     KANM          KLIST                                                  *
194800950810     C                   KFLD                    STALNP                         *
194900950810     C                   KFLD                    STALNA                         *
195000950810     C                   KFLD                    STANRS                         *
195100950810     C                   KFLD                    STASCN                         *
195200950810     C                   KFLD                    COMCAA                         *
195300950810     C     KART          KLIST                                                  *
195400950811     C                   KFLD                    KLNP
195500950811     C                   KFLD                    KLNA
195600950811     C                   KFLD                    KNRS
195700950811     C                   KFLD                    KNSC
195800021122     C     KBTT          KLIST                                                  *
195900021122     C                   KFLD                    DLRFGS
196000021122     C                   KFLD                    KLNP
196100021122     C                   KFLD                    KLNA
196200021122     C                   KFLD                    KNRS
196300021122     C                   KFLD                    KNSC
196400950810     C     KARB          KLIST                                                  *
196500950810     C                   KFLD                    ARTAAS                         *
196600950810     C                   KFLD                    ARTLNP                         *
196700950810     C                   KFLD                    ARTNRS                         *
196800950810     C                   KFLD                    ARTNSP                         *
196900040531     C     KAR5          KLIST                                                  *
197000040531     C                   KFLD                    blTAAS                         *
197100040531     C                   KFLD                    blTLNP                         *
197200040531     C                   KFLD                    blTNRS                         *
197300040531     C                   KFLD                    blTNSP                         *
197400040531     C                   KFLD                    ktrd                           *
197500950810     C     KTAB          KLIST
197600950810     C                   KFLD                    CODUT
197700950810     C                   KFLD                    COD               2
197800950810     C                   KFLD                    KEY               8
197900950810     C     KFVV          KLIST                                                  FNFVV06L
198000950810     C                   KFLD                    KNPG2
198100950811     C                   KFLD                    WDFV
198200950817     C                   KFLD                    DLRFGS                                     NE
198300950810     C     KFVV1         KLIST                                                  FNFVV01L
198400950810     C                   KFLD                    KNPG                           ..CATEGORIA
198500950810     C                   KFLD                    KNFV                           ..NR.FOGLIO
198600950810     C                   KFLD                    KFGS                           ..FIL.GESTIONE
198700950810     C     KFVV2         KLIST                                                  FNFVV01L
198800950811     C                   KFLD                    KNPG3
198900950811     C                   KFLD                    KNFV3
199000950811     C                   KFLD                    KFGS3
199100980114     C*
199200980114     C     KCAE          KLIST
199300980114     C                   KFLD                    KEPA
199400980114     C                   KFLD                    FVVFGS
199500980114     C                   MOVEL     'A'           KEPA
199600000503     C*
199700000503     C     KFWA          KLIST
199800000503     C                   KFLD                    FVALNP
199900000503     C                   KFLD                    FVANFV
200000000503     C                   KFLD                    FVALAI
200100950810     C*
200200990623     C                   Z-ADD     0             COMDFS            8 0
200300990623     C                   Z-ADD     0             COMHMS            4 0
200400950810     C*---------------------------------------------------------------*
200500950810     C                   CLEAR                   COMMAN
200600950810     C                   MOVEA     CMD2          COMMAN
200700040322     C                   Z-ADD     51            LUNG             15 5
200800950810     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
200900040322     C                   PARM                    COMMAN           80
201000950810     C                   PARM                    LUNG
201100950810     C                   OPEN      QSYSPRT
201200030114      *
201300950810     C                   ENDSR
201400980401     C**************************************************************************
201500980401     C* TERMINAL DI ARRIVO DELLA FIL GESTIONE SPUNTA LETTA
201600980401     C**************************************************************************
201700980401     C     TERFGS        BEGSR
201800980401     C* IMPOSTO TERMINAL PARTENZA E ARRIVO
201900980401     C                   CLEAR                   DSLV55
202000050929     C                   MOVEL     ' '           D55TPT
202100980401     C                   MOVEL     LNPB          D55LNP
202200980401     C                   MOVEL     FVADFV        D55DRF
202300980401     C                   MOVE      BRVFGS        D55LIN
202400980401     C                   CALL      'FNLV55R'
202500980401     C                   PARM                    DSLV55
202600980401     C**
202700980401     C     D55ERR        IFEQ      ' '
202800980401     C                   MOVE      D55TFA        FGSFE0            3 0
202900980401     C                   ELSE
203000980401     C                   MOVE      BRVFGS        FGSFE0
203100980401     C                   END
203200050929     c*
203300050929     c                   movel     d55tfp        fgstfp            3 0
203400980401     C                   ENDSR
203500101202
203600101202      *---------------------------------------------------------------*
203700101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
203800101202      *---------------------------------------------------------------*
203900101202     c     sr_DatiJob    BEGSR
204000101202      *
204100101202     c     *dtaara       define    §azute        AZUTEds
204200101202     c     *dtaara       define    §datiute      dDatiUte
204300101202      *
204400101202     c                   in(E)     *dtaara
204500101202     c                   IF        %Error or RSUT = *blanks
204600101202     c                   clear                   TIBS34ds
204700101202     c                   call      'TIBS34R'
204800101202     c                   parm                    TIBS34ds
204900101202     c                   in        *dtaara
205000101202     c                   ENDIF
205100101202      *
205200101202     c                   ENDSR
205300941222     C**************************************************************************
205400941222     OFNFVV000  E            UPDFVV
205500941222     O                       FVVFCF
205600941222     C**************************************************************************
205700911015     OQSYSPRT   E            TES              02
205800911015     O                       STA(1)              66
205900911015     O                       STA(2)             132
206000900626     O                       RSUT                20
206100911015     O                                           21 '/'
206200911015     O                       SIMFEL              24
206300040611     O                       WDATE              115 '  /  /    '
206400901115     O                       PAGE          Z    132
206500921124     O          E            TES         1
206600941222     O                       KNSIF               10
206700941222     O                       KNMUS               22
206800941222     O                       UTIME              115 '  :  :  '
206900921124     O          E            TES         1
207000911015     O                       STA(3)              66
207100911109     O               10      STA(9)             132
207200930205     O                       DESNPG              19
207300950817     O                       DLRFGS              26
207400950811     O                       DESFGS              42
207500941228     O                       DLRNPG        Z     48
207600941228     O                       DLRNFV        Z     54
207700941223     O                       STADFV              71 '  /  /    '
207800941222     O*-------------------------------------
207900921124     O          E            TES1        2
208000911015     O                       STA(4)              66
208100930205     O                       SAVLAI              40
208200921124     O                       DESLAI              61
208300941222     O*-------------------------------------
208400911015     O          E            TES2        1
208500921124     O                       STF                132
208600941222     O*-------------------------------------
208700920716     O          E            TES3        2
208800920716     O                       STA(6)              66
208900920716     O                       STA(11)            132
209000920716     O          E            TES3        1
209100920716     O                       STA(10)             66
209200920716     O                       STA(12)            132
209300941222     O*-------------------------------------
209400911015     O          E            DET         1
209500911015     O                       STALNP               3
209600921126     O                                         +  1 '-'
209700921126     O                       DESLNP            +  1
209800921126     O                       STALNA            +  2
209900921126     O                                         +  1 '-'
210000921126     O                       DESLNA            +  1
210100921126     O                       STANRS        Z   +  2
210200921126     O                       STASCN        Z   +  2
210300921126     O                       STAZNC            +  4
210400921126     O                       STALPV            +  2
210500921126     O                       STAFV         Z   +  0
210600921126     O                       STANPP            +  3
210700921126     O                       STANPA            +  6
210800000127     O                       STAERR            +  2
210900921126     O                  05   ERR(4)             122
211000000127     O                  05   SPPLPV             114
211100921126     O                  05   SPPNFV        Z    121
211200921126     O                  05                      123 '-'
211300941222     O                  05   STSDFV             132 '  /  /  '
211400941222     O**************************************************************************
211500960212**
211600941222XXXXXXXXXXXXXXXXXXXX/XXX        ** I.D.A. : INVENTARIO DISCORDANZE   1
211700950811 ARRIVI **                FNLR32R        XX/XX/XX        PAG. XXXX   2
211800941223FOGLIO XXXXXXXXXXXX DI XXX-XXXXXXXXXXXXXXX  N. X-XXXXX  DEL  XX/XX
211900921124FOGLI VIAGGIO PARTENZE ABBINATI PER: XXX-XXXXXXXXXXXXXXXXXXXX        4
212000921126XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XX   5
212100921126Linea Partenza    Linea Arrivo     Ser Segnacollo Zona F.Viaggio     6
212200921126XXX - XXXXXXXXXX  XXX - XXXXXXXXXX  xx  XXXXXXX    XX  AAAAXXXXX     7
212300920716I.D.A. NON ELABORATO A CAUSA DI ERRORI: RILANCIARE L'ELABORAZIONE!   8
212400920716               ***  C  O  N  F  E  R  M  A  ***                      9
212500000127                                                  Arr. Partenze     10
212600000127 Lett.BARCODE                                                       11
212700000127In Part In Arr                                                      12
212800921124XXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX XXX-XXXXX     13
212900921126 XX      XX   XXXXXXXXXXAAAAAAAAAAAAAAAAAAAAAXXXXXXXXXXXXXXXXXXXXX  14
213000920716**
213100921126PARTITO  NON ARRIVATO                                                1
213200921126ARRIVATO NON PARTITO                                                 2
213300921126ARRIVATO  MA PARTITO                                                 3
213400000127SU F.V. N.XXX-XXXXXX - XX/XX/XX                                      4
213500941222**         CMD2
213600960212OVRPRTF FILE(QSYSPRT) USRDTA('I.D.A.')  HOLD(*YES)
