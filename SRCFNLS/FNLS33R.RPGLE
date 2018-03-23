000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200110414     H* FNLS33R *-----------------------------------------------------*
000300110414     H*       - Bolle per operatore - lancio per area FILTRO          *
000400930913     H*---------------------------------------------------------------*
000500110429     FFNLS33D   CF   E             WORKSTN
000600921002     FTABEL00F  IF   E           K DISK
000601110503     Fazorg01l  IF   E           K DISK
000602110428     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
000700930913     D MC              S              3  0 DIM(30)
000701110414     D $pog            s              3    dim(250)
000702110415     D $ARG            s              3    dim(50)
000704110414      *
000705110414     d wabi            s                   like(UteAut)
000706110429     d codut           s              1  0 inz(1)
000707110429     d waut            s              1    inz(*off)
000708110504     d s_orgcar        s              3
000800950308     D*
000900950308     D DSLS01        E DS                  INZ
001000930913     D KPJBA         E DS
001300970901     D* DS PER TRUL06R - CARICAMENTO £X
001400970901     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001500970901     D  LIN                    1     90  0
001600970901     D                                     DIM(30)
001700930913     D*----------------------------------
001800941227     D* PARAMETRI CONTROLLO/INVERSIONE/CALCOLO GIORNI DATA   - XSRDA8 -
001900930913     D*----------------------------------
002000921002     D WLBDAT          DS
002100941227     D  G02DAT                 1      8  0
002200941227     D  G02INV                 9     16  0
002300941227     D  G02ERR                17     17
002400941227     D  G02TGI                18     22  0
002500930913     D*----------------------------------
002600941227     D* PARAMETRI CALCOLO DATA DA GIORNI DATA                - XSRGI8 -
002700930913     D*----------------------------------
002800930913     D WGIDAT          DS
002900941227     D  GIODAT                 1      8  0
003000941227     D  GIOINV                 9     16  0
003100941227     D  GIOTGI                17     21  0
003101110414      *
003102110414     d Tibs02Ds      e ds
003103110414     D TRUL31DS      E DS
003104110429     D TRUL31DS2     E DS
003200930913     D*----------------------------------
003300110414     D* PARAMETRI RICERCA DATI UTENTI
003400930913     D*----------------------------------
003601110414     d Azuteds       e ds                  extname(Azute00f)
003602110414     d dDatiute      e ds
003603110414     d dLat          e ds
003604110415     d dute01        e ds
003605110414     D TIBS34DS      E DS
003700930913     D*----------------------------------
003800930913     D* DS GIORNI DI PULIZIA  - 5A-
003900930913     D*----------------------------------
004000930913     D DS5A          E DS
004100000606     D*----------------------------------
004101110415     d ds05          e ds
004102110415      *
004200000608     D* DS PER FNLSC4R1 - CONTROLLO ELABORAZIONE PER POSTE - PARAMETRI:
004300000608     D* IC4LIN = LINEA
004400000608     D* IC4FLG = I=INTERATTIVA (1 RICHIESTA) .. NON UTILIZZATO B=BATCH
004500000606     D*----------------------------------
004600000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
004700930913     C*---------------------------------------------------------------*
004800930913     C*                    MAIN                                       *
004900930913     C* PROGRAMMA FILTRO STAMPA BOLLE PER OPERATORE                   *
005000930913     C*---------------------------------------------------------------*
005200930913     C* DEFINIZIONE VARIABILI, KLIST E PLIST
005201110414     c                   eval      waut=*off
005300930913     C                   EXSR      DEFVAR
005301110414     c                   if        *in90
005302110414     c                   eval      waut=*on
005303110414     c                   endif
005400930913     C*
005500930913     C* CARICAMENTO FILIALI MECCANIZZATE
005600921002     C                   EXSR      CARTAB
005700900517     C*
005800900524     C                   Z-ADD     0             VIDDSD
005900900524     C                   Z-ADD     0             VIDDSA
006000950308     C                   Z-ADD     0             DS1AAL
006100930913     C*
006200900509     C     FOR01         TAG
006300941227     C                   EXFMT     LS31D01
006301110505     c                   if        waut=*on
006302110505     c                   eval      *inkc=*on
006303110505     c                   endif
006400930913     C*
006500930913     C* CMD3 - FINE LAVORO
006600900509     C   KC              GOTO      FINE
006700930913     C*
006800900524     C* CONTROLLO
006900900524     C                   EXSR      CONTR
007000941227     C  NKF
007100900524     COR 90              GOTO      FOR01
007200930913     C*
007300941227     C* CMD6 - ELABORAZIONE
007400950308     C*
007500950308     C                   MOVEL     *BLANKS       DS1FPG
007600950308     C                   Z-ADD     VIDLNP        DS1LNP
007700950308     C                   Z-ADD     VIDDSD        DS1GDA
007800950308     C                   Z-ADD     VIDDSA        DS1GAL
007802110503     c                   movel(P)  vidcar        ds1flo
007803110504     c                   if        s_orgcar<>*blanks
007804110504     c                   movel(P)  s_orgcar      ds1flo
007805110504     c                   endif
007900950308     C                   MOVEL     DSLS01        KPJBU
008000941227     C                   MOVEL     'LS32'        KCOAZ
008100900523     C                   CALL      'BCH10'
008200900523     C                   PARM                    KPJBA
008300900523     C*
008400000000     C     FINE          TAG
008500000000     C                   SETON                                        LR
008600930913     C*---------------------------------------------------------------*
008700930913     C*                    CONTR                                      *
008800930913     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
008900930913     C*---------------------------------------------------------------*
009000900524     C     CONTR         BEGSR
009100930913     C*
009200110428     C                   SETOFF                                       9028
009201110504     c                   clear                   s_orgcar
009300930913     C*
009400921002     C* DATA SPEDIZIONE DAL OBBLIGATORIA
009500921002     C                   MOVE      VIDDSD        G02DAT
009600921002     C                   MOVEL     *BLANK        G02ERR
009700941227     C                   CALL      'XSRDA8'
009800921002     C                   PARM                    WLBDAT
009900930913IF  1C     G02ERR        IFEQ      '1'
010000921002     C                   SETON                                        40  90
010100921002     C                   GOTO      ENDCTR
010200930913E   1C                   ENDIF
010300950308     C                   Z-ADD     G02INV        DS1ADA
010400941227     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
010500941227     C                   Z-ADD     G02DAT        VIDDSD
010600900524     C*
010700930913IF  1C     VIDDSA        IFNE      0
010800921002     C                   MOVE      VIDDSA        G02DAT
010900921002     C                   MOVEL     *BLANK        G02ERR
011000941227     C                   CALL      'XSRDA8'
011100921002     C                   PARM                    WLBDAT
011200930913IF  2C     G02ERR        IFEQ      '1'
011300921002     C                   SETON                                        41  90
011400921002     C                   GOTO      ENDCTR
011500930913E   2C                   ENDIF
011600950308     C                   Z-ADD     G02INV        DS1AAL
011700941227     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
011800941227     C                   Z-ADD     G02DAT        VIDDSA
011900930913     C*
012000930913     C* DATA DAL MINORE DATA AL
012100950308IF  2C     DS1ADA        IFGT      DS1AAL
012200930913     C                   SETON                                        4290
012300930913     C                   GOTO      ENDCTR
012400930913E   2C                   ENDIF
012500930913E   1C                   ENDIF
012600930913     C*
012700930913     C* REPERISCO DATA ULTIMO UTILIZZO PROGRAMMA DI PULIZIA BOLLE
012800941227     C                   MOVEL     'FNLS61R'     PGM
012900930913     C                   MOVEL     *BLANKS       DAT
013000930913     C                   MOVEL     *BLANKS       ERR
013100930913     C*
013200930915     C                   CALL      'TRUL49C'
013300930913     C                   PARM                    PGM              10
013400000103     C                   PARM                    DAT               8
013500930913     C                   PARM                    ERR               1
013600930913     C*
013700930913     C* PROGRAMMA IN USO: IMPOSSIBILE REPERIRE LA DATA
013800930913IF  1C     ERR           IFEQ      '1'
013900930913     C                   SETON                                        4490
014000930913     C                   GOTO      ENDCTR
014100930913X   1C                   ELSE
014200930913     C*
014300930913     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
014400930913     C* PULIZIA E TROVO LA DATA AL DI SOTTO DELLA QUALE NON SI POSSONO
014500930913     C* LANCIARE ELABORAZIONI PER MANCANZA DI DATI
014600950203     C                   CLEAR                   G02INV
014700930914     C                   MOVE      DAT           G02INV
014800930914     C*
014900930914     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
015000930914IF  2C     G02INV        IFEQ      *ZEROS
015100930914     C                   Z-ADD     *ZEROS        GIOINV
015200930914X   2C                   ELSE
015300930914     C*
015400930913     C                   Z-ADD     *ZEROS        G02DAT
015500930913     C                   Z-ADD     *ZEROS        G02TGI
015600930913     C                   MOVEL     '3'           G02ERR
015700941227     C                   CALL      'XSRDA8'
015800930913     C                   PARM                    WLBDAT
015900930913     C*
016000930913     C                   MOVEL     '5A'          COD
016100930913     C                   MOVEL     *BLANKS       KEY
016200930913     C                   MOVEL     '1'           KEY
016300930913     C     KTAB          CHAIN     TABEL00F                           30
016400930914IF  3C     *IN30         IFEQ      *ON
016500930913     C                   MOVEL     *BLANKS       DS5A
016600930914X   3C                   ELSE
016700930913     C                   MOVEL     TBLUNI        DS5A
016800930914E   3C                   ENDIF
016900930913     C*
017000930913     C                   SUB       §5ABLP        G02TGI
017100930913     C*
017200930913     C                   Z-ADD     *ZEROS        GIODAT
017300930913     C                   Z-ADD     *ZEROS        GIOINV
017400930913     C                   Z-ADD     G02TGI        GIOTGI
017500930913     C*
017600941227     C                   CALL      'XSRGI8'
017700930913     C                   PARM                    WGIDAT
017800930914E   2C                   ENDIF
017900930913     C*
018000930913     C* DATA DAL MAGGIORE DATA CALCOLATA
018100950308IF  2C     DS1ADA        IFLE      GIOINV
018200930913     C                   SETON                                        4590
018300930913     C                   GOTO      ENDCTR
018400930913E   2C                   ENDIF
018500930913E   1C                   ENDIF
018501110503     c* codice area
018502110504     c                   if        vidcar=*blanks and vidlnp=0
018503110503     c                   seton                                        904628
018504110503     c                   movel     msg(3)        vidmsg
018505110503     c                   goto      endctr
018506110503     c                   endif
018507110503     c*
018508110503     c* . . . . . . deve essere fra quelli nella schiera
018509110415     c                   clear                   viddar
018511110503    1c                   if        vidcar<>*blanks
018512110415    2c                   if        %lookup(vidcar:$arg)=0
018513110428     c                   seton                                        904628
018514110415     c                   movel     msg(2)        vidmsg
018515110415     c                   goto      endctr
018516110415    2c                   endif
018517110503     c                   exsr      deccar
018524110415    1c                   endif
018525110415     c
018600921002     C*
018700930913     C* FILIALE FRA QUELLE CARICATE
018800930913IF  1C     VIDLNP        IFNE      0
018801110503     c*                  if        *in06=*off
018900000606      * .. CONTROLLO CHE RISPETTI LE REGOLE DI ELABORAZIONE (PER POSTE)
019000110503     C*                  CLEAR                   FNLSC4
019100110503     C*                  Z-ADD     VIDLNP        IC4LIN
019200110503     C*                  MOVE      'I'           IC4FLG
019300110503     C*                  MOVEL     FNLSC4        KPJBU
019400110503     C*                  CALL      'FNLSC4R1'
019500110503     C*                  PARM                    KPJBA
019600110503     C*                  MOVEL     KPJBU         FNLSC4
019700000606      *
019800110503     C*    OC4ESE        IFNE      'S'
019900110503     C*                  SETON                                        4390
020000110503     C*                  GOTO      ENDCTR
020100110503     C*                  ENDIF
020101110503     c*                  else
020102110429     c                   if        %lookup(%editc(vidlnp:'X'):$pog)=0
020103110415     C                   SETON                                        4390
020104110415     C                   GOTO      ENDCTR
020105110415     c                   endif
020106110503     c                   move      vidcar        w0030             3 0
020107110503     c     vidlnp        chain     azorg01l
020108110504     c                   if        %found(azorg01l) and
020109110504     c                             w0030>0 and orgcar<>w0030
020110110503     C                   SETON                                        4390
020111110503     C                   GOTO      ENDCTR
020112110503     c                   endif
020113110504     c                   if        w0030 = 0
020114110504     c                   move      orgcar        s_orgcar
020115110504     c                   endif
020116110504
020200930913E   1C                   ENDIF
020300930913     C*
020400900524     C     ENDCTR        ENDSR
020401110503     C*---------------------------------------------------------------*
020402110503     C*                    DECCAR                                     *
020403110503     C* Decodifica codice area                                        *
020404110503     C*---------------------------------------------------------------*
020405110503     c     deccar        begsr
020406110503     c                   eval      cod='05'
020407110503     c                   eval      key=vidcar
020408110503     c     ktab          chain     tabel00f
020409110503    2c                   if        %found(tabel00f)
020410110503     c                   movel     tbluni        ds05
020411110503     c                   eval      viddar=§05des
020412110503    2c                   endif
020413110503     c                   endsr
020500930913     C*---------------------------------------------------------------*
020600930913     C*                    CARTAB                                     *
020700930913     C* ROUTINE DI CARICAMENTO FILIALI MECCANIZZATE                   *
020800930913     C*---------------------------------------------------------------*
020900921002     C     CARTAB        BEGSR
021000970901     C                   CLEAR                   DSUL06
021100970901     C                   MOVE      '£1'          D06COD
021200970901     C                   MOVEL     SIMFEL        D06KEY
021300970901     C                   MOVEL     'L'           D06TLA
021400000103     C                   MOVEL     DSUL06        KPJBU
021500970901     C*
021600970901     C                   CALL      'TRUL06R'
021700970901     C                   PARM                    KPJBA
021800970901     C                   MOVEL     KPJBU         DSUL06
021900970901     C                   MOVEA     LIN           MC
021901110415      *
021902110415     C                   Z-ADD     dutPOU        DS1FGS
023700921002     C*
023800930913     C                   ENDSR
023900930913     C*---------------------------------------------------------------*
024000930913     C*                    DEFVAR                                     *
024100930913     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
024200930913     C*---------------------------------------------------------------*
024300930913     C     DEFVAR        BEGSR
024400930913     C*
024401110414     C     *ENTRY        PLIST
024402110414     C                   PARM                    KPJBA
024403110414     C*
024404110414     c     *dtaara       define    §azute        azuteds
024405110414     c     *dtaara       define    §datiute      ddatiute
024406110414     c                   in(E)     *dtaara
024407110414    1c                   If        %error  or RSUT = *blanks
024408110414     c                   Clear                   Tibs34ds
024409110414     c                   Call      'TIBS34R'
024410110414     c                   Parm                    Tibs34ds
024411110414     c                   In        *dtaara
024412110414    1c                   EndIf
024413110414
024414110414     c                   Clear                   wabi
024415110415     c                   Clear                   vidmsg
024416110414     c                   Clear                   dLat
024417110414
024418110414      * Verifica errori e autorità profilo
024419110414s   1c                   Select
024420110414      * se ho errori nei dati utente esco dal pgm
024421110414w   1c                   When      DutErr = 'E'
024422110414     c                   eval      *in90=*on
024423110414     c                   leavesr
024424110414      * se non c'è l'abilitazione
024425110414      * --> se 1° livello, abilitazioni al terminal
024426110414      *     se 2° livello, abilitazioni al punto operativo
024427110414      *     se sede è impossibile ma se capita mando a fine il pgm
024428110414w   1c                   When      UteAut = *Blanks
024429110414if  2c                   If        DutLpo = '1'
024430110414     c                   Eval      wabi   = 'TP'
024431110414e   2c                   EndIf
024432110414if  2c                   If        DutLpo = '2'
024433110414     c                   Eval      wabi   = 'PO'
024434110414e   2c                   EndIf
024435110414if  2c                   If        DutLpo = 'S'
024436110414     c                   eval      *in90=*on
024437110414     c                   leavesr
024438110414e   2c                   EndIf
024439110414      * carica le abilitazioni del profilo
024440110414x   1c                   Other
024441110415     c                   movel     utefaf        dute01
024445110414     c                   Eval      wabi = UteAut
024447110414e   1c                   EndSl
024448110505     c* Errore se utente non abilitato al "Monitoraggio Spedizioni"
024449110505     c                   if        §utestp<>'S'
024450110505     c                   Seton                                        2890
024451110505     c                   movel     MSG(1)        vidmsg
024452110505     c                   leavesr
024454110505     c                   endif
024455110414
024456110414      * controllo se ok l'abilitazione dell'utente
024457110414     c                   Clear                   Tibs02ds
024458110414     c                   Eval      T02Mod = 'C'
024459110414     c                   Eval      T02Sif = knsif
024460110414     c                   Eval      T02Cod = 'LAT'
024461110414     c                   Movel(p)  wabi          T02Ke1
024462110414     c                   Call      'TIBS02R'
024463110414     c                   Parm                    kpjba
024464110414     c                   Parm                    Tibs02ds
024465110414if  1c                   If        T02Err = *Blanks
024466110414     c                   Eval      dLat = T02Uni
024467110414e   1c                   EndIf
024468110414      * errore o non abilitato
024469110414if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
024470110414     c                   Seton                                        2890
024471110415     C                   movel     msg(1)        vidmsg
024472110414     c                   leavesr
024473110414   x1c                   Else
024474110414      * Reperimento dei P.O. gestibili dall'utente
024475110414     c                   clear                   TRUL31DS
024476110415     c                   clear                   TRUL31DS2
024477110414     c                   eval      I31abi = wabi
024478110414     c                   eval      I31cdi = DUTdis
024479110414     c                   eval      I31car = DUTare
024480110414     c                   eval      I31cpo = DUTpou
024481110414     c                   call      'TRUL31R'
024482110414     c                   parm                    KPJBA
024483110414     c                   parm                    TRUL31DS
024484110415     c                   parm                    TRUL31DS2
024485110414if  2c                   if        O31pog > *zeros
024486110414     c                   movea     O31pog        $pog
024487110415     c                   movea     O31arg        $arg
024488110503     c                   if        $arg(2)=*blanks
024489110503     c                   move      $arg(1)       vidcar
024490110503     c                   exsr      deccar
024491110503     c                   endif
024496110414e   2c                   endif
024497110414    1c                   endif
025300930913     C*
025400941227     C* ACCESSO TABEL00F
025500930913     C     KTAB          KLIST
025600930913     C                   KFLD                    CODUT
025700941227     C                   KFLD                    COD
025800941227     C                   KFLD                    KEY
025900930913     C*
026000941227     C* DEFINIZIONE CAMPI
026100941227     C     *LIKE         DEFINE    TBLCOD        COD
026200941227     C     *LIKE         DEFINE    TBLKEY        KEY
026300930913     C                   ENDSR
026301110414**
026400110505Utente non autorizzato alla stampa Bolle operatore per Area                   01
026500110428Utente non abilitato all'area richiesta                                       02
026600110505Immettere il codice area o la filiale di bollettazione                        03
