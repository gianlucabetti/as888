000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020809      * FNLR14R *----------------------------------------------------*
000300020809      *     - RICEZIONE BOLLE PARTENZE ALL'ARRIVO
000400020809      *--------------------------------------------------------------*
000500000622     FTABEL00F  IF   E           K DISK
000600940322     FAZORG01L  IF   E           K DISK
000700960529     FFNFGV01L  IF   E           K DISK
000800960529     FFNFVA01L  IF   E           K DISK
000900921022     F* BOLLE ARRIVI
001000950118     FFNARB01L  UF A E           K DISK
001100950413     FFNART27L  UF A E           K DISK
001200961121     FFNART01L  UF   E           K DISK
001300950118     F                                     RENAME(FNART000:FNART001)
001400080407     FFNblt01L  UF   E           K DISK
001500060220     FFiAR401L  UF A E           K DISK
001600051013     FFIAR601L  if   E           K DISK
001700051116     FFiAR901L  iF   E           K DISK
001800921022     F* BOLLE PARTENZE RICEZIONE
001900950118     FFNBLPE0R  UF   E           K DISK    USROPN
002000921022     F*
002100990510     FFNANM02L  IF   E           K DISK
002200990510     F                                     INFDS(FNANM2)
002300990510     FFNANM01L  IF   E           K DISK
002400990510     F                                     RENAME(FNANM000:FNANM001)
002500990510     F                                     INFDS(FNANM1)
002600961121     FFNEVS02L  UF   E           K DISK    USROPN
002700070123     FFNBRV07L  IF   E           K DISK
002800070123     FFNBRV09L  IF   E           K DISK
002900070123     F                                     RENAME(FNBRV000:FNBRV009)
003000951011     FFNAGB01L  O  A E           K DISK
003100061031     ffnblp01l  if   e           k disk    prefix(w_)
003200030519
003300021008     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
003400021008     D CMD6            S             55    DIM(1) CTDATA PERRCD(1)
003500021008     D CMD9            S             55    DIM(1) CTDATA PERRCD(1)
003600050804
003700050804     d CmdDSPLOG       s             55    Dim(1) Ctdata Perrcd(1)
003800911011     D*
003900980623     D TDF             S              1    DIM(10)                              COSTANTI STAMPA
004000000622     D DVA             S              3    DIM(10)                              DIVISE ACC.ITAA
004100011025     D DEC             S              2  0 DIM(10)                              NUMERO DECIMALI
004200010118     D MSG             S             70    DIM(10) CTDATA PERRCD(1)             MSG POSTA ELET
004300040317     D NFV             S              5  0 DIM(10)                              MSG POSTA ELET
004400020226
004500040317     d kfgs            s                   like(brvfgs)
004600040317     d lnantw          s                   like(§ogntw)
004700051013     d lnpntw          s                   like(§ogntw)
004800030401     d wtfplna         s                   like(d55tfp)
004900020809     D*
005000020809     D KCEV            S                   LIKE(EVSCEV)
005100020809     D WCEV            S                   LIKE(EVSCEV)
005200020809     D WDEV            S                   LIKE(EVSDEV)
005300020809     D WDFV            S                   LIKE(FGVDFV)
005400020809     D KNFV            S                   LIKE(FGVNFV)
005500020809     D KNPG            S                   LIKE(BRVNPG)
005600020809     D LNATFA          S                   LIKE(ARBTFA)
005700031203     D PESTFA          S                   LIKE(ARBTFA)
005800020809     D SAVLNP          S                   LIKE(BLPLNP)
005900031210     D SAVLNA          S                   LIKE(BLPLNA)
006000020809     D WTRC            S                   LIKE(AR4TRC)
006100020809     d WTTR            s                   like(FGVTTR)
006200050314     D ktrc            S                   LIKE(ar6trc)
006300050314     D fineksc         S              4
006400031216     D paramx          S                   LIKE(param)
006500070228     D*COMDFS          S              8  0 INZ
006600070228     D*COMHMS          S              4  0 INZ
006700020809     D COMCAA          S              3  0 INZ
006800020809     D COMNCD          S              8  0 INZ
006900020809     D COMNCA          S              8  0 INZ
007000020809     D W0140           S             14  0 INZ
007100020809     D DATEU           S              8  0 INZ
007200020809     D UDATE8          S              8  0 INZ
007300020809     D COD             S              2    INZ
007400020809     D KEY             S              8    INZ
007500020809     D MBR             S              3  0 INZ
007600020809     D XX              S              3  0 INZ
007700021008     D VAR11           S             11    INZ
007800021008     D VAR12           S             12    INZ
007900021008     D LUNG            S             15  5 INZ(55)
008000020809     D COMMAN          S             80    INZ
008100031210     d lnapks          s              1
008200070322     d lnageo          s              1
008300020809
008400020809     D KPJBA         E DS
008500020809     D DSCV          E DS
008600020809     D DSTV          E DS
008700020809     D DS7N          E DS
008800020809     D DS3A          E DS
008900020809     D DS7A          E DS
009000020809     D DSBL4A        E DS
009100070322     d dsbl4l        e ds
009200020809     D DSBL4J        E DS
009300020809     D DGEI          E DS
009400020809     D DGED          E DS
009500020809     D OG143         E DS
009600070322     d og148         e ds
009700031027     d Og150         e ds
009800020809     D YEURCODS      E DS
009900020809     D UT§DSE0F      E DS
010000020809     D CNCR80        E DS
010100020809     D FNLR33DS      E DS
010200020809     D FNLV55DS      E DS
010300020809     D TIBS02DS      E DS
010400070228     D fnls45ds      E DS
010500020809     d trul21ds      e ds
010600070123     D DSBRV         E DS                  EXTNAME(FNBRV00F)
010700020226
010800950118     D***
010900950118     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
011000950118     D***
011100950118     D WLBDAT          DS                  INZ
011200950118     D  G02DAT                 1      8  0
011300950118     D  G02INV                 9     16  0
011400950118     D  G02ERR                17     17
011500950118     D  G02TGI                18     22  0
011600950919     D* PARAMETRI PASSATI
011700950919     D PARAM           DS
011800950919     D  PARDTP                 1      8  0
011900950919     D  PARNFV                 9     13  0
012000960806     D  PARDEF                14     14
012100051006     D  tentativi            234    236
012200051006     D  LEGFIL               237    246
012300950919     D  MBRFIL               247    256
012400950919     D  ALFFIL               248    250
012500950919     D* PER ESPLOSIONE SPUNTE IN FILE DI RICEZIONE
012600950128     D PARAM5          DS
012700950128     D  PA5FIL               237    246    INZ('FXXX999999')
012800021015     D  PA5MBR4              247    250    INZ
012900021015     D  PA5MBR               247    256    Inz
013000020809     D                 DS
013100020809     D  BLPAAS                 1      4  0
013200020809     D  BLPMGS                 5      8  0
013300020809     D  BLPDSP                 1      8  0
013400020809     D                 DS
013500020809     D  WNUM                   1     13
013600020809     D  NUM                    1     13
013700020809     D                                     DIM(13)
013800020809     D                 DS
013900020809     D  WNUME                  1     14
014000020809     D  NUE                    1     14
014100020809     D                                     DIM(14)
014200990510     D FNANM1          DS
014300960424     D  ANMNR1               397    400B 0
014400990510     D FNANM2          DS
014500960424     D  ANMNR2               397    400B 0
014600000622     D                 DS
014700050804     D* DEFINIZIONE COSTANTI
014800050804     D CUTI            C                   CONST('EDP*      ')
014900011210
015000011210     d                sds
015100011210     d  nompgm                 1     10
015200020809
015300911003     C****************************************************************
015400911003     C*  RIEPILOGO INDICATORI
015500911003     C***************************************************************
015600911016     C* 02    - LACIO PGM ABILITAZIONE MERCE
015700911016     C* 03    - SONO ME STESSO NON SCRIVO CMRIC
015800920917     C* 04    - DETTAGLIO DOPPIO
015900050314     C* 06    - off - bolla non presente
016000921123     C* 07    - MANCA DETTAGLIO COLLI PER BOLLA CON COLLI NO SEQ
016100950905     C* 08    - DETTAGLIO DOPPIO - AGGIORNATO PERCHE' GIA' CONSEGNATO
016200940322     C* 10    - SPUNTA DAL MIO TERMINAL
016300940322     C* 18/19 - INDICATORI DI TIPO RECORD
016400911003     C* 20    - NON ALLOCATI DEGLI AGGETTI ESCO DA PROGRAMMA
016500911003     C* 31    - LETTURA FLBLPE00
016600911003     C* 32/33 - COMODO
016700990906     C* 34    - COMODO
016800960705     C* 35/36 - COMODO
016900911003     C*****************************************************************
017000000000     C     *ENTRY        PLIST
017100000000     C                   PARM                    KPJBA
017200950919     C                   MOVEL     KPJBU         PARAM
017300000000     C                   Z-ADD     1             CODUT
017400911001     C                   CALL      'X§PARUT'
017500020809     C                   PARM                    UT§DSE0F
017600911001     C                   MOVEL     REC80         CNCR80
017700920423     C                   MOVEL     RAGUT         RSUT             20
017800021015     c*
017900070228     C*    SIMFEL        CHAIN     AZORG01L                           30
018000070228     C* N30              MOVEL     ORGDE3        OG143
018100070228     C* N30              MOVEL     §OGAS         COMAS             3 0
018200070228     C*  30              MOVEL     SIMFEL        COMAS
018300070228     C*                  Z-ADD     SIMFEL        COMTFP            3 0
018400900618     C*---------------------------------------------------------------*
018500950118     C     KBLP2         KLIST
018600950118     C                   KFLD                    BLPAAS
018700950118     C                   KFLD                    BLPLNP
018800950118     C                   KFLD                    BLPNRS
018900950118     C                   KFLD                    BLPNSP
019000050314     C     Kar6          KLIST
019100050314     C                   KFLD                    BLPAAS
019200050314     C                   KFLD                    BLPLNP
019300050314     C                   KFLD                    BLPNRS
019400050314     C                   KFLD                    BLPNSP
019500050314     C                   KFLD                    ktrc
019600070831     C     Kart          KLIST
019700070831     C                   KFLD                    BLPAAS
019800070831     C                   KFLD                    BLPLNP
019900070831     C                   KFLD                    BLPNRS
020000070831     C                   KFLD                    BLPNSP
020100070831     C                   KFLD                    artnsc
020200911014     C     KANM          KLIST
020300911014     C                   KFLD                    ARTFLS
020400911014     C                   KFLD                    ARTLNA
020500911014     C                   KFLD                    ARTNRS
020600950119     C                   KFLD                    ARTNSC
020700020809     C                   KFLD                    COMCAA
020800961121     C     KEVS          KLIST
020900961121     C                   KFLD                    ARTFLS
021000961121     C                   KFLD                    ARTLNA
021100961121     C                   KFLD                    ARTNRS
021200961121     C                   KFLD                    ARTNSC
021300961121     C                   KFLD                    KCEV
021400960424     C     KANM1         KLIST
021500990510     C                   KFLD                    BLPAAS
021600960424     C                   KFLD                    BLPLNP
021700960424     C                   KFLD                    BLPNRS
021800960424     C                   KFLD                    BLPNSP
021900020809     C                   KFLD                    COMCAA
022000911014     C     KSPP          KLIST
022100950413     C                   KFLD                    ARTFLS
022200950413     C                   KFLD                    ARTLNA
022300950413     C                   KFLD                    ARTNRS
022400950119     C                   KFLD                    ARTNSC
022500951011     C     KBRV9         KLIST
022600951011     C                   KFLD                    KNPG
022700951011     C                   KFLD                    ARTFLS
022800951011     C                   KFLD                    ARTLNA
022900951011     C                   KFLD                    ARTNRS
023000951011     C                   KFLD                    ARTNSC
023100951011     C                   KFLD                    SIMFEL
023200911002     C     KTAB          KLIST
023300911002     C                   KFLD                    CODUT
023400020809     C                   KFLD                    COD
023500020809     C                   KFLD                    KEY
023600980623     C     KTAB2         KLIST
023700980623     C                   KFLD                    CODUT
023800020809     C                   KFLD                    COD
023900960529     C     KFVA          KLIST
024000960529     C                   KFLD                    BRVFGS
024100000110     C                   KFLD                    KNFV
024200000622     C     KAR4          KLIST
024300000622     C                   KFLD                    BLPAAS
024400000622     C                   KFLD                    BLPLNP
024500000622     C                   KFLD                    BLPNRS
024600000622     C                   KFLD                    BLPNSP
024700000622     C                   KFLD                    WTRC
024800020809     C     K02FGV01      klist
024900020809     C                   kfld                    KNFV
025000040317     C                   kfld                    kfgs
025100950119     C*---------------------------------------------------------------*
025200911011     C* GIRO DATA DEL GIORNO
025300020809     C                   TIME                    W0140
025400950118     C                   MOVE      W0140         G02DAT
025500911011     C                   MOVEL     *BLANK        G02ERR
025600950118     C                   CALL      'XSRDA8'
025700911011     C                   PARM                    WLBDAT
025800020809     C                   Z-ADD     G02INV        DATEU
025900020809     C                   Z-ADD     G02DAT        UDATE8
026000950214     C*
026100911003     C                   SETON                                        OF
026200040317     c                   eval      xx=1
026300911011     C*
026400911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
026500911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
026600911001     C     MBRFIL        IFEQ      *BLANK
026700021008     C                   MOVEL     '*FIRST'      MBRFIL
026800911001     C                   END
026900911001     C* MEMBRO LUNGO 3
027000020809     C                   MOVEL     ALFFIL        MBR
027100911107     C*
027200911107     C* IMPOSTAZIONI INIZIALI
027300911107     C                   EXSR      CARTAB
027400031027
027500911001     C*
027600911001     C* ALLOCO MEMBRI
027700911001     C                   EXSR      CAEX
027800911001     C   20              GOTO      FINE
027900911001     C*
028000950118     C                   READ      FNBLPE00                               31
028100911003     C*
028200931125     C     *IN31         DOWEQ     *OFF
028300921123     C                   SETOFF                                       040607
028400040317     C                   SETOFF                                       03
028500000622     C     BLPLNP        IFNE      SAVLNP
028600000622     C     BLPLNP        CHAIN     AZORG01L                           32
028700000622     C  N32              MOVEL     ORGDE3        OG143
028800000622     C   32              CLEAR                   OG143
028900020226     c                   eval      lnpntw = §ogntw
029000000622     C                   Z-ADD     BLPLNP        SAVLNP
029100000622     C                   ENDIF
029200031210     c* A cambio di lna devo memorizzare se è lna con picking attivo
029300041215     c* o lna per la quale stampare i DDT SI in arrivo
029400070322      * o se lna abilitata alle 'telefonate GEO'
029500031210     C     BLPLNA        IFNE      SAVLNA
029600060515     c                   clear                   lnapks
029700070322     c                   clear                   lnageo
029800031210     c* controllo se lna gestita da altri
029900031210     c                   clear                   fnlv55ds
030000031210     c                   movel     '6'           D55TPT
030100031210     c                   z-add     blplna        d55lin
030200031210     c                   z-add     dateu         d55drf
030300031210     c                   movel     'S'           wcal55
030400031210     c                   call      'FNLV55R'
030500031210     c                   parm                    fnlv55ds
030600060515      * se è un p.o. gestito da altri controllo se la lna stessa ha il picking attivo
030700060515     c                   if        d55lin <> d55tfa
030800060515     c     d55lin        chain     azorg01l                           32
030900060515     c  n32              movel     orgdf0        og150
031000070322     c  n32              eval      og148 = orgde8
031100060515     c   32              clear                   og150
031200070322     c   32              clear                   og148
031300120604     c**                 If        §OgPck = 'S' and §OgPkd <> 'S'
031400120604     c**                           or §ogsda = 'S'
031500120604     c**                 eval      lnapks = 'S'
031600120604     c**                 endif
031700070322      * controllo se LNA è abilitata a telefonate GEO
031800070322     c                   if        §oggeot = 'S'
031900070322     c                   eval      lnageo = 'S'
032000070322     c                   endif
032100060515     c                   endif
032200060515      * controllo se c'è il picking attivo
032300120604     c**                 if        lnapks = *blanks
032400120604     C**   d55tfa        CHAIN     AZORG01L                           32
032500120604     C**N32              MOVEL     ORGDf0        OG150
032600120604     C** 32              CLEAR                   OG150
032700120604if  5c**                 If        §OgPck = 'S' and §OgPkd <> 'S'
032800120604     c**                           or §ogsda = 'S'
032900120604     c**                 eval      lnapks = 'S'
033000120604     c**                 else
033100120604     c**                 clear                   lnapks
033200120604     c**                 endif
033300120604     c**                 endif
033400120604     c
033500070322      * se lna non abilitata a telefonate GEO controllo la sua capofila
033600070322     c                   if        lnageo = *blanks
033700070322     c     d55tfa        chain     azorg01l                           32
033800070322     c  n32              eval      og148 = orgde8
033900070322     c   32              clear                   og148
034000070322     c                   if        §oggeot = 'S'
034100070322     c                   eval      lnageo = 'S'
034200070322     c                   endif
034300070322     c                   endif
034400031210     C                   Z-ADD     BLPLNA        SAVLNA
034500031210     C                   ENDIF
034600040317     c
034700040317     c* Controllo bolla per bolla se si tratta di locale, non più in base
034800040317     c*  al membro ma in base ai terminal di partenza/arrivo
034900040330    1c                   if        blptfa=blptfp
035000040330    2c     blplna        ifne      blptfa
035100040330     C     blplna        CHAIN     AZORG01L                           32
035200040330     C  N32              MOVEL     ORGde3        OG143
035300040330     C   32              CLEAR                   OG143
035400040330     c                   z-add     §ogas         ogaslna           3 0
035500040330     C     blptfa        CHAIN     AZORG01L                           32
035600040330     C  N32              MOVEL     ORGDe3        OG143
035700040330     C   32              CLEAR                   OG143
035800040330    3c     ogaslna       ifeq      §ogas
035900040330     c                   seton                                        03
036000040330    3c                   endif
036100040330   x2c                   else
036200040317     c                   seton                                        03
036300040330    2c                   endif
036400040317     c                   endif
036500000713     C*
036600950118     C     KBLP2         SETLL     FNARB01L                               06
036700060220     c
036800000622     C     *IN06         IFEQ      *ON
036900060220     c
037000060220     C**   §B4ASO        ANDNE     '&'
037100060220     C**   *IN06         OREQ      *ON
037200060220     C**   BL2CEI        ANDNE     '&'
037300000622     C* 06 ON E BOLLA NON AUTOGENERATA - ERRORE GIA' ESISTENTE
037400060220     c* 20/02/06 : tolgo il discorso delle autogenerate perchè valeva solo
037500060220     c*            per Poste; se bolle già presente sempre errore
037600000622     C                   EXSR      STAERR
037700000622     C                   ELSE
037800000622     C* 06 OFF OPPURE ON MA BOLLA AUTOGENERATA: CREO O AGGIORNO BOLLA
037900060220     C** 06KBLP2         CHAIN     FNARB01L                           32
038000000622     C                   EXSR      CRTBOL
038100040317     c
038200040317     c* Memorizzo il numero foglio se bolla locale per la sottomissione
038300040317     c                   if        *in03
038400040317     c     blpnfv        lookup    nfv                                    36
038500040317     c                   if        not *in36
038600040317     c                   z-add     blpnfv        nfv(xx)
038700040317     c                   add       1             xx                3 0
038800040317     c                   endif
038900040317     c                   endif
039000040317     c
039100000622     C                   ENDIF
039200060220     c
039300950118     C                   READ      FNBLPE00                               31
039400911001     C                   END
039500031203     c* richiamo fnlr15r per la chiusura file e la scrittura dell'ultim
039600031203     c* agb
039700031203     c                   movel     'C'           flg15
039800031203     c                   call      'FNLR15R'
039900031203     c                   parm                    kpjba
040000031203     c                   parm                    dsbrv
040100031203     c                   parm                    flg15
040200900911     C*
040300040428     c                   setoff                                       2022
040400900409     C     FINE          TAG
040500040428     c*
040600040428     c* Per 20 on - risottometto ricezione bolle e spunte
040700040428     c                   if        *in20  and not *in22
040800050804     c
040900051006     c                   testn                   tentativi            30
041000051006     c                   move      tentativi     w001a             1
041100051006     c                   if        not *in30 or w001a<'0'
041200051006     c                   eval      tentativi='001'
041300051006     c                   eval      w0030=1
041400051006     c                   else
041500051006     c                   movel     tentativi     w0030             3 0
041600051006     c                   add       1             w0030
041700051006     c                   movel     w0030         tentativi
041800051006     c                   endif
041900051006     c
042000051006     c* max 200 tentativi
042100051006     c                   if        w0030<=200
042200050804     C                   MOVEL     'K'           legfil
042300050804     C                   MOVEl(P)  PARAM         KPJBU
042400040428     c
042500040428     C                   MOVEL     'LR14'        KCOAZ
042600040428     C                   CALL      'BCH10'
042700040428     C                   PARM                    KPJBA
042800040428     c
042900040428     C                   MOVEL     'LR22'        KCOAZ
043000040428     C                   CALL      'BCH10'
043100040428     C                   PARM                    KPJBA
043200040428     C                   SETON                                        98
043300051006     c                   else
043400051006     c
043500051006     c* msg x  pgm non eseguito e da chi allocato
043600051006     c
043700050804     c* richiamo pgm per rifare alcobj
043800051006     c                   call      'FNLR14C1'
043900080407     c                   parm                    fisico           10
044000051006     c                   parm                    mbrfil
044100051006     c
044200050804     c* eseguo dspjoblog per vedere chi mi alloca
044300051006     c                   Movel     CmdDSPLOG(1)  Comman
044400051006     c                   Call      'QCMDEXC'
044500051006     c                   Parm                    Comman
044600051006     c                   Parm                    Lung
044700050804     C
044800111104     c                   clear                   msgtes
044900111104     c                   clear                   msgdet
045000111104     c                   eval      msgtes='RICEZIONE BOLLE: FNLR14R '
045100111104     c                   eval      msgdet='FILE ALLO'+
045200111104     C                             'CATO. VEDI DSPJOBLOG   -   utente:'+knmus
045300050804     C
045400120305     C                   eval      wrkEml= 'ced@brt.it'
045500111104     C                   call(e)   'TIS701C'
045600111104     C                   parm                    wrkEml          253
045700111104     C                   PARM                    MSGTES           44
045800111104     C                   PARM                    MSGDET         5000
045900050804     C*
046000051006     c                   endif
046100050804     c
046200040428    2C                   ENDIF
046300040317     c*
046400040317     c* sottometto lr15 del foglio passato solo se è locale
046500040317     c                   if        nfv(1)>0
046600040317     c     parnfv        lookup    nfv                                    36
046700040317     c                   if        *in36 and pardef<>'S'
046800040317     c                   clear                   kpjbu
046900040317     C                   MOVEL     PARAM         KPJBU
047000040317     C                   CALL      'FNLR15R'
047100040317     c                   PARM                    KPJBA
047200040317     c                   endif
047300040317     c                   endif
047400950919     C*
047500950128     C** LANCIO PGM DI ABILITAZIONE BOLLE ALLA CONSEGNA
047600970509     C     *IN02         IFEQ      *ON
047700970509     C* CHIUDO PGM FNLS44R PER FARE SCRIVERE L'ULTIMO FNAGB
047800070228     C                   MOVEL     'C'           dls45flg
047900070228     C                   CALL      'FNLS45R'
048000070228     c                   parm                    fnls45ds
048100070228     c                   parm                    dsbrv
048200970509     C                   ENDIF
048300971114     C**
048400971114     C     WCAL55        IFEQ      'S'
048500020809     C                   CLEAR                   FNLV55DS
048600971114     C                   MOVEL     'C'           D55TLA
048700971114     C                   CALL      'FNLV55R'
048800020809     C                   PARM                    FNLV55DS
048900971114     C                   ENDIF
049000911016     C*
049100020809     C                   EVAL      *INLR = *ON
049200910322     C*
049300911107     C*--- CARICO TABELLE --------------------------------------------*
049400911107     C     CARTAB        BEGSR
049500921022     C*
049600921022     C* PRENDO TABELLA VARIABILI ARRIVI
049700921022     C                   MOVEL     '7A'          COD
049800921022     C                   MOVEL     *BLANKS       KEY
049900921022     C                   MOVEL     '1'           KEY
050000921022     C     KTAB          CHAIN     TABEL                              33
050100921022     C  N33              MOVEL     TBLUNI        DS7A
050200950130     C   33              CLEAR                   DS7A
050300010110     C* RICERCO FASE
050400921022     C                   MOVEL     '7N'          COD
050500960424     C                   MOVEL(P)  2             KEY
050600921022     C     KTAB          CHAIN     TABEL                              31
050700921022     C  N31              MOVEL     TBLUNI        DS7N
050800980623     C* CARICO TIPI TRAINO DEFLUENZA
050900980623     C                   MOVEL     'TV'          COD
051000020809     C                   CLEAR                   XX
051100980623     C     KTAB2         SETLL     TABEL
051200980623     C     KTAB2         READE     TABEL                                  31
051300980623     C     *IN31         DOWEQ     *OFF
051400980623     C     TBLFLG        IFEQ      ' '
051500980623     C                   MOVEL     TBLUNI        DSTV
051600980623     C     §TVDEF        IFEQ      'S'
051700020809     C                   ADD       1             XX
051800020809     C                   MOVEL     TBLKEY        TDF(XX)
051900980623     C                   ENDIF
052000980623     C                   ENDIF
052100980623     C**
052200980623     C     KTAB2         READE     TABEL                                  31
052300980623     C                   ENDDO
052400000622     C*
052500000622     C* CARICO IN SCHIERA ELENCO DIVISE ACCETTATE IN ITALIA
052600000622     C*
052700000622     C                   MOVEL     'CV'          COD
052800000622     C                   CLEAR                   DVA
052900011025     C                   CLEAR                   DEC
053000020809     C                   CLEAR                   XX
053100000622     C     KTAB2         SETLL     TABEL
053200000622     C     KTAB2         READE     TABEL                                  31
053300000622     C     *IN31         DOWEQ     *OFF
053400000622     C     TBLFLG        IFEQ      ' '
053500000622     C                   MOVEL     TBLUNI        DSCV
053600000622     C     §CVDVA        IFEQ      'S'
053700020809     C                   ADD       1             XX
053800020809     C                   MOVEL     TBLKEY        DVA(XX)
053900011025      * Carico in schiera il numero d decimali della divisa
054000020809     C                   Z-ADD     §CVDEC        DEC(XX)
054100000622     C                   ENDIF
054200000622     C                   ENDIF
054300000622     C**
054400000622     C     KTAB2         READE     TABEL                                  31
054500000622     C                   ENDDO
054600011029      *
054700020809     C                   CLEAR                   TIBS02DS
054800011029     C                   CLEAR                   KPJBU
054900011029     C                   MOVE      'C'           T02MOD
055000011029     C                   MOVE      KNSIF         T02SIF
055100011029     C                   MOVE      'GED'         T02COD
055200011029     C                   MOVEL(P)  1             T02KE1
055300011029     C                   CALL      'TIBS02R'
055400011029     C                   PARM                    KPJBA
055500020809     C                   PARM                    TIBS02DS
055600011029    6C     T02ERR        IFEQ      *BLANKS
055700011029     C                   MOVEL     T02UNI        DGED
055800011029     C                   ELSE
055900011029     C                   CLEAR                   DGED
056000011029    6C                   ENDIF
056100011029      *
056200011029      * Recupero il numero di decimali di §GEDCN
056300011029     C                   MOVEL     'CV'          TBLCOD
056400011029     C                   MOVEL     *BLANKS       TBLKEY
056500011029     C                   MOVEL     §GEDCN        TBLKEY
056600011029     C     KTAB          CHAIN     TABEL                              31
056700011029     C  N31              MOVEL     TBLUNI        DSCV
056800011029     C   31              CLEAR                   DSCV
056900011030      * IMPORTI VARI
057000020809     C                   CLEAR                   TIBS02DS
057100011030     C                   CLEAR                   KPJBU
057200011030     C                   MOVE      'C'           T02MOD                         CONTROLLO
057300011030     C                   MOVE      KNSIF         T02SIF                         S.I.
057400011030     C                   MOVE      'GEI'         T02COD                         TABELLA PROFILI
057500011030     C                   MOVEL(P)  §GEDCN        T02KE1                         PROFILO IN K1
057600011030     C                   CALL      'TIBS02R'
057700011030     C                   PARM                    KPJBA
057800020809     C                   PARM                    TIBS02DS
057900011030    6C     T02ERR        IFEQ      *BLANKS
058000011030     C                   MOVEL     T02UNI        DGEI
058100011030     C                   ELSE
058200011030     C                   CLEAR                   DGEI
058300011030    6C                   ENDIF
058400911107     C                   ENDSR
058500911001     C**---------------------------------------------------------
058600911001     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
058700911001     C**---------------------------------------------------------
058800911001     C     CAEX          BEGSR
058900040428     c                   setoff                                       22
059000020809     C                   MOVE      *BLANK        VAR11
059100020809     C                   MOVE      *BLANK        VAR12
059200911001     C                   MOVE      ')'           VAR11
059300021008     C                   MOVEL     MBRfil        VAR11
059400911001     C                   MOVE      '))'          VAR12
059500021008     C                   MOVEL     MBRfil        VAR12
059600021008     C                   Z-ADD     55            LUNG
059700911001     C*
059800911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
059900911001     C                   SETOFF                                       20
060000950118     C                   MOVE      VAR11         CMD5                            FNBLPE0R
060100911001     C                   MOVEL     *BLANKS       COMMAN
060200020809     C                   MOVEA     CMD5(1)       COMMAN
060300911001     C                   CALL      'QCMDEXC'                            20
060400911001     C                   PARM                    COMMAN
060500911001     C                   PARM                    LUNG
060600040428     c*
060700040428     C* 20 ON - ESCO
060800040428     c                   if        *in20
060900040428     c                   seton                                          22
061000040428     C                   GOTO      ENDCAE
061100040428     c                   endif
061200911002     C* ALLOCO MEMBRO: SE NON RIESCO ESCO DA PROGRAMMA
061300911001     C                   SETOFF                                       20
061400911002     C                   MOVE      VAR12         CMD6
061500911001     C                   MOVEL     *BLANKS       COMMAN
061600911002     C                   MOVEA     CMD6(1)       COMMAN
061700911001     C                   CALL      'QCMDEXC'                            20
061800911001     C                   PARM                    COMMAN
061900911001     C                   PARM                    LUNG
062000911002     C*
062100050804     c                   if        *in20
062200050804     c                   eval      fisico='FNBLPE0R'
062300050804     C                   GOTO      ENDCAE
062400050804     c                   endif
062500911001     C* OVRDBF
062600911002     C                   SETOFF                                       20
062700911002     C                   MOVE      VAR11         CMD9
062800911001     C                   MOVEL     *BLANKS       COMMAN
062900911002     C                   MOVEA     CMD9(1)       COMMAN
063000911002     C                   CALL      'QCMDEXC'                            20
063100911001     C                   PARM                    COMMAN
063200911001     C                   PARM                    LUNG
063300050804     C   20              GOTO      ENDCAE
063400911002     C*
063500950118     C                   OPEN      FNBLPE0R
063600911001     C*
063700911002     C     ENDCAE        ENDSR
063800911002     C**---------------------------------------------------------
063900911002     C** CREO BOLLA ARRIVI
064000911002     C**---------------------------------------------------------
064100980311     C     CRTBOL        BEGSR
064200080407     c*
064300931125     C* S C R I V O   B O L L A
064400990906     C                   EXSR      RIEARB
064500000622     C* LE SEGUENTI SPECIFICHE LE ESEGUO SOLO SE BOLLA NON PRESENTE
064600000622   1AC     *IN06         IFEQ      *OFF
064700911003     C*
064800931125     C* D E T T A G L I O   C O L L I
064900950413     C*
065000990906     C                   EXSR      RIEBLT
065100931125     C*
065200080407     C* Leggo il numero dei segnacolli dal dettaglio bolle partenza
065300080407     C     KBLP2         SETLL     FNBLT01l                               32
065400921123     C*
065500921123     C* 32 OFF-  NON TROVATO IL DETTAGLIO COLLI: STAMPO ERRORE
065600950118    2C     *IN32         IFEQ      *OFF
065700080411     c     §3arbl        andne     'R'
065800090109     c     §3aurc        andne     'S'
065900921123     C                   SETON                                        07
066000921123     C                   EXSR      STAERR
066100950118   X2C                   ELSE
066200921123     C*
066300080407     C     KBLP2         READE     FNBLT01l                               32
066400990906     C*
066500950118    3C     *IN32         DOWEQ     *OFF
066600100414     C                   Z-ADD     BLTfls        ARTfls
066700100414     C                   Z-ADD     BLTNSC        ARTNSC
066800080407     c                   exsr      PesoeVol
066900931125     C*
067000920917     C* CONTROLLO SE SEGNACOLLO GIA' ESISTENTE
067100950905     C                   SETOFF                                       0408
067200950413     C     KSPP          CHAIN     FNART27L                           31
067300080407    4C     *IN31         IFEQ      *OFF
067400950413     C                   SETON                                        04
067500080407    5C     ARTDCM        IFNE      *ZEROS
067600950905     C                   SETON                                        08
067700950413     C                   EXSR      RIEBLT
067800100414     C                   Z-ADD     BLTfls        ARTfls
067900950418     C                   Z-ADD     BLTNSC        ARTNSC
068000080407     c                   exsr      PesoeVol
068100950413     C* Segnacollo già esistente: se già consegnato aggiorno il record
068200950413     C                   UPDATE    FNART000
068300040317     c   03              exsr      ctrdet
068400040317     c
068500080407   x5C                   ELSE
068600040317     c
068700950418     C                   EXSR      RIEBLT
068800100414     C                   Z-ADD     BLTfls        ARTfls
068900950418     C                   Z-ADD     BLTNSC        ARTNSC
069000950413     C                   UNLOCK    FNART27L
069100080407    5C                   END
069200950413     C* Stampo errore
069300950413     C                   EXSR      STAERR
069400080407   x4C                   ELSE
069500980311     C*
069600951011     C                   WRITE     FNART000
069700031202     c   03              exsr      ctrdet
069800080407    4C                   END
069900911014     C*
070000080407     C     KBLP2         READE     FNBLT01l                               32
070100950118    3C                   ENDDO
070200950118    2C                   ENDIF
070300911003     C*
070400000622   1AC                   ENDIF
070500911003     C* SCRIVO FILE
070600000622     C  N06              WRITE     FNARB000
070700000622     C   06              UPDATE    FNARB000
070800080407     C*
070900911003     C* DELETO
071000000622     C     *IN06         IFEQ      *OFF
071100950118     C                   DELETE    FNBLPE00
071200000622     C                   ELSE
071300000622     C                   EXSR      DLTRIC
071400000622     C                   ENDIF
071500960424     C**
071600960424     C* CANCELLO EVENTUALE ANOMALIA 12
071700960424     C                   Z-ADD     12            COMCAA
071800990510     C     KANM1         CHAIN     FNANM01L                           33
071900960424     C* CHIUDO ANOMALIA
072000960424    1C     *IN33         IFEQ      *OFF
072100020809     C                   CLEAR                   FNLR33DS
072200960424     C                   Z-ADD     ANMNR1        D33NRR
072300960424     C                   MOVE      DATEU         D33DCH
072400960424     C                   MOVEL     §7NFS1        D33FSC
072500960424     C                   CALL      'FNLR33R'
072600020809     C                   PARM                    FNLR33DS
072700960424     C                   ENDIF
072800921022     C*
072900000622     C     *IN06         IFEQ      *OFF
073000000111     C     BLPLNA        CHAIN     AZORG01L                           32
073100020218     C  N32              MOVEL     ORGDE3        OG143
073200020218     C   32              CLEAR                   OG143
073300020226     c                   eval      lnantw = §ogntw
073400921022     C* CONTROLLO SE C'ERANO ANOMALIE MANCA BOLLA
073500960827     C*           E ANOMALIE 050-COLLO ARR. NON PART. SU BOLLE LOCALI
073600950123     C     KBLP2         SETLL     FNART001
073700020226     C     lnaNTW        IFNE      'EEX'
073800020226     C     lnaNTW        ANDNE     'EUP'
073900020226     C     lnaNTW        ANDNE     'FED'
074000060322     C     lnaNTW        ANDNE     'DPD'
074100961121     C     KBLP2         READE(N)  FNART001                               32
074200961121     C                   ELSE
074300961121     C     KBLP2         READE     FNART001                               32
074400961121     C                   ENDIF
074500921022     C*
074600961121     C     *IN32         DOWEQ     *OFF
074700950128     C                   EXSR      CTRANM
074800960827     C* . . . . . BOLLA LOCALE , SERIE>0 , FLS *NE LNP
074900960827     C     *IN03         IFEQ      *ON
075000960827     C     ARTNRS        ANDGT     *ZEROS
075100960827     C     ARTFLS        ANDNE     ARTLNP
075200960827     C                   EXSR      ANM050
075300960827     C                   END
075400961121     C**
075500000111     C* SE LNA ESTERA POSSO AGGIORNARE IL RECORD
075600020226     C     lnaNTW        IFNE      'EEX'
075700020226     C     lnaNTW        ANDNE     'EUP'
075800020226     C     lnaNTW        ANDNE     'FED'
075900060322     C     lnaNTW        ANDNE     'DPD'
076000961121     C     KBLP2         READE(N)  FNART001                               32
076100961121     C                   ELSE
076200961121     C     KBLP2         READE     FNART001                               32
076300921022     C                   END
076400961121     C**
076500961121     C                   ENDDO
076600000622     C                   ENDIF
076700911002     C*
076800911002     C                   ENDSR
076900080407     C**---------------------------------------------------------
077000080407     c* imposto peso e volume della partenza sul dettaglio colli
077100080407     C**---------------------------------------------------------
077200080407     c     PesoeVol      BEGSR
077300080407     c                   z-add     bltvuc        artvuc
077400080407     c                   movel     bltfvc        artfvc
077500080407     c                   z-add     bltpuc        artpuc
077600080407     c                   movel     bltfpc        artfpc
077700080407     c                   select
077800080407     c                   when      artvuc>0 and artpuc>0
077900080407     c                   eval      arbfbs='E'
078000080423     c                   when      artvuc>0   and arbfbs=' '
078100080407     c                   eval      arbfbs='V'
078200080423     c                   when      artpuc>0   and arbfbs=' '
078300080407     c                   eval      arbfbs='P'
078400080423     c                   other
078500080423     c                   eval      arbfbs='E'
078600080407     c                   endsl
078700080407     c                   ENDSR
078800911002     C**---------------------------------------------------------
078900911002     C** RIEMPO CAMPI FLARB00F
079000911002     C**---------------------------------------------------------
079100911002     C     RIEARB        BEGSR
079200000622     C* DEVO CLEARARE FORMATO FNARB SOLO SE BOLLA NON PRESENTE
079300000622     C  N06              CLEAR                   FNARB000
079400931125     C*
079500950118     C     BLPCBO        IFNE      SAVCBO
079600920603     C                   MOVEL     '3A'          COD
079700920603     C                   MOVEL     *BLANKS       KEY
079800950118     C                   MOVEL     BLPCBO        KEY
079900920603     C     KTAB          CHAIN     TABEL                              33
080000920603     C  N33              MOVEL     TBLUNI        DS3A
080100920603     C   33              MOVEL     'F1'          §3ATB1
080200920625     C   33              MOVEL     '  '          §3ATB2
080300950118     C                   MOVEL     BLPCBO        SAVCBO            2
080400950118     C                   ENDIF
080500920603     C*
080600911002     C                   MOVEL     BLPLNP        ARBLNP
080700950118     C                   MOVE      BLPAAS        ARBAAS
080800911002     C                   MOVEL     BLPMGS        ARBMGS
080900911002     C                   MOVEL     BLPNRS        ARBNRS
081000911002     C                   MOVEL     BLPNSP        ARBNSP
081100010110     C* L'IMPOSTAZIONE DEL CODICE BOLLA SE STO RICEVENDO UN AGGIORNAMEN
081200010110     C* TO DI BOLLA AUTOGENERATA LA FACCIO QUANDO GESTISCO IL CONTRAS-
081300010110     C* SEGNO
081400010110     C  N06              MOVEL     BLPCBO        ARBCBO
081500950118     C                   MOVEL     BLPFDN        ARBFDN
081600911002     C                   MOVEL     BLPFNS        ARBFNS
081700911002     C                   MOVEL     BLPLNA        ARBLNA
081800911002     C                   MOVEL     BLPKSC        ARBKSC
081900911002     C                   MOVEL     BLPZNC        ARBZNC
082000911002     C                   MOVEL     BLPRSM        ARBRSM
082100911002     C                   MOVEL     BLPINM        ARBINM
082200911002     C                   MOVEL     BLPLOM        ARBLOM
082300911002     C                   MOVEL     BLPPRM        ARBPRM
082400950118     C                   MOVEL     BLPCAM        ARBCAM
082500950118     C                   MOVEL     BLPNZM        ARBNZM
082600911002     C                   MOVEL     BLPRSD        ARBRSD
082700911003     C                   MOVEL     BLPIND        ARBIND
082800911002     C                   MOVEL     BLPLOD        ARBLOD
082900911002     C                   MOVEL     BLPPRD        ARBPRD
083000950118     C                   MOVEL     BLPCAD        ARBCAD
083100950118     C                   MOVEL     BLPNZD        ARBNZD
083200911002     C                   MOVEL     BLPGC1        ARBGC1
083300911002     C                   MOVEL     BLPGC2        ARBGC2
083400911002     C                   MOVEL     BLPCTR        ARBCTR
083500911002     C                   MOVEL     BLPCTS        ARBCTS
083600911002     C                   MOVEL     BLPFTM        ARBFTM
083700950118     C                   MOVEL     BLPFIN        ARBFIN
083800911002     C                   MOVEL     BLPFAP        ARBFAP
083900911002     C                   MOVEL     BLPTSP        ARBTSP
084000950118     C                   Z-ADD     BLPIAS        ARBIAS
084100950123     C                   MOVEL     BLPVAS        ARBVAS
084200911002     C                   MOVEL     BLPNAS        ARBNAS
084300911002     C                   MOVEL     BLPNCL        ARBNCL
084400950118     C                   Z-ADD     BLPPKB        ARBPKB
084500950118     C                   Z-ADD     BLPPKF        ARBPKF
084600000622     C* SE BOLLA GIA' PRESENTE SOSTITUISCO SOLO SE IL VOLUME SU ARB
084700000622     C* NON E' NE' "T" NE' "Z" E SE IL VOLUME RICEVUTO E' > DI QUELLO
084800000622     C* SU ARB
084900000622     C     *IN06         IFEQ      *OFF
085000000622     C     ARBFVF        ORNE      'T'
085100000622     C     ARBFVF        ANDNE     'Z'
085200000622     C     BLPVLF        ANDGT     ARBVLF
085300950118     C                   Z-ADD     BLPVLF        ARBVLF
085400950118     C                   MOVEL     BLPFVF        ARBFVF
085500000622     C                   ENDIF
085600950118     C                   Z-ADD     BLPVLB        ARBVLB
085700950118     C                   MOVEL     BLPFVB        ARBFVB
085800950118     C                   MOVEL     BLPQFT        ARBQFT
085900950118     C                   MOVEL     BLPVMD        ARBVMD
086000950118     C                   MOVEL     BLPVAD        ARBVAD
086100070222     C*
086200911002     C                   MOVEL     BLPCCM        ARBCCM
086300950118     C                   Z-ADD     BLPRMN        ARBRMN
086400950118     C                   MOVEL     BLPRMA        ARBRMA
086500911002     C                   MOVEL     BLPRMO        ARBRMO
086600911002     C                   MOVEL     BLPFLS        ARBFLS
086700911002     C                   MOVEL     BLPNCD        ARBNCD
086800911002     C                   MOVEL     BLPNCA        ARBNCA
086900911002     C                   MOVEL     BLPXCO        ARBXCO
087000950118     C*
087100911002     C                   MOVEL     BLPFFD        ARBFFD
087200950118     C                   MOVE      BLPDCR        ARBDCR
087300911002     C                   MOVEL     BLPHCR        ARBHCR
087400911002     C                   MOVEL     BLPTCR        ARBTCR
087500950118     C                   MOVE      BLPDBR        ARBDBR
087600931215     C                   MOVEL     BLPNFV        ARBNFV
087700950124     C*
087800911002     C                   MOVEL     BLPFLE        ARBFLE
087900911002     C                   MOVEL     BLPTFA        ARBTFA
088000911002     C                   MOVEL     BLPTFP        ARBTFP
088100911002     C                   MOVEL     BLPFEA        ARBFEA
088200911008     C                   MOVEL     BLPFBR        ARBFBR
088300950118     C                   MOVEL     BLPTC1        ARBTC1
088400950118     C                   MOVEL     BLPTC2        ARBTC2
088500950118     C                   MOVEL     BLPGVA        ARBGVA
088600950118     C                   MOVEL     BLPGMA        ARBGMA
088700950118     C                   MOVEL     BLPGGA        ARBGGA
088800001204     C*  SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA N FT1
088900051116     C* BLANK NON LO DEVO AGGIORNARE ALTRIMENTI NON VENGONO TRASMESSE
089000001204     C* LE DATE DI CONSEGNA
089100001204     C     *IN06         IFEQ      *OFF
089200001204     C     ARBFT1        ORNE      *BLANKS
089300931125     C                   MOVEL     'R'           ARBFT1
089400950118     C                   Z-ADD     DATEU         ARBDT1
089500001204     C                   ENDIF
089600970212     C*
089700060220     c* Imposto nel tipo record "A" (se non c'e' lo creo,)
089800060220     C*   IL 1° BYTE DEL CAMPO AR4NOT  E §B4ASO=SIGLA OPERATORE
089900060302     c                   clear                   contaerror
090000060220     c                   eval      wtrc='A'
090100060302     c
090200060302     c* Se errore in write (succede per conflitto con fnlrc1r)
090300060302     c*  ritento l'update
090400060302    1c                   dou       not *in99
090500060302     c                   setoff                                       99
090600060220     c     kar4          chain     fiar401l
090700060302     c                   exsr      Scriviar4A
090800060302     c
090900060302     c* dopo 10 tentativi mando un messaggio in posta ed esco senza
091000060302     c*  impostare il record A
091100060302     c                   add       1             contaerror        2 0
091200060302    2c                   if        contaerror>10
091300060302     C
091400111104     c* Mando e_mail
091500111104     c                   clear                   msgtes
091600111104     c                   clear                   msgdet
091700111104     c                   eval      msgtes='RICEZIONE BOLLE: FNLR14R'
091800111104     c                   eval      msgdet='Non scritto record"A"   -  utente'+
091900111104     c                               KNMUS + '   bolla :' +
092000111104     c                               (%editc(arbaas:'Z'))
092100060302     c                             + (%editc(arblnp:'Z')) + (%editc(arbnrs:'2'))
092200060302     c                             + (%editc(arbnsp:'2'))
092300120305     C                   eval      wrkEml= 'ced@brt.it'
092400111104     C                   call(e)   'TIS701C'
092500111104     C                   parm                    wrkEml          253
092600111104     C                   PARM                    MSGTES           44
092700111104     C                   PARM                    MSGDET         5000
092800060302     C*
092900060302     c                   leave
093000060302    2c                   endif
093100060302    1c                   enddo
093200060302     c
093300060220     c
093400060220     c* Cerco partita iva da impostare su fnarb
093500060220     c                   eval      wtrc='P'
093600060220     c     kar4          chain(N)  fiar401l
093700060220     c                   if        %found(fiar401l)
093800060220     C                   MOVEL     §3ATB1        UNOTB1            1
093900060220     C     UNOTB1        IFEQ      'F'
094000060220     C     §3ATB2        ANDEQ     *BLANKS
094100060220     C                   MOVEL     AR4NOT        ARBCPI
094200060220     C                   ELSE
094300060220     C                   MOVE      AR4NOT        ARBCPI
094400060220     C                   ENDif
094500061031
094600061031     c                   else
094700061031      * se non c'è il fiar4 rcd P aggancio la bolla partenza se bolla unica
094800061031     c                   if        §3atb2 = *blanks
094900061031     c     kblp2         chain     fnblp01l
095000061031     c                   if        %found(fnblp01l)
095100061031     c                   eval      arbcpi = w_blpcpi
095200061031     c                   endif
095300061031     c                   endif
095400061031
095500060220     C                   ENDif
095600030109      *
095700930201     C* A B I L I T A Z I O N E   M E R C E
095800931125     C**
095900000622     C*
096000930201     C* SE FOGLIO VIAGGIO 99999 ABILITO SUBITO
096100000623     C  N06BLPNFV        IFEQ      99999
096200930201     C                   MOVEL     3             ARBAMA
096300950919     C                   ENDIF
096400931125     C**
096500931125     C* A B I L I T A Z I O N E   C / A S S E G N O
096600000623     C*
096700911002     C* PER BOLLE SENZA CONTRASSEGNO
096800051116     c* Vedo se essite C/A per abilitazione
096900051116     c     kblp2         chain     fiar901l
097000051116     c                   if        not %found(fiar901l) or ar9cas=0
097100911002     C                   MOVEL     'S'           ARBACC
097200950119   X1C                   ELSE
097300011211
097400011210      * ------------------
097500011210
097600011210      * NUOVO CONTROLLO C/ASSEGNO
097700011210     c                   clear                   trul21ds
097800011210     c                   eval      i21tla = 'L'
097900011210     c                   eval      i21cbo = blpcbo
098000011210     c                   eval      i21tsp = blptsp
098100011210     c                   eval      i21lnp = blplnp
098200011210     c                   eval      i21nzm = blpnzm
098300011210     c                   eval      i21lna = blplna
098400011210     c                   eval      i21nzd = blpnzd
098500051116     c                   eval      i21tic = ar9tic
098600051116     c                   eval      i21imp = ar9cas
098700051116     c                   eval      i21div = ar9vca
098800011210     c                   eval      i21ute = knmus
098900011210     c                   eval      i21pgm = nompgm
098901130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
098903130827     c                   if        %subst(§3atb1:1:1)='F'
098904130827     c                   z-add     blpksc        i21ksc
098905130827     c                   else
098906130827     c                   z-add     blpccm        i21ksc
098907130827     c                   if        i21ksc=0
098908130827     c                   z-add     8888          i21ksc
098909130827     c                   movel     blplnp        i21ksc
098910130827     c                   endif
098911130827     c                   endif
098912130827     c
099000011210     c                   call      'TRUL21R'
099100011210     c                   parm                    trul21ds
099200011210
099300011210     c                   if        o21fca = *blanks
099400011210     c                   eval      arbacc = 'S'
099500011210     c                   endif
099600911002     C*
099700950119    1C                   END
099800931125     C**
099900950119     C* A B I L I T A Z I O N E    A S S E G N A T O
100000931125     C**
100100000623   1AC     *IN06         IFEQ      *OFF
100200000623     C*
100300911003     C                   MOVEL     §3ATB1        UNOTB1            1
100400911003     C* FRANCO
100500950119    1C     UNOTB1        IFEQ      'F'
100600930112     C* SE NON C'E' SECONDA BOLLA ABILITO
100700950119    2C     §3ATB2        IFEQ      '  '
100800911003     C                   MOVEL     'S'           ARBACA
100900950119    2C                   END
101000950119    1C                   END
101100050314     c
101200050314     c* Se già tassata, abilito subito l'assegnato
101300050314     c                   if        arbaca=' '
101400050314     c                   if        §3atb2<>*blanks
101500050314     c                   eval      ktrc='2'
101600050314     c                   else
101700050314     c                   eval      ktrc='1'
101800050314     c                   endif
101900050314     c
102000050314     c     kar6          chain     fiar601l
102100050314     c                   if        %found(fiar601l)
102200050314     c                   move      ar6ksc        fineksc
102300050314     c                   if        fineksc<>'9999' and fineksc<>'0000'
102400050314     c                   eval      arbaca='S'
102500050314     c* mi salvo i campi da inmpostare in arbksc e arbctr
102600050314     c                   eval      arbksc=ar6ksc
102700050314     c                   eval      arbctr=ar6ctr
102800050331     c* Se si tratta di bolla di recupero, devo anche chiudere la spedizione
102900050331    3C     §3ATB1        IFEQ      'A3'
103000050331     C* IMPOSTO I CAMPI DELLA CHIUSURA
103100050331     C                   MOVEL     'Y'           ARBICC
103200050331     C                   MOVEL     'S'           ARBICA
103300050331     C                   Z-ADD     DATEU         ARBDCM
103400050331     C                   time                    wora              6 0
103500050331     C                   movel     wora          ARBHMC
103600050331     C                   CLEAR                   ARBFT1
103700050331     C                   CLEAR                   ARBDT1
103800050331     C                   MOVEL     *ALL'9'       ARBNDC
103900050331     C                   Z-ADD     DATEU         ARBDDC
104000050331     C                   MOVEL     'S'           ARBFDC
104100050331     C                   MOVEL     *ALL'9'       ARBNMI
104200050331     C                   Z-ADD     DATEU         ARBDMI
104300050331     C                   MOVEL     *ALL'9'       ARBCUC
104400050331     C                   CLEAR                   ARBFBC
104500050331    3C                   ENDIF
104600050314     c                   endif
104700050314     c                   endif
104800050314     c                   endif
104900050314     c
105000000623   1AC                   ENDIF
105100070322
105200070322      * se LNA o sua capofila abilitata alle telefonate per GEO
105300070329      *  e NO bolle di recupero
105400070329     c                   if        lnageo = 'S' and §3arbl <> 'R'
105500070322     c                   clear                   dsbl4l
105600070322      * Genero record blocco spedizione se:
105700070322      * --> Consegna particolare Appuntamento o Supermercato (non tassative)
105800070322     c                   if        arbtc1 = 'A' or arbtc1 = 'S' or arbtc2 = 'A'
105900070322     c                             or arbtc2 = 'S'
106000070322     c                   eval      §b4lbas = 'S'
106100070322      * se consegna tassativa sblocco
106200070322     c                   if        arbdcr <> *zeros and arbtcr = *blanks
106300070322     c                   clear                   §b4lbas
106400070322     c                   endif
106500070322     c                   endif
106600070322      * --> Contrassegno non abilitato
106700070322     c                   if        arbacc = *blanks
106800070322     c                   eval      §b4ltca = 'S'
106900070322     c                   endif
107000070322      * --> Fermo deposito
107100070322     c                   if        arbffd <> *blanks
107200070322     c                   eval      §b4ltfd = 'S'
107300070322     c                   endif
107400070322      * scrivo record L di fiar4 per la gestione dei blocchi
107500070322     c                   if        dsbl4l <> *blanks
107600070322     c                   clear                   fiar4000
107700070322     c                   eval      ar4aas = blpaas
107800070322     c                   eval      ar4lnp = blplnp
107900070322     c                   eval      ar4nrs = blpnrs
108000070322     c                   eval      ar4nsp = blpnsp
108100070322     c                   eval      ar4trc = 'L'
108200070322     c                   eval      ar4not = dsbl4l
108300070322     c                   eval      ar4ftr = 'R'
108400070322     c                   eval      ar4dtr = dateu
108500070322     c                   eval      ar4duv = dateu
108600070322     c                   write     fiar4000                             99
108700070322     c                   endif
108800070322     c                   endif
108900950118     C*
109000911002     C                   ENDSR
109100070322      *-----------------------------------------------------------------------
109200060302     c     Scriviar4A    BEGSR
109300060302     c                   if        %found(fiar401l)
109400060302     c                   movel     ar4not        dsbl4a
109500060302     c                   else
109600060302     C                   MOVE      BLPAAS        AR4AAS
109700060302     C                   MOVEL     BLPLNP        AR4LNP
109800060302     C                   MOVEL     BLPNRS        AR4NRS
109900060302     C                   MOVEL     BLPNSP        AR4NSP
110000060302     C                   MOVEL     'A'           AR4TRC
110100060302     C                   CLEAR                   AR4NOT
110200060302     C                   CLEAR                   DSBL4A
110300060302     c                   movel     'R'           ar4ftr
110400060302     c                   movel     dateu         ar4DTR
110500060302     c                   endif
110600060302     c
110700060302     c                   movel     dateu         ar4duv
110800120604     c* ES --> dal momento che non stampiamo più i DDT ma escono con le distinte,
110900120604     c*        non serve più impostare il falg stampa DDT ma lo tengo come inviato
111000120604     c*        dalla partenza
111100060302     C                   MOVEL     BL2SV1        §B4ABM
111200060302
111300060302      * se linea di arrivo con picking attivo o lna per cui stampare i
111400060302      * DDT SI in arrivo
111500120604if  1c**                 If        lnapks = 'S'
111600120604 s  2c**                 Select
111700060302
111800060302      * per bolla di recupero
111900060302      * imposto che la bolla è già stata stampata
112000120604     c**                 When         §3arbl = 'R'
112100120604     c**                 If        Bl2Sv1 = 'S'
112200120604     c**                 Eval      §B4Abm = 'P'
112300120604     c**                 EndIf
112400120604     c**                 If        Bl2Sv1 = 'C'
112500120604     c**                 Eval      §B4Abm = 'Q'
112600120604     c**                 EndIf
112700120604     c**                 If        Bl2Sv1 = 'N' or Bl2Sv1 = 'Y'
112800120604     c**                 Eval      §B4Abm = 'J'
112900120604     c**                 Endif
113000120604     c**                 If        Bl2Sv1 = 'K'
113100120604     c**                 Eval      §B4Abm = 'W'
113200120604     c*+                 Endif
113300060302
113400060302      * se bl2sv1 = 'Y'
113500060302      * imposto 'N' x bolla ancora da stampare
113600120604     c**                 When      Bl2Sv1 = 'Y'
113700120604     c**                 Eval      §B4Abm = 'N'
113800060302
113900120604    2c**                 Endsl
114000120604    1c**                 Endif
114100060302
114200120604     C**   §7AFSR        IFEQ      'Y'
114300120604     C**   BL2SV1        ANDEQ     'Y'
114400120604     C**   lnpNTW        ANDNE     'DPD'
114500120604     C**                 MOVEL     'N'           §B4ABM
114600120604     C**                 ENDIF
114700120604     c*
114800060302     c
114900060302     c* Sigla operatore video
115000060302     C                   MOVE      BL2CEI        §B4ASO
115100060302     C                   MOVEL     DSBL4A        AR4NOT
115200060302     c                   if        %found(fiar401l)
115300060302     c                   update    fiar4000
115400060302     c                   else
115500060302     C                   WRITE     FiAR4000                             99
115600060302     c                   endif
115700060302     c                   endsr
115800911002     C**---------------------------------------------------------
115900911002     C** RIEMPO DETTAGLIO SEGNACOLLI
116000911002     C**---------------------------------------------------------
116100911002     C     RIEBLT        BEGSR
116200950119     C                   CLEAR                   FNART000
116300950118     C                   Z-ADD     ARBAAS        ARTAAS
116400931125     C                   Z-ADD     BLPLNP        ARTLNP
116500911002     C                   Z-ADD     BLPNRS        ARTNRS
116600911002     C                   Z-ADD     BLPNSP        ARTNSP
116700911002     C                   MOVEL     BLPLNA        ARTLNA
116800911002     C                   MOVEL     BLPFLS        ARTFLS
116900950119     C                   MOVEL     'R'           ARTFTR
117000960712     C* SE COLLO LOCALE INDICATO CHE E' ABBINATO
117100960712     C     *IN03         IFEQ      *ON
117200960712     C                   MOVEL     'S'           ARTABN
117300960712     C                   ENDIF
117400950119     C                   ENDSR
117500911014     C**---------------------------------------------------------
117600960424     C** CONTROLLO SE ESISTE ANOMALIA 10-NO REC BOLLA E 12-
117700911014     C**---------------------------------------------------------
117800950128     C     CTRANM        BEGSR
117900960529     C                   Z-ADD     0             OFV               5 0
118000970521     C*
118100970521     C* CHIUDO ANOMALIA 410 SE ESISTE
118200970521     C                   Z-ADD     410           COMCAA
118300990510     C     KANM          CHAIN     FNANM000                           33
118400970521    1C     *IN33         IFEQ      *OFF
118500020809     C                   CLEAR                   FNLR33DS
118600970521     C                   Z-ADD     ANMNR2        D33NRR
118700970521     C                   MOVE      DATEU         D33DCH
118800970521     C                   MOVEL     §7NFS1        D33FSC
118900970521     C                   CALL      'FNLR33R'
119000020809     C                   PARM                    FNLR33DS
119100970521    1C                   END
119200950128     C*
119300960424     C                   Z-ADD     10            COMCAA
119400990510     C     KANM          CHAIN     FNANM000                           33
119500950128     C* CHIUDO ANOMALIA
119600950128    1C     *IN33         IFEQ      *OFF
119700000623     C*
119800020809     C                   CLEAR                   FNLR33DS
119900960415     C                   Z-ADD     ANMNR2        D33NRR
120000960415     C                   MOVE      DATEU         D33DCH
120100960415     C                   MOVEL     §7NFS1        D33FSC
120200960415     C                   CALL      'FNLR33R'
120300020809     C                   PARM                    FNLR33DS
120400960415     C*
120500950128     C* ESEGUO LA CALL AL PGM DI CARICAMENTO SPUNTE PER AGGIORNARE
120600951213     C*  LA BOLLA. NON LO FACCIO SE E' LOCALE
120700961121    2C     *IN98         IFEQ      *OFF
120800951213     C     *IN03         ANDEQ     *OFF
120900021015     c                   Eval      Pa5Mbr = MbrFil
121000021015     c                   Movel     Pa5Mbr4       PA5FIL
121100950128     C                   MOVEL     'F'           PA5FIL
121200950128     C                   MOVE      PARAM5        KPJBU
121300950203     C                   MOVEL     'LR22'        KCOAZ
121400950203     C                   CALL      'BCH10'
121500950203     C                   PARM                    KPJBA
121600950203     C                   SETON                                        98
121700961121    2C                   ENDIF
121800070831     c
121900070831     c* Se esiste anomalia di collo arrivato non partito
122000070831     c*  chiano in update il file fnart e aggiorno i dati della partenza
122100070831     C                   Z-ADD     050           COMCAA
122200070831     C     KANM          CHAIN     FNANM000                           33
122300070831    2C     *IN33         IFEQ      *OFF
122400070831     c     kart          chain     fnart01l
122500070831    3c                   if        %found(fnart01l)
122600070831     c* Se manca data partenza la aggiorno, altrimenti data uscita transito
122700070831    4c                   if        anmcdu=arbtfp
122800070831     C                   Z-ADD     anmdao        ARTDFV
122900070831     C                   Z-ADD     anmnfv        ARTnFV
123000070831     C                   Z-ADD     88            ARTNPP
123100070831    5c                   if        arbtfp=arbtfa
123200070831     C                   Z-ADD     90            ARTNPP
123300070831    5c                   endif
123400070831     C                   movel     'S'           ARTABN
123500070831   x4c                   else
123600070831     C                   Z-ADD     anmdao        ARTDUT
123700070831     C                   Z-ADD     89            ARTPUT
123800070831     C                   Z-ADD     anmcdu        ARTFLP
123900070831     C                   movel     'S'           ARTABN
124000070831    4c                   endif
124100070831     c
124200070831    4c                   if        lnantw<>'EEX' and lnantw<>'EUP' and
124300070831     c                             lnantw<>'FED' and lnantw<>'DPD'
124400070903     c                   update    fnart001
124500070831    4c                   endif
124600070831    3c                   endif
124700070831    2c                   endif
124800961122     C**
124900000111     C** SE LNA ESTERA CONTROLLO SE ESISTE EVENTO COLLO 2 /6A/6
125000020226    2C     lnaNTW        IFEQ      'EEX'
125100020226    2C     lnaNTW        OREQ      'EUP'
125200020226    2C     lnaNTW        OREQ      'FED'
125300060322    2C     lnaNTW        OREQ      'DPD'
125400961122     C                   CLEAR                   WDEV
125500961122     C                   CLEAR                   WCEV
125600961122     C* APRO IL FILE
125700961122    3C     WAPER         IFEQ      ' '
125800961122     C                   OPEN      FNEVS02L
125900961122     C                   MOVEL     'S'           WAPER             1
126000961122    3C                   ENDIF
126100961122     C**
126200961122     C                   MOVEL     'E2 '         KCEV
126300020809    3C                   DO        3             XX
126400961122     C     KEVS          SETLL     FNEVS02L
126500961122     C     KEVS          READE     FNEVS02L                               33
126600961122    4C     *IN33         DOWEQ     *OFF
126700970602    5C     EVSDEV        IFGT      WDEV
126800961122     C     WDEV          OREQ      0
126900961122     C                   MOVEL     EVSDEV        WDEV
127000961122     C                   MOVEL     EVSCEV        WCEV
127100961122    5C                   ENDIF
127200961122     C**
127300961122     C* AGGIORNO L'EVENTO COL NUMERO DI SPEDIZIONE
127400961122     C                   Z-ADD     BLPAAS        EVSAAS
127500961122     C                   Z-ADD     BLPLNP        EVSLNP
127600961122     C                   Z-ADD     BLPNSP        EVSNSP
127700961122     C                   CLEAR                   EVSFTR
127800961122     C                   CLEAR                   EVSDTR
127900961122     C                   UPDATE    FNEVS000
128000961122     C*
128100961122     C     KEVS          READE     FNEVS02L                               33
128200961122    4C                   ENDDO
128300961122     C**
128400961122     C* AL 2 GIRO CERCO LE DISCOR.6  --> COLLO ARRIVATO MA A TERRA
128500020809    4C     XX            IFEQ      1
128600961122     C                   MOVEL     'E6 '         KCEV
128700961122    4C                   ENDIF
128800970602     C* AL 3 GIRO CERCO LE DISCOR.6B --> COLLO ARRIVATO MA IN PARTENZA
128900020809    4C     XX            IFEQ      2
129000970602     C                   MOVEL     'E6B'         KCEV
129100961122    4C                   ENDIF
129200961122    3C                   ENDDO
129300961122     C* SE TROVATA AGGIORNO IL DETTAGLIO COLLI
129400961122    3C     WDEV          IFGT      0
129500961122     C* SE ERA COLLO RIAMSTO A TERRA ARTDTR DEVE ESSERE = 0
129600970602    4C     WCEV          IFEQ      'E6 '
129700961122     C                   CLEAR                   ARTDTR
129800961122     C                   ELSE
129900961122     C* ALTRIMENTI IMPOSTO LA DATA DELL'EVENTO
130000961122     C                   Z-ADD     WDEV          ARTDTR
130100961122    4C                   ENDIF
130200961122     C* AGGIORNO RECORD
130300961122     C                   UPDATE    FNART001
130400961122     C* DISALLOCO RECORD
130500961122   X3C                   ELSE
130600070903     C                   update    FNART001
130700961122    3C                   ENDIF
130800961122     C*
130900961122    2C                   ENDIF
131000960529     C*
131100960529     C* LEGGO LA SPUNTA PARTENZA DEL COLLO
131200040317     C*  PER REPERIRE LA DATA DEL FOGLIO VIAGGIO PARTENZA, AI FINI DI
131300040317     C*  DETERMINARE IL TERMINAL DI ARRIVO DELLA LINEA DI ARRIVO
131400040317     c*  In realtà prendiamo la data della prima spunta partenza che
131500040317     c*  troviamo del collo, che potrebbe essere anche quella di un
131600040317     c*  p.o. di transito o di disguido. Ma sono casi molto rari ed
131700040317     c*  un problema si potrebbe presentare soltanto in caso di
131800040317     c*  cambio del terminal di arrivo della lna tra il momento in
131900040317     c*  cui parte/arriva il collo, ed il momento in cui
132000040317     c*  parte/arrva la bolla -vsito che siamo nel caso di manca rec-
132100040317     c*  con il terminal di arrivo calcolato richiama LS44 solo x le
132200040317     c*  spunte dell'area di arrivo, oper aggiornare la bolla in arr
132300040317     c*  Ma se cambia terminal in base alla data spunta le aree di arr
132400040317     c*  possono essere 2. allora si dovrebbe o duplicare dei controlli
132500040317     c*  che il pgm ls44 già fa, o richimare ls44 in modo che aggiorni
132600040317     c*  solo fnarb Ma data la eccezionalità dei casi, non facciamo
132700040317     c*  nulla e teniamo il pgm come è
132800040317     c*
132900960603     C                   EXSR      DFVPAR
133000961121     C*
133100960617     C* PRENDO IL TERMINAL DI ARRIVO DELLA LNA
133200020809     C                   CLEAR                   FNLV55DS
133300971114     C                   MOVEL     'A'           D55TPT
133400971114     C                   MOVEL     ARTLNP        D55LNP
133500971114     C                   MOVEL     ARTLNA        D55LIN
133600971114     C                   MOVEL     WDFV          D55DRF
133700971114     C                   CALL      'FNLV55R'
133800020809     C                   PARM                    FNLV55DS
133900971114     C                   MOVEL     'S'           WCAL55            1
134000971114     C**
134100971114     C     D55ERR        IFNE      ' '
134200971114     C                   MOVEL     D55LIN        D55TFA
134300971114     C                   ENDIF
134400971114     C                   MOVEL     D55TFA        LNATFA
134500950130     C*
134600950130     C* CERCO UNA SPUNTA DEL DETTAGLIO COLLO
134700070123     C     KSPP          CHAIN     FNBRV07L                           33
134800960529     C*
134900961121    2C     *IN33         DOWEQ     *OFF
135000960705     C*
135100960705     C* SE CATEGORIA 1 ESCLUDO SE IL FOGLIO E' UN NORMALE: ELABORO
135200960705     C*  SOLO SE E' DEFLUENZA
135300961121    3C     BRVNPG        IFEQ      1
135400960705     C                   CLEAR                   KEY
135500960705     C                   CLEAR                   WELAB
135600960705     C*
135700000110     C                   Z-ADD     BRVNFV        KNFV
135800040317     C                   Z-ADD     BRVfgs        Kfgs
135900020809     C     K02FGV01      chain     FNFGV01L                           36
136000031203     c     *in36         ifeq      *off
136100031203     C                   MOVEL     FGVTTR        WTTR
136200031203   X4C                   ELSE
136300960705     C     KFVA          CHAIN     FNFVA01L                           36
136400980623     C                   MOVEL     FVATTR        WTTR
136500031203    4C                   ENDIF
136600960705     C*
136700980623    4C     WTTR          IFNE      *BLANKS
136800980623     C     WTTR          LOOKUP    TDF                                    36
136900980623     C  N36              MOVEL     'N'           WELAB             1
137000961121    4C                   ENDIF
137100960705     C*
137200961121    3C                   ENDIF
137300960705     C*
137400960705     C** ELABORO LA CATEGORIA 1 SOLO PER LA DEFLUENZA E LE ALTRE CATEG
137500961121    3C     BRVNPG        IFNE      1
137600960705     C     WELAB         OREQ      ' '
137700960617     C**
137800960603     C* LEGGERE LE ECCEZIONI TERMINAL
137900960603     C**
138000020809     C                   CLEAR                   FNLV55DS
138100971114     C                   MOVEL     'A'           D55TPT
138200971114     C                   MOVEL     ARTLNP        D55LNP
138300031203     C                   MOVEL     BRVPES        D55LIN
138400971114     C                   MOVEL     WDFV          D55DRF
138500971114     C                   CALL      'FNLV55R'
138600020809     C                   PARM                    FNLV55DS
138700971114     C                   MOVEL     'S'           WCAL55            1
138800971114     C**
138900971114     C     D55ERR        IFNE      ' '
139000971114     C                   MOVEL     D55LIN        D55TFA
139100971114     C                   ENDIF
139200031203     C                   MOVEL     D55TFA        PESTFA
139300960529     C*
139400031203    5C     LNATFA        IFEQ      PESTFA
139500960603     C** IMPOSTO L'AS GESTIONE DELLA ANOMALIA
139600970114     C                   CLEAR                   OFV
139700960529     C                   EXSR      CHIAMO
139800970115     C*
139900960529    5C                   ENDIF
140000961121    3C                   ENDIF
140100960529     C**
140200070123     C     KSPP          READE     FNBRV07L                               33
140300961121    2C                   ENDDO
140400961121     C**
140500961121    1C                   ENDIF
140600950130     C*
140700950130     C                   ENDSR
140800960603     C**---------------------------------------------------------
140900960603     C*  LEGGO LA SPUNTA PARTENZA DEL COLLO PER PRENDERE LA DATA FV
141000960603     C**---------------------------------------------------------
141100960603     C     DFVPAR        BEGSR
141200960603     C                   CLEAR                   WDFV
141300070123     C     KSPP          CHAIN     FNBRV07L                           34
141400960603    1C     *IN34         DOWEQ     *OFF
141500960603    2C     BRVNPG        IFEQ      1
141600960603     C*
141700000110     C                   Z-ADD     BRVNFV        KNFV
141800040317     C                   Z-ADD     BRVfgs        Kfgs
141900020809     C     K02FGV01      chain     FNFGV01L                           35
142000960603     C**
142100960603     C* ESCLUDO I TRAINI DI DEFLUENZA
142200031216    3C     *IN35         IFEQ      *OFF
142300960603     C**
142400031216    5C     FGVTTR        IFNE      ' '
142500031216     C     FGVTTR        LOOKUP    TDF                                    35
142600031216    5C                   ENDIF
142700980623     C**
142800031216    5C     FGVTTR        IFEQ      ' '
142900031216     C     *IN35         OREQ      *OFF
143000980623     C**
143100031216    6C     FGVDFV        IFGT      WDFV
143200031216     C                   MOVEL     FGVDFV        WDFV
143300031216    6C                   ENDIF
143400031216    5C                   ENDIF
143500960603     C**
143600960603   X3C                   ELSE
143700960603     C**
143800960603     C     KFVA          CHAIN     FNFVA01L                           35
143900031216    4c     *in35         ifeq      *off
144000031216    5C     FVATTR        IFNE      ' '
144100031216     C     FVATTR        LOOKUP    TDF                                    35
144200031216    5C                   ENDIF
144300031216     C**
144400031216    5C     FVATTR        IFEQ      ' '
144500031216     C     *IN35         OREQ      *OFF
144600031216    6C     FVADFV        IFGT      WDFV
144700960603     C                   MOVEL     FVADFV        WDFV
144800031216    6C                   ENDIF
144900031216    5c                   endif
145000031216    4c                   endif
145100960603     C*
145200960603    3C                   ENDIF
145300960603    2C                   ENDIF
145400070123     C     KSPP          READE     FNBRV07L                               34
145500960603    1C                   ENDDO
145600960603     C**
145700960603     C* SE NON L'HO TROVATA PRENDO LA DATA DI CREAZIONE DELLA ANOMALIA
145800960603     C     WDFV          IFEQ      0
145900960603     C                   Z-ADD     ANMDAO        WDFV
146000960603     C                   ENDIF
146100960603     C                   ENDSR
146200950130     C**---------------------------------------------------------
146300950130     C** CHAIMO PGM DI AGGIORNAMENTO BOLLE DA SPUNTA
146400950130     C**---------------------------------------------------------
146500950130     C     CHIAMO        BEGSR
146600070228     c                   clear                   fnls45ds
146700070228     c                   movel     'S'           dls45ric
146800070228     c                   move      brvpes        dls45pes
146900070228     c                   move      simfel        dls45tfp
147000070228     c                   call      'FNLS45R'
147100070228     c                   parm                    fnls45ds
147200070228     c                   parm                    dsbrv
147300950130     C*
147400950130     C                   SETON                                        02
147500950130     C                   ENDSR
147600950123     C***--------------------------------------------------------
147700080411     C** invio msg in sede
147800911003     C**---------------------------------------------------------
147900911003     C     STAERR        BEGSR
148000111104     c* Mando e_mail
148100111104     c                   clear                   msgtes
148200111104     c                   clear                   msgdet
148300111104     C                   MOVEL     MSG(1)        msgtes
148400111104     c                   eval      %subst(msgtes:26:3)=%editc(simfel:'X')
148500111104     c                   eval      %subst(msgtes:35:10)=mbrfil
148600080411
148700080411     C                   MOVEL     BLPRSM        STARSM           20
148800080603     c     kblp2         chain     fnblp01l
148900080603     c                   if        not %found(fnblp01l)
149000080603     c                   clear                   w_blpctm
149100080603     c                   endif
149200080411
149300111104     c                   eval      msgdet='Bolla: '+%editc(blpaas:'X') +' '
149400080411     c                             + %editc(blplnp:'X')+' '+%editc(blpnrs:'Z')
149500080411     c                             + ' '+%editc(blpnsp:'Z')+' LNA '
149600111104     c                             + %editc(blplna:'Z')+' Mit:'+ starsm +
149700111104     c                             '       Cod.Trattamento merce:' +W_blpctm
149800080411     c* Bolla già esistente
149900111104     c   06              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(2)
150000111104     c** 06              eval      ym5t03=msg(2)
150100080411     c* non trovato dettaglio colli
150200111104     c   07              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(3)
150300111104     c** 07              eval      ym5t03=msg(3)
150400080411     c* Segnacollo esistente
150500080411     c                   if        *in04
150600111104     c   08              eval      %subst(msg(4):22:7)=%editc(artnsc:'Z')
150700111104     c   08              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(4)
150800111104     c  N08              eval      %subst(msg(5):22:7)=%editc(artnsc:'Z')
150900111104     c  N08              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(5)
151000111104     c** 08              eval      ym5t03=msg(4)
151100111104     c**N08              eval      ym5t03=msg(5)
151200111104     c**                 eval      %subst(msgT03:22:7)=%editc(artnsc:'Z')
151300080411     c                   endif
151400080411     C*
151500120305     C                   eval      wrkEml= 'ced@brt.it'
151600111104     C                   call(e)   'TIS701C'
151700111104     C                   parm                    wrkEml          253
151800111104     C                   PARM                    MSGTES           44
151900111104     C                   PARM                    MSGDET         5000
152000080411     C*
152100000622     C                   EXSR      DLTRIC
152200000622     C                   ENDSR
152300000622     C***--------------------------------------------------------
152400000622     C** DELETO RECORD DA FILE DI RICEZIONE PER BOLLA DOPPIA
152500000622     C**---------------------------------------------------------
152600000622     C     DLTRIC        BEGSR
152700921123     C*
152800921123     C* C A N C E L L O  SOLO SE BOLLA DOPPIA
152900921123     C   06              DO
153000950118     C                   DELETE    FNBLPE00
153100920917     C                   END
153200911003     C*
153300911003     C                   ENDSR
153400040317     C*
153500951011     C* CONTROLLO SE LOCALE SE DEVO AGGIORNARE LA DATA PARTENZA------*
153600031202     C     CTRDET        BEGSR
153700031202     C                   Z-ADD     1             KNPG
153800070123     C     KBRV9         CHAIN     FNBRV09L                           35
153900951011     C* TROVATA SPUNTA LOCALE PARTITA NON SUL FV IN ELABORAZIONE
154000980623     C*  --> AGGIORNO  SE NON SI TRATTA DI FOGLIO DEFLUENZA E SE
154100980623     C*  IL FOGLIO E' CHIUSO
154200031202    1C     *IN35         DOWEQ     *OFF
154300031202   1AC     BRVNFV        IFNE      PARNFV
154400041006     c* ignoro la spunta se fatta su foglio viaggio partenza errato
154500041006     c* (lna del collo non presente sul foglio viaggio)
154600041006     c     brvftr        andne     'L'
154700951011     C*
154800951011     C* DATA PARTENZA MERCE E NUMERO PISTOLA PARTENZA SOLO SE LOCALE
154900031202     C                   Z-ADD     BRVNFV        KNFV
155000040317     C                   Z-ADD     BRVfgs        Kfgs
155100031202     C     K02FGV01      chain     FNFGV01L                           35
155200031202    2C     FGVFCF        IFEQ      'S'
155300980623     C* CONTROLLO IL TIPO TRAINO
155400031202    3C     FGVTTR        IFNE      ' '
155500031202     C     FGVTTR        LOOKUP    TDF                                    36
155600031202    3C                   ENDIF
155700980623     C**
155800031202    3C     FGVTTR        IFEQ      ' '
155900031202     C     *IN36         OREQ      *OFF
156000031203     c                   clear                   flg15
156100031202     C                   CLEAR                   KPJBU
156200031216     c                   movel     param         paramx
156300031216     c                   z-add     brvnfv        parnfv
156400031216     c                   z-add     fgvdfv        pardtp
156500031202     C                   MOVEL     PARAM         KPJBU
156600031202     c                   call      'FNLR15R'
156700031202     c                   parm                    kpjba
156800031202     c                   parm                    dsbrv
156900031203     c                   parm                    flg15             1
157000031216     c                   movel     paramx        param
157100031202    3C                   ENDIF
157200031202    2C                   ENDIF
157300951011     C*
157400031202   1AC                   ENDIF
157500070123     C     KBRV9         READE     FNBRV09L                               35
157600031202    1C                   ENDDO
157700951011     C*
157800031202     C                   ENDSR
157900960826     C*
158000960827     C* CANCELLAZIONE ANOMALIA 50-COLLO ARRIVATO NON PARTITO --------*
158100960826     C     ANM050        BEGSR
158200960826     C                   Z-ADD     50            COMCAA
158300990510     C     KANM          CHAIN     FNANM000                           33
158400960826    1C     *IN33         IFEQ      *OFF
158500020809     C                   CLEAR                   FNLR33DS
158600960826     C                   Z-ADD     ANMNR2        D33NRR
158700960826     C                   MOVE      'S'           D33MAC
158800960826     C                   CALL      'FNLR33R'
158900020809     C                   PARM                    FNLR33DS
159000960826     C                   END
159100960826     C                   ENDSR
159200080411     c*----------------------------------------------------------------
159300911001**         CMD5
159400021008CHKOBJ OBJ(FNBLPE0R) OBJTYPE(*FILE)     MBR(M234567890)
159500911001**         CMD6
159600021008ALCOBJ OBJ((FNBLPE0R *FILE *EXCL           M234567890))
159700911002**         CMD9
159800021008OVRDBF FILE(FNBLPE0R)                   MBR(M234567890)
159900050804**         CmdDSPLOG
160000050804DSPJOBLOG OUTPUT(*PRINT)
160100000622**
160200080411RICEZIONE BOLLE ARRIVO A xxx: MBR Mxxxyyy                               1
160300111104Bolla già esistente .                                                   2
160400111104Non trovato dettaglio colli .                                           3
160500111104Segnacollo esistente xxxxxxx : SOSTITUITO .                             4
160600111104Segnacollo esistente xxxxxxx : NON sostituito .                         5
