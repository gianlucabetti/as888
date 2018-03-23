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
010300140212     D trtcp2ds      E DS
010400020809     D TIBS02DS      E DS
010500070228     D fnls45ds      E DS
010600020809     d trul21ds      e ds
010700070123     D DSBRV         E DS                  EXTNAME(FNBRV00F)
010800020226
010900950118     D***
011000950118     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
011100950118     D***
011200950118     D WLBDAT          DS                  INZ
011300950118     D  G02DAT                 1      8  0
011400950118     D  G02INV                 9     16  0
011500950118     D  G02ERR                17     17
011600950118     D  G02TGI                18     22  0
011700950919     D* PARAMETRI PASSATI
011800950919     D PARAM           DS
011900950919     D  PARDTP                 1      8  0
012000950919     D  PARNFV                 9     13  0
012100960806     D  PARDEF                14     14
012200051006     D  tentativi            234    236
012300051006     D  LEGFIL               237    246
012400950919     D  MBRFIL               247    256
012500950919     D  ALFFIL               248    250
012600950919     D* PER ESPLOSIONE SPUNTE IN FILE DI RICEZIONE
012700950128     D PARAM5          DS
012800950128     D  PA5FIL               237    246    INZ('FXXX999999')
012900021015     D  PA5MBR4              247    250    INZ
013000021015     D  PA5MBR               247    256    Inz
013100020809     D                 DS
013200020809     D  BLPAAS                 1      4  0
013300020809     D  BLPMGS                 5      8  0
013400020809     D  BLPDSP                 1      8  0
013500020809     D                 DS
013600020809     D  WNUM                   1     13
013700020809     D  NUM                    1     13
013800020809     D                                     DIM(13)
013900020809     D                 DS
014000020809     D  WNUME                  1     14
014100020809     D  NUE                    1     14
014200020809     D                                     DIM(14)
014300990510     D FNANM1          DS
014400960424     D  ANMNR1               397    400B 0
014500990510     D FNANM2          DS
014600960424     D  ANMNR2               397    400B 0
014700000622     D                 DS
014800050804     D* DEFINIZIONE COSTANTI
014900050804     D CUTI            C                   CONST('EDP*      ')
015000011210
015100011210     d                sds
015200011210     d  nompgm                 1     10
015300020809
015400911003     C****************************************************************
015500911003     C*  RIEPILOGO INDICATORI
015600911003     C***************************************************************
015700911016     C* 02    - LACIO PGM ABILITAZIONE MERCE
015800911016     C* 03    - SONO ME STESSO NON SCRIVO CMRIC
015900920917     C* 04    - DETTAGLIO DOPPIO
016000050314     C* 06    - off - bolla non presente
016100921123     C* 07    - MANCA DETTAGLIO COLLI PER BOLLA CON COLLI NO SEQ
016200950905     C* 08    - DETTAGLIO DOPPIO - AGGIORNATO PERCHE' GIA' CONSEGNATO
016300940322     C* 10    - SPUNTA DAL MIO TERMINAL
016400940322     C* 18/19 - INDICATORI DI TIPO RECORD
016500911003     C* 20    - NON ALLOCATI DEGLI AGGETTI ESCO DA PROGRAMMA
016600911003     C* 31    - LETTURA FLBLPE00
016700911003     C* 32/33 - COMODO
016800990906     C* 34    - COMODO
016900960705     C* 35/36 - COMODO
017000911003     C*****************************************************************
017100000000     C     *ENTRY        PLIST
017200000000     C                   PARM                    KPJBA
017300950919     C                   MOVEL     KPJBU         PARAM
017400000000     C                   Z-ADD     1             CODUT
017500911001     C                   CALL      'X§PARUT'
017600020809     C                   PARM                    UT§DSE0F
017700911001     C                   MOVEL     REC80         CNCR80
017800920423     C                   MOVEL     RAGUT         RSUT             20
017900021015     c*
018000070228     C*    SIMFEL        CHAIN     AZORG01L                           30
018100070228     C* N30              MOVEL     ORGDE3        OG143
018200070228     C* N30              MOVEL     §OGAS         COMAS             3 0
018300070228     C*  30              MOVEL     SIMFEL        COMAS
018400070228     C*                  Z-ADD     SIMFEL        COMTFP            3 0
018500900618     C*---------------------------------------------------------------*
018600950118     C     KBLP2         KLIST
018700950118     C                   KFLD                    BLPAAS
018800950118     C                   KFLD                    BLPLNP
018900950118     C                   KFLD                    BLPNRS
019000950118     C                   KFLD                    BLPNSP
019100050314     C     Kar6          KLIST
019200050314     C                   KFLD                    BLPAAS
019300050314     C                   KFLD                    BLPLNP
019400050314     C                   KFLD                    BLPNRS
019500050314     C                   KFLD                    BLPNSP
019600050314     C                   KFLD                    ktrc
019700070831     C     Kart          KLIST
019800070831     C                   KFLD                    BLPAAS
019900070831     C                   KFLD                    BLPLNP
020000070831     C                   KFLD                    BLPNRS
020100070831     C                   KFLD                    BLPNSP
020200070831     C                   KFLD                    artnsc
020300911014     C     KANM          KLIST
020400911014     C                   KFLD                    ARTFLS
020500911014     C                   KFLD                    ARTLNA
020600911014     C                   KFLD                    ARTNRS
020700950119     C                   KFLD                    ARTNSC
020800020809     C                   KFLD                    COMCAA
020900961121     C     KEVS          KLIST
021000961121     C                   KFLD                    ARTFLS
021100961121     C                   KFLD                    ARTLNA
021200961121     C                   KFLD                    ARTNRS
021300961121     C                   KFLD                    ARTNSC
021400961121     C                   KFLD                    KCEV
021500960424     C     KANM1         KLIST
021600990510     C                   KFLD                    BLPAAS
021700960424     C                   KFLD                    BLPLNP
021800960424     C                   KFLD                    BLPNRS
021900960424     C                   KFLD                    BLPNSP
022000020809     C                   KFLD                    COMCAA
022100911014     C     KSPP          KLIST
022200950413     C                   KFLD                    ARTFLS
022300950413     C                   KFLD                    ARTLNA
022400950413     C                   KFLD                    ARTNRS
022500950119     C                   KFLD                    ARTNSC
022600951011     C     KBRV9         KLIST
022700951011     C                   KFLD                    KNPG
022800951011     C                   KFLD                    ARTFLS
022900951011     C                   KFLD                    ARTLNA
023000951011     C                   KFLD                    ARTNRS
023100951011     C                   KFLD                    ARTNSC
023200951011     C                   KFLD                    SIMFEL
023300911002     C     KTAB          KLIST
023400911002     C                   KFLD                    CODUT
023500020809     C                   KFLD                    COD
023600020809     C                   KFLD                    KEY
023700980623     C     KTAB2         KLIST
023800980623     C                   KFLD                    CODUT
023900020809     C                   KFLD                    COD
024000960529     C     KFVA          KLIST
024100960529     C                   KFLD                    BRVFGS
024200000110     C                   KFLD                    KNFV
024300000622     C     KAR4          KLIST
024400000622     C                   KFLD                    BLPAAS
024500000622     C                   KFLD                    BLPLNP
024600000622     C                   KFLD                    BLPNRS
024700000622     C                   KFLD                    BLPNSP
024800000622     C                   KFLD                    WTRC
024900020809     C     K02FGV01      klist
025000020809     C                   kfld                    KNFV
025100040317     C                   kfld                    kfgs
025200950119     C*---------------------------------------------------------------*
025300911011     C* GIRO DATA DEL GIORNO
025400020809     C                   TIME                    W0140
025500950118     C                   MOVE      W0140         G02DAT
025600911011     C                   MOVEL     *BLANK        G02ERR
025700950118     C                   CALL      'XSRDA8'
025800911011     C                   PARM                    WLBDAT
025900020809     C                   Z-ADD     G02INV        DATEU
026000020809     C                   Z-ADD     G02DAT        UDATE8
026100950214     C*
026200911003     C                   SETON                                        OF
026300040317     c                   eval      xx=1
026400911011     C*
026500911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
026600911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
026700911001     C     MBRFIL        IFEQ      *BLANK
026800021008     C                   MOVEL     '*FIRST'      MBRFIL
026900911001     C                   END
027000911001     C* MEMBRO LUNGO 3
027100020809     C                   MOVEL     ALFFIL        MBR
027200911107     C*
027300911107     C* IMPOSTAZIONI INIZIALI
027400911107     C                   EXSR      CARTAB
027500031027
027600911001     C*
027700911001     C* ALLOCO MEMBRI
027800911001     C                   EXSR      CAEX
027900911001     C   20              GOTO      FINE
028000911001     C*
028100950118     C                   READ      FNBLPE00                               31
028200911003     C*
028300931125     C     *IN31         DOWEQ     *OFF
028400921123     C                   SETOFF                                       040607
028500040317     C                   SETOFF                                       03
028600000622     C     BLPLNP        IFNE      SAVLNP
028700000622     C     BLPLNP        CHAIN     AZORG01L                           32
028800000622     C  N32              MOVEL     ORGDE3        OG143
028900000622     C   32              CLEAR                   OG143
029000020226     c                   eval      lnpntw = §ogntw
029100000622     C                   Z-ADD     BLPLNP        SAVLNP
029200000622     C                   ENDIF
029300031210     c* A cambio di lna devo memorizzare se è lna con picking attivo
029400041215     c* o lna per la quale stampare i DDT SI in arrivo
029500070322      * o se lna abilitata alle 'telefonate GEO'
029600031210     C     BLPLNA        IFNE      SAVLNA
029700060515     c                   clear                   lnapks
029800070322     c                   clear                   lnageo
029900031210     c* controllo se lna gestita da altri
030000031210     c                   clear                   fnlv55ds
030100031210     c                   movel     '6'           D55TPT
030200031210     c                   z-add     blplna        d55lin
030300031210     c                   z-add     dateu         d55drf
030400031210     c                   movel     'S'           wcal55
030500031210     c                   call      'FNLV55R'
030600031210     c                   parm                    fnlv55ds
030700060515      * se è un p.o. gestito da altri controllo se la lna stessa ha il picking attivo
030800060515     c                   if        d55lin <> d55tfa
030900060515     c     d55lin        chain     azorg01l                           32
031000060515     c  n32              movel     orgdf0        og150
031100070322     c  n32              eval      og148 = orgde8
031200060515     c   32              clear                   og150
031300070322     c   32              clear                   og148
031400120604     c**                 If        §OgPck = 'S' and §OgPkd <> 'S'
031500120604     c**                           or §ogsda = 'S'
031600120604     c**                 eval      lnapks = 'S'
031700120604     c**                 endif
031800070322      * controllo se LNA è abilitata a telefonate GEO
031900070322     c                   if        §oggeot = 'S'
032000070322     c                   eval      lnageo = 'S'
032100070322     c                   endif
032200060515     c                   endif
032300060515      * controllo se c'è il picking attivo
032400120604     c**                 if        lnapks = *blanks
032500120604     C**   d55tfa        CHAIN     AZORG01L                           32
032600120604     C**N32              MOVEL     ORGDf0        OG150
032700120604     C** 32              CLEAR                   OG150
032800120604if  5c**                 If        §OgPck = 'S' and §OgPkd <> 'S'
032900120604     c**                           or §ogsda = 'S'
033000120604     c**                 eval      lnapks = 'S'
033100120604     c**                 else
033200120604     c**                 clear                   lnapks
033300120604     c**                 endif
033400120604     c**                 endif
033500120604     c
033600070322      * se lna non abilitata a telefonate GEO controllo la sua capofila
033700070322     c                   if        lnageo = *blanks
033800070322     c     d55tfa        chain     azorg01l                           32
033900070322     c  n32              eval      og148 = orgde8
034000070322     c   32              clear                   og148
034100070322     c                   if        §oggeot = 'S'
034200070322     c                   eval      lnageo = 'S'
034300070322     c                   endif
034400070322     c                   endif
034500031210     C                   Z-ADD     BLPLNA        SAVLNA
034600031210     C                   ENDIF
034700040317     c
034800040317     c* Controllo bolla per bolla se si tratta di locale, non più in base
034900040317     c*  al membro ma in base ai terminal di partenza/arrivo
035000040330    1c                   if        blptfa=blptfp
035100040330    2c     blplna        ifne      blptfa
035200040330     C     blplna        CHAIN     AZORG01L                           32
035300040330     C  N32              MOVEL     ORGde3        OG143
035400040330     C   32              CLEAR                   OG143
035500040330     c                   z-add     §ogas         ogaslna           3 0
035600040330     C     blptfa        CHAIN     AZORG01L                           32
035700040330     C  N32              MOVEL     ORGDe3        OG143
035800040330     C   32              CLEAR                   OG143
035900040330    3c     ogaslna       ifeq      §ogas
036000040330     c                   seton                                        03
036100040330    3c                   endif
036200040330   x2c                   else
036300040317     c                   seton                                        03
036400040330    2c                   endif
036500040317     c                   endif
036600000713     C*
036700950118     C     KBLP2         SETLL     FNARB01L                               06
036800060220     c
036900000622     C     *IN06         IFEQ      *ON
037000060220     c
037100060220     C**   §B4ASO        ANDNE     '&'
037200060220     C**   *IN06         OREQ      *ON
037300060220     C**   BL2CEI        ANDNE     '&'
037400000622     C* 06 ON E BOLLA NON AUTOGENERATA - ERRORE GIA' ESISTENTE
037500060220     c* 20/02/06 : tolgo il discorso delle autogenerate perchè valeva solo
037600060220     c*            per Poste; se bolle già presente sempre errore
037700000622     C                   EXSR      STAERR
037800000622     C                   ELSE
037900000622     C* 06 OFF OPPURE ON MA BOLLA AUTOGENERATA: CREO O AGGIORNO BOLLA
038000060220     C** 06KBLP2         CHAIN     FNARB01L                           32
038100000622     C                   EXSR      CRTBOL
038200040317     c
038300040317     c* Memorizzo il numero foglio se bolla locale per la sottomissione
038400040317     c                   if        *in03
038500040317     c     blpnfv        lookup    nfv                                    36
038600040317     c                   if        not *in36
038700040317     c                   z-add     blpnfv        nfv(xx)
038800040317     c                   add       1             xx                3 0
038900040317     c                   endif
039000040317     c                   endif
039100040317     c
039200000622     C                   ENDIF
039300060220     c
039400950118     C                   READ      FNBLPE00                               31
039500911001     C                   END
039600031203     c* richiamo fnlr15r per la chiusura file e la scrittura dell'ultim
039700031203     c* agb
039800031203     c                   movel     'C'           flg15
039900031203     c                   call      'FNLR15R'
040000031203     c                   parm                    kpjba
040100031203     c                   parm                    dsbrv
040200031203     c                   parm                    flg15
040300900911     C*
040400040428     c                   setoff                                       2022
040500900409     C     FINE          TAG
040600040428     c*
040700040428     c* Per 20 on - risottometto ricezione bolle e spunte
040800040428     c                   if        *in20  and not *in22
040900050804     c
041000051006     c                   testn                   tentativi            30
041100051006     c                   move      tentativi     w001a             1
041200051006     c                   if        not *in30 or w001a<'0'
041300051006     c                   eval      tentativi='001'
041400051006     c                   eval      w0030=1
041500051006     c                   else
041600051006     c                   movel     tentativi     w0030             3 0
041700051006     c                   add       1             w0030
041800051006     c                   movel     w0030         tentativi
041900051006     c                   endif
042000051006     c
042100051006     c* max 200 tentativi
042200051006     c                   if        w0030<=200
042300050804     C                   MOVEL     'K'           legfil
042400050804     C                   MOVEl(P)  PARAM         KPJBU
042500040428     c
042600040428     C                   MOVEL     'LR14'        KCOAZ
042700040428     C                   CALL      'BCH10'
042800040428     C                   PARM                    KPJBA
042900040428     c
043000040428     C                   MOVEL     'LR22'        KCOAZ
043100040428     C                   CALL      'BCH10'
043200040428     C                   PARM                    KPJBA
043300040428     C                   SETON                                        98
043400051006     c                   else
043500051006     c
043600051006     c* msg x  pgm non eseguito e da chi allocato
043700051006     c
043800050804     c* richiamo pgm per rifare alcobj
043900051006     c                   call      'FNLR14C1'
044000080407     c                   parm                    fisico           10
044100051006     c                   parm                    mbrfil
044200051006     c
044300050804     c* eseguo dspjoblog per vedere chi mi alloca
044400051006     c                   Movel     CmdDSPLOG(1)  Comman
044500051006     c                   Call      'QCMDEXC'
044600051006     c                   Parm                    Comman
044700051006     c                   Parm                    Lung
044800050804     C
044900111104     c                   clear                   msgtes
045000111104     c                   clear                   msgdet
045100111104     c                   eval      msgtes='RICEZIONE BOLLE: FNLR14R '
045200111104     c                   eval      msgdet='FILE ALLO'+
045300111104     C                             'CATO. VEDI DSPJOBLOG   -   utente:'+knmus
045400050804     C
045500120305     C                   eval      wrkEml= 'ced@brt.it'
045600111104     C                   call(e)   'TIS701C'
045700111104     C                   parm                    wrkEml          253
045800111104     C                   PARM                    MSGTES           44
045900111104     C                   PARM                    MSGDET         5000
046000050804     C*
046100051006     c                   endif
046200050804     c
046300040428    2C                   ENDIF
046400040317     c*
046500040317     c* sottometto lr15 del foglio passato solo se è locale
046600040317     c                   if        nfv(1)>0
046700040317     c     parnfv        lookup    nfv                                    36
046800040317     c                   if        *in36 and pardef<>'S'
046900040317     c                   clear                   kpjbu
047000040317     C                   MOVEL     PARAM         KPJBU
047100040317     C                   CALL      'FNLR15R'
047200040317     c                   PARM                    KPJBA
047300040317     c                   endif
047400040317     c                   endif
047500950919     C*
047600950128     C** LANCIO PGM DI ABILITAZIONE BOLLE ALLA CONSEGNA
047700970509     C     *IN02         IFEQ      *ON
047800970509     C* CHIUDO PGM FNLS44R PER FARE SCRIVERE L'ULTIMO FNAGB
047900070228     C                   MOVEL     'C'           dls45flg
048000070228     C                   CALL      'FNLS45R'
048100070228     c                   parm                    fnls45ds
048200070228     c                   parm                    dsbrv
048300970509     C                   ENDIF
048400971114     C**
048500971114     C     WCAL55        IFEQ      'S'
048600020809     C                   CLEAR                   FNLV55DS
048700971114     C                   MOVEL     'C'           D55TLA
048800971114     C                   CALL      'FNLV55R'
048900020809     C                   PARM                    FNLV55DS
049000971114     C                   ENDIF
049100911016     C*
049200020809     C                   EVAL      *INLR = *ON
049300910322     C*
049400911107     C*--- CARICO TABELLE --------------------------------------------*
049500911107     C     CARTAB        BEGSR
049600921022     C*
049700921022     C* PRENDO TABELLA VARIABILI ARRIVI
049800921022     C                   MOVEL     '7A'          COD
049900921022     C                   MOVEL     *BLANKS       KEY
050000921022     C                   MOVEL     '1'           KEY
050100921022     C     KTAB          CHAIN     TABEL                              33
050200921022     C  N33              MOVEL     TBLUNI        DS7A
050300950130     C   33              CLEAR                   DS7A
050400010110     C* RICERCO FASE
050500921022     C                   MOVEL     '7N'          COD
050600960424     C                   MOVEL(P)  2             KEY
050700921022     C     KTAB          CHAIN     TABEL                              31
050800921022     C  N31              MOVEL     TBLUNI        DS7N
050900980623     C* CARICO TIPI TRAINO DEFLUENZA
051000980623     C                   MOVEL     'TV'          COD
051100020809     C                   CLEAR                   XX
051200980623     C     KTAB2         SETLL     TABEL
051300980623     C     KTAB2         READE     TABEL                                  31
051400980623     C     *IN31         DOWEQ     *OFF
051500980623     C     TBLFLG        IFEQ      ' '
051600980623     C                   MOVEL     TBLUNI        DSTV
051700980623     C     §TVDEF        IFEQ      'S'
051800020809     C                   ADD       1             XX
051900020809     C                   MOVEL     TBLKEY        TDF(XX)
052000980623     C                   ENDIF
052100980623     C                   ENDIF
052200980623     C**
052300980623     C     KTAB2         READE     TABEL                                  31
052400980623     C                   ENDDO
052500000622     C*
052600000622     C* CARICO IN SCHIERA ELENCO DIVISE ACCETTATE IN ITALIA
052700000622     C*
052800000622     C                   MOVEL     'CV'          COD
052900000622     C                   CLEAR                   DVA
053000011025     C                   CLEAR                   DEC
053100020809     C                   CLEAR                   XX
053200000622     C     KTAB2         SETLL     TABEL
053300000622     C     KTAB2         READE     TABEL                                  31
053400000622     C     *IN31         DOWEQ     *OFF
053500000622     C     TBLFLG        IFEQ      ' '
053600000622     C                   MOVEL     TBLUNI        DSCV
053700000622     C     §CVDVA        IFEQ      'S'
053800020809     C                   ADD       1             XX
053900020809     C                   MOVEL     TBLKEY        DVA(XX)
054000011025      * Carico in schiera il numero d decimali della divisa
054100020809     C                   Z-ADD     §CVDEC        DEC(XX)
054200000622     C                   ENDIF
054300000622     C                   ENDIF
054400000622     C**
054500000622     C     KTAB2         READE     TABEL                                  31
054600000622     C                   ENDDO
054700011029      *
054800020809     C                   CLEAR                   TIBS02DS
054900011029     C                   CLEAR                   KPJBU
055000011029     C                   MOVE      'C'           T02MOD
055100011029     C                   MOVE      KNSIF         T02SIF
055200011029     C                   MOVE      'GED'         T02COD
055300011029     C                   MOVEL(P)  1             T02KE1
055400011029     C                   CALL      'TIBS02R'
055500011029     C                   PARM                    KPJBA
055600020809     C                   PARM                    TIBS02DS
055700011029    6C     T02ERR        IFEQ      *BLANKS
055800011029     C                   MOVEL     T02UNI        DGED
055900011029     C                   ELSE
056000011029     C                   CLEAR                   DGED
056100011029    6C                   ENDIF
056200011029      *
056300011029      * Recupero il numero di decimali di §GEDCN
056400011029     C                   MOVEL     'CV'          TBLCOD
056500011029     C                   MOVEL     *BLANKS       TBLKEY
056600011029     C                   MOVEL     §GEDCN        TBLKEY
056700011029     C     KTAB          CHAIN     TABEL                              31
056800011029     C  N31              MOVEL     TBLUNI        DSCV
056900011029     C   31              CLEAR                   DSCV
057000011030      * IMPORTI VARI
057100020809     C                   CLEAR                   TIBS02DS
057200011030     C                   CLEAR                   KPJBU
057300011030     C                   MOVE      'C'           T02MOD                         CONTROLLO
057400011030     C                   MOVE      KNSIF         T02SIF                         S.I.
057500011030     C                   MOVE      'GEI'         T02COD                         TABELLA PROFILI
057600011030     C                   MOVEL(P)  §GEDCN        T02KE1                         PROFILO IN K1
057700011030     C                   CALL      'TIBS02R'
057800011030     C                   PARM                    KPJBA
057900020809     C                   PARM                    TIBS02DS
058000011030    6C     T02ERR        IFEQ      *BLANKS
058100011030     C                   MOVEL     T02UNI        DGEI
058200011030     C                   ELSE
058300011030     C                   CLEAR                   DGEI
058400011030    6C                   ENDIF
058500911107     C                   ENDSR
058600911001     C**---------------------------------------------------------
058700911001     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
058800911001     C**---------------------------------------------------------
058900911001     C     CAEX          BEGSR
059000040428     c                   setoff                                       22
059100020809     C                   MOVE      *BLANK        VAR11
059200020809     C                   MOVE      *BLANK        VAR12
059300911001     C                   MOVE      ')'           VAR11
059400021008     C                   MOVEL     MBRfil        VAR11
059500911001     C                   MOVE      '))'          VAR12
059600021008     C                   MOVEL     MBRfil        VAR12
059700021008     C                   Z-ADD     55            LUNG
059800911001     C*
059900911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
060000911001     C                   SETOFF                                       20
060100950118     C                   MOVE      VAR11         CMD5                            FNBLPE0R
060200911001     C                   MOVEL     *BLANKS       COMMAN
060300020809     C                   MOVEA     CMD5(1)       COMMAN
060400911001     C                   CALL      'QCMDEXC'                            20
060500911001     C                   PARM                    COMMAN
060600911001     C                   PARM                    LUNG
060700040428     c*
060800040428     C* 20 ON - ESCO
060900040428     c                   if        *in20
061000040428     c                   seton                                          22
061100040428     C                   GOTO      ENDCAE
061200040428     c                   endif
061300911002     C* ALLOCO MEMBRO: SE NON RIESCO ESCO DA PROGRAMMA
061400911001     C                   SETOFF                                       20
061500911002     C                   MOVE      VAR12         CMD6
061600911001     C                   MOVEL     *BLANKS       COMMAN
061700911002     C                   MOVEA     CMD6(1)       COMMAN
061800911001     C                   CALL      'QCMDEXC'                            20
061900911001     C                   PARM                    COMMAN
062000911001     C                   PARM                    LUNG
062100911002     C*
062200050804     c                   if        *in20
062300050804     c                   eval      fisico='FNBLPE0R'
062400050804     C                   GOTO      ENDCAE
062500050804     c                   endif
062600911001     C* OVRDBF
062700911002     C                   SETOFF                                       20
062800911002     C                   MOVE      VAR11         CMD9
062900911001     C                   MOVEL     *BLANKS       COMMAN
063000911002     C                   MOVEA     CMD9(1)       COMMAN
063100911002     C                   CALL      'QCMDEXC'                            20
063200911001     C                   PARM                    COMMAN
063300911001     C                   PARM                    LUNG
063400050804     C   20              GOTO      ENDCAE
063500911002     C*
063600950118     C                   OPEN      FNBLPE0R
063700911001     C*
063800911002     C     ENDCAE        ENDSR
063900911002     C**---------------------------------------------------------
064000911002     C** CREO BOLLA ARRIVI
064100911002     C**---------------------------------------------------------
064200980311     C     CRTBOL        BEGSR
064300080407     c*
064400931125     C* S C R I V O   B O L L A
064500990906     C                   EXSR      RIEARB
064600000622     C* LE SEGUENTI SPECIFICHE LE ESEGUO SOLO SE BOLLA NON PRESENTE
064700000622   1AC     *IN06         IFEQ      *OFF
064800911003     C*
064900931125     C* D E T T A G L I O   C O L L I
065000950413     C*
065100990906     C                   EXSR      RIEBLT
065200931125     C*
065300080407     C* Leggo il numero dei segnacolli dal dettaglio bolle partenza
065400080407     C     KBLP2         SETLL     FNBLT01l                               32
065500921123     C*
065600921123     C* 32 OFF-  NON TROVATO IL DETTAGLIO COLLI: STAMPO ERRORE
065700950118    2C     *IN32         IFEQ      *OFF
065800080411     c     §3arbl        andne     'R'
065900090109     c     §3aurc        andne     'S'
066000921123     C                   SETON                                        07
066100921123     C                   EXSR      STAERR
066200950118   X2C                   ELSE
066300921123     C*
066400080407     C     KBLP2         READE     FNBLT01l                               32
066500990906     C*
066600950118    3C     *IN32         DOWEQ     *OFF
066700100414     C                   Z-ADD     BLTfls        ARTfls
066800100414     C                   Z-ADD     BLTNSC        ARTNSC
066900080407     c                   exsr      PesoeVol
067000931125     C*
067100920917     C* CONTROLLO SE SEGNACOLLO GIA' ESISTENTE
067200950905     C                   SETOFF                                       0408
067300950413     C     KSPP          CHAIN     FNART27L                           31
067400080407    4C     *IN31         IFEQ      *OFF
067500950413     C                   SETON                                        04
067600080407    5C     ARTDCM        IFNE      *ZEROS
067700950905     C                   SETON                                        08
067800950413     C                   EXSR      RIEBLT
067900100414     C                   Z-ADD     BLTfls        ARTfls
068000950418     C                   Z-ADD     BLTNSC        ARTNSC
068100080407     c                   exsr      PesoeVol
068200950413     C* Segnacollo già esistente: se già consegnato aggiorno il record
068300950413     C                   UPDATE    FNART000
068400040317     c   03              exsr      ctrdet
068500040317     c
068600080407   x5C                   ELSE
068700040317     c
068800950418     C                   EXSR      RIEBLT
068900100414     C                   Z-ADD     BLTfls        ARTfls
069000950418     C                   Z-ADD     BLTNSC        ARTNSC
069100950413     C                   UNLOCK    FNART27L
069200080407    5C                   END
069300950413     C* Stampo errore
069400950413     C                   EXSR      STAERR
069500080407   x4C                   ELSE
069600980311     C*
069700951011     C                   WRITE     FNART000
069800031202     c   03              exsr      ctrdet
069900080407    4C                   END
070000911014     C*
070100080407     C     KBLP2         READE     FNBLT01l                               32
070200950118    3C                   ENDDO
070300950118    2C                   ENDIF
070400911003     C*
070500000622   1AC                   ENDIF
070600911003     C* SCRIVO FILE
070700000622     C  N06              WRITE     FNARB000
070800000622     C   06              UPDATE    FNARB000
070900140513     c                   if        arbffd <> *blanks
071000140513     c* richiamo pgm per eventuale scrittura file spia per invio alert
071100140513     c                   clear                   trtcp2ds
071200140513     c                   eval      t2chi= 'F'
071300140513     c                   eval      T2AAS=arbaas
071400140513     c                   eval      T2LNP=arblnp
071500140513     c                   eval      T2NRS=arbnrs
071600140513     c                   eval      T2NSP=arbnsp
071700140513     c* passo in t2fgs la lna (serve per far funzionare il chiodo nel trtcp2r)
071800140513     c                   eval      T2fgs=arblna
071900140513     c                   call      'TRTCP2R'
072000140513     c                   parm                    trtcp2ds
072100140513     c                   endif
072200080407     C*
072300911003     C* DELETO
072400000622     C     *IN06         IFEQ      *OFF
072500950118     C                   DELETE    FNBLPE00
072600000622     C                   ELSE
072700000622     C                   EXSR      DLTRIC
072800000622     C                   ENDIF
072900960424     C**
073000960424     C* CANCELLO EVENTUALE ANOMALIA 12
073100960424     C                   Z-ADD     12            COMCAA
073200990510     C     KANM1         CHAIN     FNANM01L                           33
073300960424     C* CHIUDO ANOMALIA
073400960424    1C     *IN33         IFEQ      *OFF
073500020809     C                   CLEAR                   FNLR33DS
073600960424     C                   Z-ADD     ANMNR1        D33NRR
073700960424     C                   MOVE      DATEU         D33DCH
073800960424     C                   MOVEL     §7NFS1        D33FSC
073900960424     C                   CALL      'FNLR33R'
074000020809     C                   PARM                    FNLR33DS
074100960424     C                   ENDIF
074200921022     C*
074300000622     C     *IN06         IFEQ      *OFF
074400000111     C     BLPLNA        CHAIN     AZORG01L                           32
074500020218     C  N32              MOVEL     ORGDE3        OG143
074600020218     C   32              CLEAR                   OG143
074700020226     c                   eval      lnantw = §ogntw
074800921022     C* CONTROLLO SE C'ERANO ANOMALIE MANCA BOLLA
074900960827     C*           E ANOMALIE 050-COLLO ARR. NON PART. SU BOLLE LOCALI
075000950123     C     KBLP2         SETLL     FNART001
075100020226     C     lnaNTW        IFNE      'EEX'
075200020226     C     lnaNTW        ANDNE     'EUP'
075300020226     C     lnaNTW        ANDNE     'FED'
075400060322     C     lnaNTW        ANDNE     'DPD'
075500961121     C     KBLP2         READE(N)  FNART001                               32
075600961121     C                   ELSE
075700961121     C     KBLP2         READE     FNART001                               32
075800961121     C                   ENDIF
075900921022     C*
076000961121     C     *IN32         DOWEQ     *OFF
076100950128     C                   EXSR      CTRANM
076200960827     C* . . . . . BOLLA LOCALE , SERIE>0 , FLS *NE LNP
076300960827     C     *IN03         IFEQ      *ON
076400960827     C     ARTNRS        ANDGT     *ZEROS
076500960827     C     ARTFLS        ANDNE     ARTLNP
076600960827     C                   EXSR      ANM050
076700960827     C                   END
076800961121     C**
076900000111     C* SE LNA ESTERA POSSO AGGIORNARE IL RECORD
077000020226     C     lnaNTW        IFNE      'EEX'
077100020226     C     lnaNTW        ANDNE     'EUP'
077200020226     C     lnaNTW        ANDNE     'FED'
077300060322     C     lnaNTW        ANDNE     'DPD'
077400961121     C     KBLP2         READE(N)  FNART001                               32
077500961121     C                   ELSE
077600961121     C     KBLP2         READE     FNART001                               32
077700921022     C                   END
077800961121     C**
077900961121     C                   ENDDO
078000000622     C                   ENDIF
078100911002     C*
078200911002     C                   ENDSR
078300080407     C**---------------------------------------------------------
078400080407     c* imposto peso e volume della partenza sul dettaglio colli
078500080407     C**---------------------------------------------------------
078600080407     c     PesoeVol      BEGSR
078700080407     c                   z-add     bltvuc        artvuc
078800080407     c                   movel     bltfvc        artfvc
078900080407     c                   z-add     bltpuc        artpuc
079000080407     c                   movel     bltfpc        artfpc
079100080407     c                   select
079200080407     c                   when      artvuc>0 and artpuc>0
079300080407     c                   eval      arbfbs='E'
079400080423     c                   when      artvuc>0   and arbfbs=' '
079500080407     c                   eval      arbfbs='V'
079600080423     c                   when      artpuc>0   and arbfbs=' '
079700080407     c                   eval      arbfbs='P'
079800080423     c                   other
079900080423     c                   eval      arbfbs='E'
080000080407     c                   endsl
080100080407     c                   ENDSR
080200911002     C**---------------------------------------------------------
080300911002     C** RIEMPO CAMPI FLARB00F
080400911002     C**---------------------------------------------------------
080500911002     C     RIEARB        BEGSR
080600000622     C* DEVO CLEARARE FORMATO FNARB SOLO SE BOLLA NON PRESENTE
080700000622     C  N06              CLEAR                   FNARB000
080800931125     C*
080900950118     C     BLPCBO        IFNE      SAVCBO
081000920603     C                   MOVEL     '3A'          COD
081100920603     C                   MOVEL     *BLANKS       KEY
081200950118     C                   MOVEL     BLPCBO        KEY
081300920603     C     KTAB          CHAIN     TABEL                              33
081400920603     C  N33              MOVEL     TBLUNI        DS3A
081500920603     C   33              MOVEL     'F1'          §3ATB1
081600920625     C   33              MOVEL     '  '          §3ATB2
081700950118     C                   MOVEL     BLPCBO        SAVCBO            2
081800950118     C                   ENDIF
081900920603     C*
082000911002     C                   MOVEL     BLPLNP        ARBLNP
082100950118     C                   MOVE      BLPAAS        ARBAAS
082200911002     C                   MOVEL     BLPMGS        ARBMGS
082300911002     C                   MOVEL     BLPNRS        ARBNRS
082400911002     C                   MOVEL     BLPNSP        ARBNSP
082500010110     C* L'IMPOSTAZIONE DEL CODICE BOLLA SE STO RICEVENDO UN AGGIORNAMEN
082600010110     C* TO DI BOLLA AUTOGENERATA LA FACCIO QUANDO GESTISCO IL CONTRAS-
082700010110     C* SEGNO
082800010110     C  N06              MOVEL     BLPCBO        ARBCBO
082900950118     C                   MOVEL     BLPFDN        ARBFDN
083000911002     C                   MOVEL     BLPFNS        ARBFNS
083100911002     C                   MOVEL     BLPLNA        ARBLNA
083200911002     C                   MOVEL     BLPKSC        ARBKSC
083300911002     C                   MOVEL     BLPZNC        ARBZNC
083400160513     c     kblp2         chain     fnblp01l
083500160513     c                   if        %found(fnblp01l)
083600160513     C                   MOVEL     w_BLPZNC      ARBZNC
083700160513     c                   endif
083800911002     C                   MOVEL     BLPRSM        ARBRSM
083900911002     C                   MOVEL     BLPINM        ARBINM
084000911002     C                   MOVEL     BLPLOM        ARBLOM
084100911002     C                   MOVEL     BLPPRM        ARBPRM
084200950118     C                   MOVEL     BLPCAM        ARBCAM
084300950118     C                   MOVEL     BLPNZM        ARBNZM
084400911002     C                   MOVEL     BLPRSD        ARBRSD
084500911003     C                   MOVEL     BLPIND        ARBIND
084600911002     C                   MOVEL     BLPLOD        ARBLOD
084700911002     C                   MOVEL     BLPPRD        ARBPRD
084800950118     C                   MOVEL     BLPCAD        ARBCAD
084900950118     C                   MOVEL     BLPNZD        ARBNZD
085000911002     C                   MOVEL     BLPGC1        ARBGC1
085100911002     C                   MOVEL     BLPGC2        ARBGC2
085200911002     C                   MOVEL     BLPCTR        ARBCTR
085300911002     C                   MOVEL     BLPCTS        ARBCTS
085400911002     C                   MOVEL     BLPFTM        ARBFTM
085500950118     C                   MOVEL     BLPFIN        ARBFIN
085600911002     C                   MOVEL     BLPFAP        ARBFAP
085700911002     C                   MOVEL     BLPTSP        ARBTSP
085800950118     C                   Z-ADD     BLPIAS        ARBIAS
085900950123     C                   MOVEL     BLPVAS        ARBVAS
086000911002     C                   MOVEL     BLPNAS        ARBNAS
086100911002     C                   MOVEL     BLPNCL        ARBNCL
086200950118     C                   Z-ADD     BLPPKB        ARBPKB
086300950118     C                   Z-ADD     BLPPKF        ARBPKF
086400000622     C* SE BOLLA GIA' PRESENTE SOSTITUISCO SOLO SE IL VOLUME SU ARB
086500000622     C* NON E' NE' "T" NE' "Z" E SE IL VOLUME RICEVUTO E' > DI QUELLO
086600000622     C* SU ARB
086700000622     C     *IN06         IFEQ      *OFF
086800000622     C     ARBFVF        ORNE      'T'
086900000622     C     ARBFVF        ANDNE     'Z'
087000000622     C     BLPVLF        ANDGT     ARBVLF
087100950118     C                   Z-ADD     BLPVLF        ARBVLF
087200950118     C                   MOVEL     BLPFVF        ARBFVF
087300000622     C                   ENDIF
087400950118     C                   Z-ADD     BLPVLB        ARBVLB
087500950118     C                   MOVEL     BLPFVB        ARBFVB
087600950118     C                   MOVEL     BLPQFT        ARBQFT
087700950118     C                   MOVEL     BLPVMD        ARBVMD
087800950118     C                   MOVEL     BLPVAD        ARBVAD
087900070222     C*
088000911002     C                   MOVEL     BLPCCM        ARBCCM
088100950118     C                   Z-ADD     BLPRMN        ARBRMN
088200950118     C                   MOVEL     BLPRMA        ARBRMA
088300911002     C                   MOVEL     BLPRMO        ARBRMO
088400911002     C                   MOVEL     BLPFLS        ARBFLS
088500911002     C                   MOVEL     BLPNCD        ARBNCD
088600911002     C                   MOVEL     BLPNCA        ARBNCA
088700911002     C                   MOVEL     BLPXCO        ARBXCO
088800950118     C*
088900911002     C                   MOVEL     BLPFFD        ARBFFD
089000950118     C                   MOVE      BLPDCR        ARBDCR
089100911002     C                   MOVEL     BLPHCR        ARBHCR
089200911002     C                   MOVEL     BLPTCR        ARBTCR
089300950118     C                   MOVE      BLPDBR        ARBDBR
089400931215     C                   MOVEL     BLPNFV        ARBNFV
089500950124     C*
089600911002     C                   MOVEL     BLPFLE        ARBFLE
089700911002     C                   MOVEL     BLPTFA        ARBTFA
089800911002     C                   MOVEL     BLPTFP        ARBTFP
089900911002     C                   MOVEL     BLPFEA        ARBFEA
090000911008     C                   MOVEL     BLPFBR        ARBFBR
090100950118     C                   MOVEL     BLPTC1        ARBTC1
090200950118     C                   MOVEL     BLPTC2        ARBTC2
090300950118     C                   MOVEL     BLPGVA        ARBGVA
090400950118     C                   MOVEL     BLPGMA        ARBGMA
090500950118     C                   MOVEL     BLPGGA        ARBGGA
090600001204     C*  SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA N FT1
090700051116     C* BLANK NON LO DEVO AGGIORNARE ALTRIMENTI NON VENGONO TRASMESSE
090800001204     C* LE DATE DI CONSEGNA
090900001204     C     *IN06         IFEQ      *OFF
091000001204     C     ARBFT1        ORNE      *BLANKS
091100931125     C                   MOVEL     'R'           ARBFT1
091200950118     C                   Z-ADD     DATEU         ARBDT1
091300001204     C                   ENDIF
091400970212     C*
091500060220     c* Imposto nel tipo record "A" (se non c'e' lo creo,)
091600060220     C*   IL 1° BYTE DEL CAMPO AR4NOT  E §B4ASO=SIGLA OPERATORE
091700060302     c                   clear                   contaerror
091800060220     c                   eval      wtrc='A'
091900060302     c
092000060302     c* Se errore in write (succede per conflitto con fnlrc1r)
092100060302     c*  ritento l'update
092200060302    1c                   dou       not *in99
092300060302     c                   setoff                                       99
092400060220     c     kar4          chain     fiar401l
092500060302     c                   exsr      Scriviar4A
092600060302     c
092700060302     c* dopo 10 tentativi mando un messaggio in posta ed esco senza
092800060302     c*  impostare il record A
092900060302     c                   add       1             contaerror        2 0
093000060302    2c                   if        contaerror>10
093100060302     C
093200111104     c* Mando e_mail
093300111104     c                   clear                   msgtes
093400111104     c                   clear                   msgdet
093500111104     c                   eval      msgtes='RICEZIONE BOLLE: FNLR14R'
093600111104     c                   eval      msgdet='Non scritto record"A"   -  utente'+
093700111104     c                               KNMUS + '   bolla :' +
093800111104     c                               (%editc(arbaas:'Z'))
093900060302     c                             + (%editc(arblnp:'Z')) + (%editc(arbnrs:'2'))
094000060302     c                             + (%editc(arbnsp:'2'))
094100120305     C                   eval      wrkEml= 'ced@brt.it'
094200111104     C                   call(e)   'TIS701C'
094300111104     C                   parm                    wrkEml          253
094400111104     C                   PARM                    MSGTES           44
094500111104     C                   PARM                    MSGDET         5000
094600060302     C*
094700060302     c                   leave
094800060302    2c                   endif
094900060302    1c                   enddo
095000060302     c
095100060220     c
095200060220     c* Cerco partita iva da impostare su fnarb
095300060220     c                   eval      wtrc='P'
095400060220     c     kar4          chain(N)  fiar401l
095500060220     c                   if        %found(fiar401l)
095600060220     C                   MOVEL     §3ATB1        UNOTB1            1
095700060220     C     UNOTB1        IFEQ      'F'
095800060220     C     §3ATB2        ANDEQ     *BLANKS
095900060220     C                   MOVEL     AR4NOT        ARBCPI
096000060220     C                   ELSE
096100060220     C                   MOVE      AR4NOT        ARBCPI
096200060220     C                   ENDif
096300061031
096400061031     c                   else
096500061031      * se non c'è il fiar4 rcd P aggancio la bolla partenza se bolla unica
096600061031     c                   if        §3atb2 = *blanks
096700061031     c     kblp2         chain     fnblp01l
096800061031     c                   if        %found(fnblp01l)
096900061031     c                   eval      arbcpi = w_blpcpi
097000061031     c                   endif
097100061031     c                   endif
097200061031
097300060220     C                   ENDif
097400030109      *
097500930201     C* A B I L I T A Z I O N E   M E R C E
097600931125     C**
097700000622     C*
097800930201     C* SE FOGLIO VIAGGIO 99999 ABILITO SUBITO
097900000623     C  N06BLPNFV        IFEQ      99999
098000930201     C                   MOVEL     3             ARBAMA
098100950919     C                   ENDIF
098200931125     C**
098300931125     C* A B I L I T A Z I O N E   C / A S S E G N O
098400000623     C*
098500911002     C* PER BOLLE SENZA CONTRASSEGNO
098600051116     c* Vedo se essite C/A per abilitazione
098700051116     c     kblp2         chain     fiar901l
098800051116     c                   if        not %found(fiar901l) or ar9cas=0
098900911002     C                   MOVEL     'S'           ARBACC
099000950119   X1C                   ELSE
099100011211
099200011210      * ------------------
099300011210
099400011210      * NUOVO CONTROLLO C/ASSEGNO
099500011210     c                   clear                   trul21ds
099600011210     c                   eval      i21tla = 'L'
099700011210     c                   eval      i21cbo = blpcbo
099800011210     c                   eval      i21tsp = blptsp
099900011210     c                   eval      i21lnp = blplnp
100000011210     c                   eval      i21nzm = blpnzm
100100011210     c                   eval      i21lna = blplna
100200011210     c                   eval      i21nzd = blpnzd
100300051116     c                   eval      i21tic = ar9tic
100400051116     c                   eval      i21imp = ar9cas
100500051116     c                   eval      i21div = ar9vca
100600011210     c                   eval      i21ute = knmus
100700011210     c                   eval      i21pgm = nompgm
100800130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
100900130827     c                   if        %subst(§3atb1:1:1)='F'
101000130827     c                   z-add     blpksc        i21ksc
101100130827     c                   else
101200130827     c                   z-add     blpccm        i21ksc
101300130827     c                   if        i21ksc=0
101400130827     c                   z-add     8888          i21ksc
101500130827     c                   movel     blplnp        i21ksc
101600130827     c                   endif
101700130827     c                   endif
101800130827     c
101900011210     c                   call      'TRUL21R'
102000011210     c                   parm                    trul21ds
102100011210
102200011210     c                   if        o21fca = *blanks
102300011210     c                   eval      arbacc = 'S'
102400011210     c                   endif
102500911002     C*
102600950119    1C                   END
102700931125     C**
102800950119     C* A B I L I T A Z I O N E    A S S E G N A T O
102900931125     C**
103000000623   1AC     *IN06         IFEQ      *OFF
103100000623     C*
103200911003     C                   MOVEL     §3ATB1        UNOTB1            1
103300911003     C* FRANCO
103400950119    1C     UNOTB1        IFEQ      'F'
103500930112     C* SE NON C'E' SECONDA BOLLA ABILITO
103600950119    2C     §3ATB2        IFEQ      '  '
103700911003     C                   MOVEL     'S'           ARBACA
103800950119    2C                   END
103900950119    1C                   END
104000050314     c
104100050314     c* Se già tassata, abilito subito l'assegnato
104200050314     c                   if        arbaca=' '
104300050314     c                   if        §3atb2<>*blanks
104400050314     c                   eval      ktrc='2'
104500050314     c                   else
104600050314     c                   eval      ktrc='1'
104700050314     c                   endif
104800050314     c
104900050314     c     kar6          chain     fiar601l
105000050314     c                   if        %found(fiar601l)
105100050314     c                   move      ar6ksc        fineksc
105200050314     c                   if        fineksc<>'9999' and fineksc<>'0000'
105300050314     c                   eval      arbaca='S'
105400050314     c* mi salvo i campi da inmpostare in arbksc e arbctr
105500050314     c                   eval      arbksc=ar6ksc
105600050314     c                   eval      arbctr=ar6ctr
105700050331     c* Se si tratta di bolla di recupero, devo anche chiudere la spedizione
105800050331    3C     §3ATB1        IFEQ      'A3'
105900050331     C* IMPOSTO I CAMPI DELLA CHIUSURA
106000050331     C                   MOVEL     'Y'           ARBICC
106100050331     C                   MOVEL     'S'           ARBICA
106200050331     C                   Z-ADD     DATEU         ARBDCM
106300050331     C                   time                    wora              6 0
106400050331     C                   movel     wora          ARBHMC
106500050331     C                   CLEAR                   ARBFT1
106600050331     C                   CLEAR                   ARBDT1
106700050331     C                   MOVEL     *ALL'9'       ARBNDC
106800050331     C                   Z-ADD     DATEU         ARBDDC
106900050331     C                   MOVEL     'S'           ARBFDC
107000050331     C                   MOVEL     *ALL'9'       ARBNMI
107100050331     C                   Z-ADD     DATEU         ARBDMI
107200050331     C                   MOVEL     *ALL'9'       ARBCUC
107300050331     C                   CLEAR                   ARBFBC
107400050331    3C                   ENDIF
107500050314     c                   endif
107600050314     c                   endif
107700050314     c                   endif
107800050314     c
107900000623   1AC                   ENDIF
108000070322
108100070322      * se LNA o sua capofila abilitata alle telefonate per GEO
108200070329      *  e NO bolle di recupero
108300070329     c                   if        lnageo = 'S' and §3arbl <> 'R'
108400070322     c                   clear                   dsbl4l
108500070322      * Genero record blocco spedizione se:
108600070322      * --> Consegna particolare Appuntamento o Supermercato (non tassative)
108700070322     c                   if        arbtc1 = 'A' or arbtc1 = 'S' or arbtc2 = 'A'
108800070322     c                             or arbtc2 = 'S'
108900070322     c                   eval      §b4lbas = 'S'
109000070322      * se consegna tassativa sblocco
109100070322     c                   if        arbdcr <> *zeros and arbtcr = *blanks
109200070322     c                   clear                   §b4lbas
109300070322     c                   endif
109400070322     c                   endif
109500070322      * --> Contrassegno non abilitato
109600070322     c                   if        arbacc = *blanks
109700070322     c                   eval      §b4ltca = 'S'
109800070322     c                   endif
109900070322      * --> Fermo deposito
110000070322     c                   if        arbffd <> *blanks
110100070322     c                   eval      §b4ltfd = 'S'
110200140212     c* richiamo pgm per eventuale scrittura file spia per invio alert
110300140513     c***                clear                   trtcp2ds
110400140513     c***                eval      t2chi= 'F'
110500140513     c***                eval      T2AAS=blpaas
110600140513     c***                eval      T2LNP=blplnp
110700140513     c***                eval      T2NRS=blpnrs
110800140513     c***                eval      T2NSP=blpnsp
110900140213     c* passo in t2fgs la lna (serve per far funzionare il chiodo nel trtcp2r)
111000140513     c***                eval      T2fgs=blplna
111100140513     c***                call      'TRTCP2R'
111200140513     c***                parm                    trtcp2ds
111300070322     c                   endif
111400070322      * scrivo record L di fiar4 per la gestione dei blocchi
111500070322     c                   if        dsbl4l <> *blanks
111600070322     c                   clear                   fiar4000
111700070322     c                   eval      ar4aas = blpaas
111800070322     c                   eval      ar4lnp = blplnp
111900070322     c                   eval      ar4nrs = blpnrs
112000070322     c                   eval      ar4nsp = blpnsp
112100070322     c                   eval      ar4trc = 'L'
112200070322     c                   eval      ar4not = dsbl4l
112300070322     c                   eval      ar4ftr = 'R'
112400070322     c                   eval      ar4dtr = dateu
112500070322     c                   eval      ar4duv = dateu
112600070322     c                   write     fiar4000                             99
112700070322     c                   endif
112800070322     c                   endif
112900950118     C*
113000911002     C                   ENDSR
113100070322      *-----------------------------------------------------------------------
113200060302     c     Scriviar4A    BEGSR
113300060302     c                   if        %found(fiar401l)
113400060302     c                   movel     ar4not        dsbl4a
113500060302     c                   else
113600060302     C                   MOVE      BLPAAS        AR4AAS
113700060302     C                   MOVEL     BLPLNP        AR4LNP
113800060302     C                   MOVEL     BLPNRS        AR4NRS
113900060302     C                   MOVEL     BLPNSP        AR4NSP
114000060302     C                   MOVEL     'A'           AR4TRC
114100060302     C                   CLEAR                   AR4NOT
114200060302     C                   CLEAR                   DSBL4A
114300060302     c                   movel     'R'           ar4ftr
114400060302     c                   movel     dateu         ar4DTR
114500060302     c                   endif
114600060302     c
114700060302     c                   movel     dateu         ar4duv
114800120604     c* ES --> dal momento che non stampiamo più i DDT ma escono con le distinte,
114900120604     c*        non serve più impostare il falg stampa DDT ma lo tengo come inviato
115000120604     c*        dalla partenza
115100060302     C                   MOVEL     BL2SV1        §B4ABM
115200060302
115300060302      * se linea di arrivo con picking attivo o lna per cui stampare i
115400060302      * DDT SI in arrivo
115500120604if  1c**                 If        lnapks = 'S'
115600120604 s  2c**                 Select
115700060302
115800060302      * per bolla di recupero
115900060302      * imposto che la bolla è già stata stampata
116000120604     c**                 When         §3arbl = 'R'
116100120604     c**                 If        Bl2Sv1 = 'S'
116200120604     c**                 Eval      §B4Abm = 'P'
116300120604     c**                 EndIf
116400120604     c**                 If        Bl2Sv1 = 'C'
116500120604     c**                 Eval      §B4Abm = 'Q'
116600120604     c**                 EndIf
116700120604     c**                 If        Bl2Sv1 = 'N' or Bl2Sv1 = 'Y'
116800120604     c**                 Eval      §B4Abm = 'J'
116900120604     c**                 Endif
117000120604     c**                 If        Bl2Sv1 = 'K'
117100120604     c**                 Eval      §B4Abm = 'W'
117200120604     c*+                 Endif
117300060302
117400060302      * se bl2sv1 = 'Y'
117500060302      * imposto 'N' x bolla ancora da stampare
117600120604     c**                 When      Bl2Sv1 = 'Y'
117700120604     c**                 Eval      §B4Abm = 'N'
117800060302
117900120604    2c**                 Endsl
118000120604    1c**                 Endif
118100060302
118200120604     C**   §7AFSR        IFEQ      'Y'
118300120604     C**   BL2SV1        ANDEQ     'Y'
118400120604     C**   lnpNTW        ANDNE     'DPD'
118500120604     C**                 MOVEL     'N'           §B4ABM
118600120604     C**                 ENDIF
118700120604     c*
118800060302     c
118900060302     c* Sigla operatore video
119000060302     C                   MOVE      BL2CEI        §B4ASO
119100060302     C                   MOVEL     DSBL4A        AR4NOT
119200060302     c                   if        %found(fiar401l)
119300060302     c                   update    fiar4000
119400060302     c                   else
119500060302     C                   WRITE     FiAR4000                             99
119600060302     c                   endif
119700060302     c                   endsr
119800911002     C**---------------------------------------------------------
119900911002     C** RIEMPO DETTAGLIO SEGNACOLLI
120000911002     C**---------------------------------------------------------
120100911002     C     RIEBLT        BEGSR
120200950119     C                   CLEAR                   FNART000
120300950118     C                   Z-ADD     ARBAAS        ARTAAS
120400931125     C                   Z-ADD     BLPLNP        ARTLNP
120500911002     C                   Z-ADD     BLPNRS        ARTNRS
120600911002     C                   Z-ADD     BLPNSP        ARTNSP
120700911002     C                   MOVEL     BLPLNA        ARTLNA
120800911002     C                   MOVEL     BLPFLS        ARTFLS
120900950119     C                   MOVEL     'R'           ARTFTR
121000960712     C* SE COLLO LOCALE INDICATO CHE E' ABBINATO
121100960712     C     *IN03         IFEQ      *ON
121200960712     C                   MOVEL     'S'           ARTABN
121300960712     C                   ENDIF
121400950119     C                   ENDSR
121500911014     C**---------------------------------------------------------
121600960424     C** CONTROLLO SE ESISTE ANOMALIA 10-NO REC BOLLA E 12-
121700911014     C**---------------------------------------------------------
121800950128     C     CTRANM        BEGSR
121900960529     C                   Z-ADD     0             OFV               5 0
122000970521     C*
122100970521     C* CHIUDO ANOMALIA 410 SE ESISTE
122200970521     C                   Z-ADD     410           COMCAA
122300990510     C     KANM          CHAIN     FNANM000                           33
122400970521    1C     *IN33         IFEQ      *OFF
122500020809     C                   CLEAR                   FNLR33DS
122600970521     C                   Z-ADD     ANMNR2        D33NRR
122700970521     C                   MOVE      DATEU         D33DCH
122800970521     C                   MOVEL     §7NFS1        D33FSC
122900970521     C                   CALL      'FNLR33R'
123000020809     C                   PARM                    FNLR33DS
123100970521    1C                   END
123200950128     C*
123300960424     C                   Z-ADD     10            COMCAA
123400990510     C     KANM          CHAIN     FNANM000                           33
123500950128     C* CHIUDO ANOMALIA
123600950128    1C     *IN33         IFEQ      *OFF
123700000623     C*
123800020809     C                   CLEAR                   FNLR33DS
123900960415     C                   Z-ADD     ANMNR2        D33NRR
124000960415     C                   MOVE      DATEU         D33DCH
124100960415     C                   MOVEL     §7NFS1        D33FSC
124200960415     C                   CALL      'FNLR33R'
124300020809     C                   PARM                    FNLR33DS
124400960415     C*
124500950128     C* ESEGUO LA CALL AL PGM DI CARICAMENTO SPUNTE PER AGGIORNARE
124600951213     C*  LA BOLLA. NON LO FACCIO SE E' LOCALE
124700961121    2C     *IN98         IFEQ      *OFF
124800951213     C     *IN03         ANDEQ     *OFF
124900021015     c                   Eval      Pa5Mbr = MbrFil
125000021015     c                   Movel     Pa5Mbr4       PA5FIL
125100950128     C                   MOVEL     'F'           PA5FIL
125200950128     C                   MOVE      PARAM5        KPJBU
125300950203     C                   MOVEL     'LR22'        KCOAZ
125400950203     C                   CALL      'BCH10'
125500950203     C                   PARM                    KPJBA
125600950203     C                   SETON                                        98
125700961121    2C                   ENDIF
125800070831     c
125900070831     c* Se esiste anomalia di collo arrivato non partito
126000070831     c*  chiano in update il file fnart e aggiorno i dati della partenza
126100070831     C                   Z-ADD     050           COMCAA
126200070831     C     KANM          CHAIN     FNANM000                           33
126300070831    2C     *IN33         IFEQ      *OFF
126400070831     c     kart          chain     fnart01l
126500070831    3c                   if        %found(fnart01l)
126600070831     c* Se manca data partenza la aggiorno, altrimenti data uscita transito
126700070831    4c                   if        anmcdu=arbtfp
126800070831     C                   Z-ADD     anmdao        ARTDFV
126900070831     C                   Z-ADD     anmnfv        ARTnFV
127000070831     C                   Z-ADD     88            ARTNPP
127100070831    5c                   if        arbtfp=arbtfa
127200070831     C                   Z-ADD     90            ARTNPP
127300070831    5c                   endif
127400070831     C                   movel     'S'           ARTABN
127500070831   x4c                   else
127600070831     C                   Z-ADD     anmdao        ARTDUT
127700070831     C                   Z-ADD     89            ARTPUT
127800070831     C                   Z-ADD     anmcdu        ARTFLP
127900070831     C                   movel     'S'           ARTABN
128000070831    4c                   endif
128100070831     c
128200070831    4c                   if        lnantw<>'EEX' and lnantw<>'EUP' and
128300070831     c                             lnantw<>'FED' and lnantw<>'DPD'
128400070903     c                   update    fnart001
128500070831    4c                   endif
128600070831    3c                   endif
128700070831    2c                   endif
128800961122     C**
128900000111     C** SE LNA ESTERA CONTROLLO SE ESISTE EVENTO COLLO 2 /6A/6
129000020226    2C     lnaNTW        IFEQ      'EEX'
129100020226    2C     lnaNTW        OREQ      'EUP'
129200020226    2C     lnaNTW        OREQ      'FED'
129300060322    2C     lnaNTW        OREQ      'DPD'
129400961122     C                   CLEAR                   WDEV
129500961122     C                   CLEAR                   WCEV
129600961122     C* APRO IL FILE
129700961122    3C     WAPER         IFEQ      ' '
129800961122     C                   OPEN      FNEVS02L
129900961122     C                   MOVEL     'S'           WAPER             1
130000961122    3C                   ENDIF
130100961122     C**
130200961122     C                   MOVEL     'E2 '         KCEV
130300020809    3C                   DO        3             XX
130400961122     C     KEVS          SETLL     FNEVS02L
130500961122     C     KEVS          READE     FNEVS02L                               33
130600961122    4C     *IN33         DOWEQ     *OFF
130700970602    5C     EVSDEV        IFGT      WDEV
130800961122     C     WDEV          OREQ      0
130900961122     C                   MOVEL     EVSDEV        WDEV
131000961122     C                   MOVEL     EVSCEV        WCEV
131100961122    5C                   ENDIF
131200961122     C**
131300961122     C* AGGIORNO L'EVENTO COL NUMERO DI SPEDIZIONE
131400961122     C                   Z-ADD     BLPAAS        EVSAAS
131500961122     C                   Z-ADD     BLPLNP        EVSLNP
131600961122     C                   Z-ADD     BLPNSP        EVSNSP
131700961122     C                   CLEAR                   EVSFTR
131800961122     C                   CLEAR                   EVSDTR
131900961122     C                   UPDATE    FNEVS000
132000961122     C*
132100961122     C     KEVS          READE     FNEVS02L                               33
132200961122    4C                   ENDDO
132300961122     C**
132400961122     C* AL 2 GIRO CERCO LE DISCOR.6  --> COLLO ARRIVATO MA A TERRA
132500020809    4C     XX            IFEQ      1
132600961122     C                   MOVEL     'E6 '         KCEV
132700961122    4C                   ENDIF
132800970602     C* AL 3 GIRO CERCO LE DISCOR.6B --> COLLO ARRIVATO MA IN PARTENZA
132900020809    4C     XX            IFEQ      2
133000970602     C                   MOVEL     'E6B'         KCEV
133100961122    4C                   ENDIF
133200961122    3C                   ENDDO
133300961122     C* SE TROVATA AGGIORNO IL DETTAGLIO COLLI
133400961122    3C     WDEV          IFGT      0
133500961122     C* SE ERA COLLO RIAMSTO A TERRA ARTDTR DEVE ESSERE = 0
133600970602    4C     WCEV          IFEQ      'E6 '
133700961122     C                   CLEAR                   ARTDTR
133800961122     C                   ELSE
133900961122     C* ALTRIMENTI IMPOSTO LA DATA DELL'EVENTO
134000961122     C                   Z-ADD     WDEV          ARTDTR
134100961122    4C                   ENDIF
134200961122     C* AGGIORNO RECORD
134300961122     C                   UPDATE    FNART001
134400961122     C* DISALLOCO RECORD
134500961122   X3C                   ELSE
134600070903     C                   update    FNART001
134700961122    3C                   ENDIF
134800961122     C*
134900961122    2C                   ENDIF
135000960529     C*
135100960529     C* LEGGO LA SPUNTA PARTENZA DEL COLLO
135200040317     C*  PER REPERIRE LA DATA DEL FOGLIO VIAGGIO PARTENZA, AI FINI DI
135300040317     C*  DETERMINARE IL TERMINAL DI ARRIVO DELLA LINEA DI ARRIVO
135400040317     c*  In realtà prendiamo la data della prima spunta partenza che
135500040317     c*  troviamo del collo, che potrebbe essere anche quella di un
135600040317     c*  p.o. di transito o di disguido. Ma sono casi molto rari ed
135700040317     c*  un problema si potrebbe presentare soltanto in caso di
135800040317     c*  cambio del terminal di arrivo della lna tra il momento in
135900040317     c*  cui parte/arriva il collo, ed il momento in cui
136000040317     c*  parte/arrva la bolla -vsito che siamo nel caso di manca rec-
136100040317     c*  con il terminal di arrivo calcolato richiama LS44 solo x le
136200040317     c*  spunte dell'area di arrivo, oper aggiornare la bolla in arr
136300040317     c*  Ma se cambia terminal in base alla data spunta le aree di arr
136400040317     c*  possono essere 2. allora si dovrebbe o duplicare dei controlli
136500040317     c*  che il pgm ls44 già fa, o richimare ls44 in modo che aggiorni
136600040317     c*  solo fnarb Ma data la eccezionalità dei casi, non facciamo
136700040317     c*  nulla e teniamo il pgm come è
136800040317     c*
136900960603     C                   EXSR      DFVPAR
137000961121     C*
137100960617     C* PRENDO IL TERMINAL DI ARRIVO DELLA LNA
137200020809     C                   CLEAR                   FNLV55DS
137300971114     C                   MOVEL     'A'           D55TPT
137400971114     C                   MOVEL     ARTLNP        D55LNP
137500971114     C                   MOVEL     ARTLNA        D55LIN
137600971114     C                   MOVEL     WDFV          D55DRF
137700971114     C                   CALL      'FNLV55R'
137800020809     C                   PARM                    FNLV55DS
137900971114     C                   MOVEL     'S'           WCAL55            1
138000971114     C**
138100971114     C     D55ERR        IFNE      ' '
138200971114     C                   MOVEL     D55LIN        D55TFA
138300971114     C                   ENDIF
138400971114     C                   MOVEL     D55TFA        LNATFA
138500950130     C*
138600950130     C* CERCO UNA SPUNTA DEL DETTAGLIO COLLO
138700070123     C     KSPP          CHAIN     FNBRV07L                           33
138800960529     C*
138900961121    2C     *IN33         DOWEQ     *OFF
139000960705     C*
139100960705     C* SE CATEGORIA 1 ESCLUDO SE IL FOGLIO E' UN NORMALE: ELABORO
139200960705     C*  SOLO SE E' DEFLUENZA
139300961121    3C     BRVNPG        IFEQ      1
139400960705     C                   CLEAR                   KEY
139500960705     C                   CLEAR                   WELAB
139600960705     C*
139700000110     C                   Z-ADD     BRVNFV        KNFV
139800040317     C                   Z-ADD     BRVfgs        Kfgs
139900020809     C     K02FGV01      chain     FNFGV01L                           36
140000031203     c     *in36         ifeq      *off
140100031203     C                   MOVEL     FGVTTR        WTTR
140200031203   X4C                   ELSE
140300960705     C     KFVA          CHAIN     FNFVA01L                           36
140400980623     C                   MOVEL     FVATTR        WTTR
140500031203    4C                   ENDIF
140600960705     C*
140700980623    4C     WTTR          IFNE      *BLANKS
140800980623     C     WTTR          LOOKUP    TDF                                    36
140900980623     C  N36              MOVEL     'N'           WELAB             1
141000961121    4C                   ENDIF
141100960705     C*
141200961121    3C                   ENDIF
141300960705     C*
141400960705     C** ELABORO LA CATEGORIA 1 SOLO PER LA DEFLUENZA E LE ALTRE CATEG
141500961121    3C     BRVNPG        IFNE      1
141600960705     C     WELAB         OREQ      ' '
141700960617     C**
141800960603     C* LEGGERE LE ECCEZIONI TERMINAL
141900960603     C**
142000020809     C                   CLEAR                   FNLV55DS
142100971114     C                   MOVEL     'A'           D55TPT
142200971114     C                   MOVEL     ARTLNP        D55LNP
142300031203     C                   MOVEL     BRVPES        D55LIN
142400971114     C                   MOVEL     WDFV          D55DRF
142500971114     C                   CALL      'FNLV55R'
142600020809     C                   PARM                    FNLV55DS
142700971114     C                   MOVEL     'S'           WCAL55            1
142800971114     C**
142900971114     C     D55ERR        IFNE      ' '
143000971114     C                   MOVEL     D55LIN        D55TFA
143100971114     C                   ENDIF
143200031203     C                   MOVEL     D55TFA        PESTFA
143300960529     C*
143400031203    5C     LNATFA        IFEQ      PESTFA
143500960603     C** IMPOSTO L'AS GESTIONE DELLA ANOMALIA
143600970114     C                   CLEAR                   OFV
143700960529     C                   EXSR      CHIAMO
143800970115     C*
143900960529    5C                   ENDIF
144000961121    3C                   ENDIF
144100960529     C**
144200070123     C     KSPP          READE     FNBRV07L                               33
144300961121    2C                   ENDDO
144400961121     C**
144500961121    1C                   ENDIF
144600950130     C*
144700950130     C                   ENDSR
144800960603     C**---------------------------------------------------------
144900960603     C*  LEGGO LA SPUNTA PARTENZA DEL COLLO PER PRENDERE LA DATA FV
145000960603     C**---------------------------------------------------------
145100960603     C     DFVPAR        BEGSR
145200960603     C                   CLEAR                   WDFV
145300070123     C     KSPP          CHAIN     FNBRV07L                           34
145400960603    1C     *IN34         DOWEQ     *OFF
145500960603    2C     BRVNPG        IFEQ      1
145600960603     C*
145700000110     C                   Z-ADD     BRVNFV        KNFV
145800040317     C                   Z-ADD     BRVfgs        Kfgs
145900020809     C     K02FGV01      chain     FNFGV01L                           35
146000960603     C**
146100960603     C* ESCLUDO I TRAINI DI DEFLUENZA
146200031216    3C     *IN35         IFEQ      *OFF
146300960603     C**
146400031216    5C     FGVTTR        IFNE      ' '
146500031216     C     FGVTTR        LOOKUP    TDF                                    35
146600031216    5C                   ENDIF
146700980623     C**
146800031216    5C     FGVTTR        IFEQ      ' '
146900031216     C     *IN35         OREQ      *OFF
147000980623     C**
147100031216    6C     FGVDFV        IFGT      WDFV
147200031216     C                   MOVEL     FGVDFV        WDFV
147300031216    6C                   ENDIF
147400031216    5C                   ENDIF
147500960603     C**
147600960603   X3C                   ELSE
147700960603     C**
147800960603     C     KFVA          CHAIN     FNFVA01L                           35
147900031216    4c     *in35         ifeq      *off
148000031216    5C     FVATTR        IFNE      ' '
148100031216     C     FVATTR        LOOKUP    TDF                                    35
148200031216    5C                   ENDIF
148300031216     C**
148400031216    5C     FVATTR        IFEQ      ' '
148500031216     C     *IN35         OREQ      *OFF
148600031216    6C     FVADFV        IFGT      WDFV
148700960603     C                   MOVEL     FVADFV        WDFV
148800031216    6C                   ENDIF
148900031216    5c                   endif
149000031216    4c                   endif
149100960603     C*
149200960603    3C                   ENDIF
149300960603    2C                   ENDIF
149400070123     C     KSPP          READE     FNBRV07L                               34
149500960603    1C                   ENDDO
149600960603     C**
149700960603     C* SE NON L'HO TROVATA PRENDO LA DATA DI CREAZIONE DELLA ANOMALIA
149800960603     C     WDFV          IFEQ      0
149900960603     C                   Z-ADD     ANMDAO        WDFV
150000960603     C                   ENDIF
150100960603     C                   ENDSR
150200950130     C**---------------------------------------------------------
150300950130     C** CHAIMO PGM DI AGGIORNAMENTO BOLLE DA SPUNTA
150400950130     C**---------------------------------------------------------
150500950130     C     CHIAMO        BEGSR
150600070228     c                   clear                   fnls45ds
150700070228     c                   movel     'S'           dls45ric
150800070228     c                   move      brvpes        dls45pes
150900070228     c                   move      simfel        dls45tfp
151000070228     c                   call      'FNLS45R'
151100070228     c                   parm                    fnls45ds
151200070228     c                   parm                    dsbrv
151300950130     C*
151400950130     C                   SETON                                        02
151500950130     C                   ENDSR
151600950123     C***--------------------------------------------------------
151700080411     C** invio msg in sede
151800911003     C**---------------------------------------------------------
151900911003     C     STAERR        BEGSR
152000111104     c* Mando e_mail
152100111104     c                   clear                   msgtes
152200111104     c                   clear                   msgdet
152300111104     C                   MOVEL     MSG(1)        msgtes
152400111104     c                   eval      %subst(msgtes:26:3)=%editc(simfel:'X')
152500111104     c                   eval      %subst(msgtes:35:10)=mbrfil
152600080411
152700080411     C                   MOVEL     BLPRSM        STARSM           20
152800080603     c     kblp2         chain     fnblp01l
152900080603     c                   if        not %found(fnblp01l)
153000080603     c                   clear                   w_blpctm
153100080603     c                   endif
153200080411
153300111104     c                   eval      msgdet='Bolla: '+%editc(blpaas:'X') +' '
153400080411     c                             + %editc(blplnp:'X')+' '+%editc(blpnrs:'Z')
153500080411     c                             + ' '+%editc(blpnsp:'Z')+' LNA '
153600111104     c                             + %editc(blplna:'Z')+' Mit:'+ starsm +
153700111104     c                             '       Cod.Trattamento merce:' +W_blpctm
153800080411     c* Bolla già esistente
153900111104     c   06              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(2)
154000111104     c** 06              eval      ym5t03=msg(2)
154100080411     c* non trovato dettaglio colli
154200111104     c   07              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(3)
154300111104     c** 07              eval      ym5t03=msg(3)
154400080411     c* Segnacollo esistente
154500080411     c                   if        *in04
154600111104     c   08              eval      %subst(msg(4):22:7)=%editc(artnsc:'Z')
154700111104     c   08              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(4)
154800111104     c  N08              eval      %subst(msg(5):22:7)=%editc(artnsc:'Z')
154900111104     c  N08              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(5)
155000111104     c** 08              eval      ym5t03=msg(4)
155100111104     c**N08              eval      ym5t03=msg(5)
155200111104     c**                 eval      %subst(msgT03:22:7)=%editc(artnsc:'Z')
155300080411     c                   endif
155400080411     C*
155500120305     C                   eval      wrkEml= 'ced@brt.it'
155600111104     C                   call(e)   'TIS701C'
155700111104     C                   parm                    wrkEml          253
155800111104     C                   PARM                    MSGTES           44
155900111104     C                   PARM                    MSGDET         5000
156000080411     C*
156100000622     C                   EXSR      DLTRIC
156200000622     C                   ENDSR
156300000622     C***--------------------------------------------------------
156400000622     C** DELETO RECORD DA FILE DI RICEZIONE PER BOLLA DOPPIA
156500000622     C**---------------------------------------------------------
156600000622     C     DLTRIC        BEGSR
156700921123     C*
156800921123     C* C A N C E L L O  SOLO SE BOLLA DOPPIA
156900921123     C   06              DO
157000950118     C                   DELETE    FNBLPE00
157100920917     C                   END
157200911003     C*
157300911003     C                   ENDSR
157400040317     C*
157500951011     C* CONTROLLO SE LOCALE SE DEVO AGGIORNARE LA DATA PARTENZA------*
157600031202     C     CTRDET        BEGSR
157700031202     C                   Z-ADD     1             KNPG
157800070123     C     KBRV9         CHAIN     FNBRV09L                           35
157900951011     C* TROVATA SPUNTA LOCALE PARTITA NON SUL FV IN ELABORAZIONE
158000980623     C*  --> AGGIORNO  SE NON SI TRATTA DI FOGLIO DEFLUENZA E SE
158100980623     C*  IL FOGLIO E' CHIUSO
158200031202    1C     *IN35         DOWEQ     *OFF
158300031202   1AC     BRVNFV        IFNE      PARNFV
158400041006     c* ignoro la spunta se fatta su foglio viaggio partenza errato
158500041006     c* (lna del collo non presente sul foglio viaggio)
158600041006     c     brvftr        andne     'L'
158700951011     C*
158800951011     C* DATA PARTENZA MERCE E NUMERO PISTOLA PARTENZA SOLO SE LOCALE
158900031202     C                   Z-ADD     BRVNFV        KNFV
159000040317     C                   Z-ADD     BRVfgs        Kfgs
159100031202     C     K02FGV01      chain     FNFGV01L                           35
159200031202    2C     FGVFCF        IFEQ      'S'
159300980623     C* CONTROLLO IL TIPO TRAINO
159400031202    3C     FGVTTR        IFNE      ' '
159500031202     C     FGVTTR        LOOKUP    TDF                                    36
159600031202    3C                   ENDIF
159700980623     C**
159800031202    3C     FGVTTR        IFEQ      ' '
159900031202     C     *IN36         OREQ      *OFF
160000031203     c                   clear                   flg15
160100031202     C                   CLEAR                   KPJBU
160200031216     c                   movel     param         paramx
160300031216     c                   z-add     brvnfv        parnfv
160400031216     c                   z-add     fgvdfv        pardtp
160500031202     C                   MOVEL     PARAM         KPJBU
160600031202     c                   call      'FNLR15R'
160700031202     c                   parm                    kpjba
160800031202     c                   parm                    dsbrv
160900031203     c                   parm                    flg15             1
161000031216     c                   movel     paramx        param
161100031202    3C                   ENDIF
161200031202    2C                   ENDIF
161300951011     C*
161400031202   1AC                   ENDIF
161500070123     C     KBRV9         READE     FNBRV09L                               35
161600031202    1C                   ENDDO
161700951011     C*
161800031202     C                   ENDSR
161900960826     C*
162000960827     C* CANCELLAZIONE ANOMALIA 50-COLLO ARRIVATO NON PARTITO --------*
162100960826     C     ANM050        BEGSR
162200960826     C                   Z-ADD     50            COMCAA
162300990510     C     KANM          CHAIN     FNANM000                           33
162400960826    1C     *IN33         IFEQ      *OFF
162500020809     C                   CLEAR                   FNLR33DS
162600960826     C                   Z-ADD     ANMNR2        D33NRR
162700960826     C                   MOVE      'S'           D33MAC
162800960826     C                   CALL      'FNLR33R'
162900020809     C                   PARM                    FNLR33DS
163000960826     C                   END
163100960826     C                   ENDSR
163200080411     c*----------------------------------------------------------------
163300911001**         CMD5
163400021008CHKOBJ OBJ(FNBLPE0R) OBJTYPE(*FILE)     MBR(M234567890)
163500911001**         CMD6
163600021008ALCOBJ OBJ((FNBLPE0R *FILE *EXCL           M234567890))
163700911002**         CMD9
163800021008OVRDBF FILE(FNBLPE0R)                   MBR(M234567890)
163900050804**         CmdDSPLOG
164000050804DSPJOBLOG OUTPUT(*PRINT)
164100000622**
164200080411RICEZIONE BOLLE ARRIVO A xxx: MBR Mxxxyyy                               1
164300111104Bolla già esistente .                                                   2
164400111104Non trovato dettaglio colli .                                           3
164500111104Segnacollo esistente xxxxxxx : SOSTITUITO .                             4
164600111104Segnacollo esistente xxxxxxx : NON sostituito .                         5
