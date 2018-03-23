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
031300060515     c                   If        §OgPck = 'S' and §OgPkd <> 'S'
031400060515     c                             or §ogsda = 'S'
031500060515     c                   eval      lnapks = 'S'
031600060515     c                   endif
031700070322      * controllo se LNA è abilitata a telefonate GEO
031800070322     c                   if        §oggeot = 'S'
031900070322     c                   eval      lnageo = 'S'
032000070322     c                   endif
032100060515     c                   endif
032200060515      * controllo se c'è il picking attivo
032300060515     c                   if        lnapks = *blanks
032400031210     C     d55tfa        CHAIN     AZORG01L                           32
032500031210     C  N32              MOVEL     ORGDf0        OG150
032600031210     C   32              CLEAR                   OG150
032700040616if  5c                   If        §OgPck = 'S' and §OgPkd <> 'S'
032800041215     c                             or §ogsda = 'S'
032900031210     c                   eval      lnapks = 'S'
033000031210     c                   else
033100031210     c                   clear                   lnapks
033200031210     c                   endif
033300060515     c                   endif
033400070322      * se lna non abilitata a telefonate GEO controllo la sua capofila
033500070322     c                   if        lnageo = *blanks
033600070322     c     d55tfa        chain     azorg01l                           32
033700070322     c  n32              eval      og148 = orgde8
033800070322     c   32              clear                   og148
033900070322     c                   if        §oggeot = 'S'
034000070322     c                   eval      lnageo = 'S'
034100070322     c                   endif
034200070322     c                   endif
034300031210     C                   Z-ADD     BLPLNA        SAVLNA
034400031210     C                   ENDIF
034500040317     c
034600040317     c* Controllo bolla per bolla se si tratta di locale, non più in base
034700040317     c*  al membro ma in base ai terminal di partenza/arrivo
034800040330    1c                   if        blptfa=blptfp
034900040330    2c     blplna        ifne      blptfa
035000040330     C     blplna        CHAIN     AZORG01L                           32
035100040330     C  N32              MOVEL     ORGde3        OG143
035200040330     C   32              CLEAR                   OG143
035300040330     c                   z-add     §ogas         ogaslna           3 0
035400040330     C     blptfa        CHAIN     AZORG01L                           32
035500040330     C  N32              MOVEL     ORGDe3        OG143
035600040330     C   32              CLEAR                   OG143
035700040330    3c     ogaslna       ifeq      §ogas
035800040330     c                   seton                                        03
035900040330    3c                   endif
036000040330   x2c                   else
036100040317     c                   seton                                        03
036200040330    2c                   endif
036300040317     c                   endif
036400000713     C*
036500950118     C     KBLP2         SETLL     FNARB01L                               06
036600060220     c
036700000622     C     *IN06         IFEQ      *ON
036800060220     c
036900060220     C**   §B4ASO        ANDNE     '&'
037000060220     C**   *IN06         OREQ      *ON
037100060220     C**   BL2CEI        ANDNE     '&'
037200000622     C* 06 ON E BOLLA NON AUTOGENERATA - ERRORE GIA' ESISTENTE
037300060220     c* 20/02/06 : tolgo il discorso delle autogenerate perchè valeva solo
037400060220     c*            per Poste; se bolle già presente sempre errore
037500000622     C                   EXSR      STAERR
037600000622     C                   ELSE
037700000622     C* 06 OFF OPPURE ON MA BOLLA AUTOGENERATA: CREO O AGGIORNO BOLLA
037800060220     C** 06KBLP2         CHAIN     FNARB01L                           32
037900000622     C                   EXSR      CRTBOL
038000040317     c
038100040317     c* Memorizzo il numero foglio se bolla locale per la sottomissione
038200040317     c                   if        *in03
038300040317     c     blpnfv        lookup    nfv                                    36
038400040317     c                   if        not *in36
038500040317     c                   z-add     blpnfv        nfv(xx)
038600040317     c                   add       1             xx                3 0
038700040317     c                   endif
038800040317     c                   endif
038900040317     c
039000000622     C                   ENDIF
039100060220     c
039200950118     C                   READ      FNBLPE00                               31
039300911001     C                   END
039400031203     c* richiamo fnlr15r per la chiusura file e la scrittura dell'ultim
039500031203     c* agb
039600031203     c                   movel     'C'           flg15
039700031203     c                   call      'FNLR15R'
039800031203     c                   parm                    kpjba
039900031203     c                   parm                    dsbrv
040000031203     c                   parm                    flg15
040100900911     C*
040200040428     c                   setoff                                       2022
040300900409     C     FINE          TAG
040400040428     c*
040500040428     c* Per 20 on - risottometto ricezione bolle e spunte
040600040428     c                   if        *in20  and not *in22
040700050804     c
040800051006     c                   testn                   tentativi            30
040900051006     c                   move      tentativi     w001a             1
041000051006     c                   if        not *in30 or w001a<'0'
041100051006     c                   eval      tentativi='001'
041200051006     c                   eval      w0030=1
041300051006     c                   else
041400051006     c                   movel     tentativi     w0030             3 0
041500051006     c                   add       1             w0030
041600051006     c                   movel     w0030         tentativi
041700051006     c                   endif
041800051006     c
041900051006     c* max 200 tentativi
042000051006     c                   if        w0030<=200
042100050804     C                   MOVEL     'K'           legfil
042200050804     C                   MOVEl(P)  PARAM         KPJBU
042300040428     c
042400040428     C                   MOVEL     'LR14'        KCOAZ
042500040428     C                   CALL      'BCH10'
042600040428     C                   PARM                    KPJBA
042700040428     c
042800040428     C                   MOVEL     'LR22'        KCOAZ
042900040428     C                   CALL      'BCH10'
043000040428     C                   PARM                    KPJBA
043100040428     C                   SETON                                        98
043200051006     c                   else
043300051006     c
043400051006     c* msg x  pgm non eseguito e da chi allocato
043500051006     c
043600050804     c* richiamo pgm per rifare alcobj
043700051006     c                   call      'FNLR14C1'
043800080407     c                   parm                    fisico           10
043900051006     c                   parm                    mbrfil
044000051006     c
044100050804     c* eseguo dspjoblog per vedere chi mi alloca
044200051006     c                   Movel     CmdDSPLOG(1)  Comman
044300051006     c                   Call      'QCMDEXC'
044400051006     c                   Parm                    Comman
044500051006     c                   Parm                    Lung
044600050804     C
044700111104     c                   clear                   msgtes
044800111104     c                   clear                   msgdet
044900111104     c                   eval      msgtes='RICEZIONE BOLLE: FNLR14R '
045000111104     c                   eval      msgdet='FILE ALLO'+
045100111104     C                             'CATO. VEDI DSPJOBLOG   -   utente:'+knmus
045200050804     C
045300120305     C                   eval      wrkEml= 'ced@brt.it'
045400111104     C                   call(e)   'TIS701C'
045500111104     C                   parm                    wrkEml          253
045600111104     C                   PARM                    MSGTES           44
045700111104     C                   PARM                    MSGDET         5000
045800050804     C*
045900051006     c                   endif
046000050804     c
046100040428    2C                   ENDIF
046200040317     c*
046300040317     c* sottometto lr15 del foglio passato solo se è locale
046400040317     c                   if        nfv(1)>0
046500040317     c     parnfv        lookup    nfv                                    36
046600040317     c                   if        *in36 and pardef<>'S'
046700040317     c                   clear                   kpjbu
046800040317     C                   MOVEL     PARAM         KPJBU
046900040317     C                   CALL      'FNLR15R'
047000040317     c                   PARM                    KPJBA
047100040317     c                   endif
047200040317     c                   endif
047300950919     C*
047400950128     C** LANCIO PGM DI ABILITAZIONE BOLLE ALLA CONSEGNA
047500970509     C     *IN02         IFEQ      *ON
047600970509     C* CHIUDO PGM FNLS44R PER FARE SCRIVERE L'ULTIMO FNAGB
047700070228     C                   MOVEL     'C'           dls45flg
047800070228     C                   CALL      'FNLS45R'
047900070228     c                   parm                    fnls45ds
048000070228     c                   parm                    dsbrv
048100970509     C                   ENDIF
048200971114     C**
048300971114     C     WCAL55        IFEQ      'S'
048400020809     C                   CLEAR                   FNLV55DS
048500971114     C                   MOVEL     'C'           D55TLA
048600971114     C                   CALL      'FNLV55R'
048700020809     C                   PARM                    FNLV55DS
048800971114     C                   ENDIF
048900911016     C*
049000020809     C                   EVAL      *INLR = *ON
049100910322     C*
049200911107     C*--- CARICO TABELLE --------------------------------------------*
049300911107     C     CARTAB        BEGSR
049400921022     C*
049500921022     C* PRENDO TABELLA VARIABILI ARRIVI
049600921022     C                   MOVEL     '7A'          COD
049700921022     C                   MOVEL     *BLANKS       KEY
049800921022     C                   MOVEL     '1'           KEY
049900921022     C     KTAB          CHAIN     TABEL                              33
050000921022     C  N33              MOVEL     TBLUNI        DS7A
050100950130     C   33              CLEAR                   DS7A
050200010110     C* RICERCO FASE
050300921022     C                   MOVEL     '7N'          COD
050400960424     C                   MOVEL(P)  2             KEY
050500921022     C     KTAB          CHAIN     TABEL                              31
050600921022     C  N31              MOVEL     TBLUNI        DS7N
050700980623     C* CARICO TIPI TRAINO DEFLUENZA
050800980623     C                   MOVEL     'TV'          COD
050900020809     C                   CLEAR                   XX
051000980623     C     KTAB2         SETLL     TABEL
051100980623     C     KTAB2         READE     TABEL                                  31
051200980623     C     *IN31         DOWEQ     *OFF
051300980623     C     TBLFLG        IFEQ      ' '
051400980623     C                   MOVEL     TBLUNI        DSTV
051500980623     C     §TVDEF        IFEQ      'S'
051600020809     C                   ADD       1             XX
051700020809     C                   MOVEL     TBLKEY        TDF(XX)
051800980623     C                   ENDIF
051900980623     C                   ENDIF
052000980623     C**
052100980623     C     KTAB2         READE     TABEL                                  31
052200980623     C                   ENDDO
052300000622     C*
052400000622     C* CARICO IN SCHIERA ELENCO DIVISE ACCETTATE IN ITALIA
052500000622     C*
052600000622     C                   MOVEL     'CV'          COD
052700000622     C                   CLEAR                   DVA
052800011025     C                   CLEAR                   DEC
052900020809     C                   CLEAR                   XX
053000000622     C     KTAB2         SETLL     TABEL
053100000622     C     KTAB2         READE     TABEL                                  31
053200000622     C     *IN31         DOWEQ     *OFF
053300000622     C     TBLFLG        IFEQ      ' '
053400000622     C                   MOVEL     TBLUNI        DSCV
053500000622     C     §CVDVA        IFEQ      'S'
053600020809     C                   ADD       1             XX
053700020809     C                   MOVEL     TBLKEY        DVA(XX)
053800011025      * Carico in schiera il numero d decimali della divisa
053900020809     C                   Z-ADD     §CVDEC        DEC(XX)
054000000622     C                   ENDIF
054100000622     C                   ENDIF
054200000622     C**
054300000622     C     KTAB2         READE     TABEL                                  31
054400000622     C                   ENDDO
054500011029      *
054600020809     C                   CLEAR                   TIBS02DS
054700011029     C                   CLEAR                   KPJBU
054800011029     C                   MOVE      'C'           T02MOD
054900011029     C                   MOVE      KNSIF         T02SIF
055000011029     C                   MOVE      'GED'         T02COD
055100011029     C                   MOVEL(P)  1             T02KE1
055200011029     C                   CALL      'TIBS02R'
055300011029     C                   PARM                    KPJBA
055400020809     C                   PARM                    TIBS02DS
055500011029    6C     T02ERR        IFEQ      *BLANKS
055600011029     C                   MOVEL     T02UNI        DGED
055700011029     C                   ELSE
055800011029     C                   CLEAR                   DGED
055900011029    6C                   ENDIF
056000011029      *
056100011029      * Recupero il numero di decimali di §GEDCN
056200011029     C                   MOVEL     'CV'          TBLCOD
056300011029     C                   MOVEL     *BLANKS       TBLKEY
056400011029     C                   MOVEL     §GEDCN        TBLKEY
056500011029     C     KTAB          CHAIN     TABEL                              31
056600011029     C  N31              MOVEL     TBLUNI        DSCV
056700011029     C   31              CLEAR                   DSCV
056800011030      * IMPORTI VARI
056900020809     C                   CLEAR                   TIBS02DS
057000011030     C                   CLEAR                   KPJBU
057100011030     C                   MOVE      'C'           T02MOD                         CONTROLLO
057200011030     C                   MOVE      KNSIF         T02SIF                         S.I.
057300011030     C                   MOVE      'GEI'         T02COD                         TABELLA PROFILI
057400011030     C                   MOVEL(P)  §GEDCN        T02KE1                         PROFILO IN K1
057500011030     C                   CALL      'TIBS02R'
057600011030     C                   PARM                    KPJBA
057700020809     C                   PARM                    TIBS02DS
057800011030    6C     T02ERR        IFEQ      *BLANKS
057900011030     C                   MOVEL     T02UNI        DGEI
058000011030     C                   ELSE
058100011030     C                   CLEAR                   DGEI
058200011030    6C                   ENDIF
058300911107     C                   ENDSR
058400911001     C**---------------------------------------------------------
058500911001     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
058600911001     C**---------------------------------------------------------
058700911001     C     CAEX          BEGSR
058800040428     c                   setoff                                       22
058900020809     C                   MOVE      *BLANK        VAR11
059000020809     C                   MOVE      *BLANK        VAR12
059100911001     C                   MOVE      ')'           VAR11
059200021008     C                   MOVEL     MBRfil        VAR11
059300911001     C                   MOVE      '))'          VAR12
059400021008     C                   MOVEL     MBRfil        VAR12
059500021008     C                   Z-ADD     55            LUNG
059600911001     C*
059700911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
059800911001     C                   SETOFF                                       20
059900950118     C                   MOVE      VAR11         CMD5                            FNBLPE0R
060000911001     C                   MOVEL     *BLANKS       COMMAN
060100020809     C                   MOVEA     CMD5(1)       COMMAN
060200911001     C                   CALL      'QCMDEXC'                            20
060300911001     C                   PARM                    COMMAN
060400911001     C                   PARM                    LUNG
060500040428     c*
060600040428     C* 20 ON - ESCO
060700040428     c                   if        *in20
060800040428     c                   seton                                          22
060900040428     C                   GOTO      ENDCAE
061000040428     c                   endif
061100911002     C* ALLOCO MEMBRO: SE NON RIESCO ESCO DA PROGRAMMA
061200911001     C                   SETOFF                                       20
061300911002     C                   MOVE      VAR12         CMD6
061400911001     C                   MOVEL     *BLANKS       COMMAN
061500911002     C                   MOVEA     CMD6(1)       COMMAN
061600911001     C                   CALL      'QCMDEXC'                            20
061700911001     C                   PARM                    COMMAN
061800911001     C                   PARM                    LUNG
061900911002     C*
062000050804     c                   if        *in20
062100050804     c                   eval      fisico='FNBLPE0R'
062200050804     C                   GOTO      ENDCAE
062300050804     c                   endif
062400911001     C* OVRDBF
062500911002     C                   SETOFF                                       20
062600911002     C                   MOVE      VAR11         CMD9
062700911001     C                   MOVEL     *BLANKS       COMMAN
062800911002     C                   MOVEA     CMD9(1)       COMMAN
062900911002     C                   CALL      'QCMDEXC'                            20
063000911001     C                   PARM                    COMMAN
063100911001     C                   PARM                    LUNG
063200050804     C   20              GOTO      ENDCAE
063300911002     C*
063400950118     C                   OPEN      FNBLPE0R
063500911001     C*
063600911002     C     ENDCAE        ENDSR
063700911002     C**---------------------------------------------------------
063800911002     C** CREO BOLLA ARRIVI
063900911002     C**---------------------------------------------------------
064000980311     C     CRTBOL        BEGSR
064100080407     c*
064200931125     C* S C R I V O   B O L L A
064300990906     C                   EXSR      RIEARB
064400000622     C* LE SEGUENTI SPECIFICHE LE ESEGUO SOLO SE BOLLA NON PRESENTE
064500000622   1AC     *IN06         IFEQ      *OFF
064600911003     C*
064700931125     C* D E T T A G L I O   C O L L I
064800950413     C*
064900990906     C                   EXSR      RIEBLT
065000931125     C*
065100080407     C* Leggo il numero dei segnacolli dal dettaglio bolle partenza
065200080407     C     KBLP2         SETLL     FNBLT01l                               32
065300921123     C*
065400921123     C* 32 OFF-  NON TROVATO IL DETTAGLIO COLLI: STAMPO ERRORE
065500950118    2C     *IN32         IFEQ      *OFF
065600080411     c     §3arbl        andne     'R'
065700090109     c     §3aurc        andne     'S'
065800921123     C                   SETON                                        07
065900921123     C                   EXSR      STAERR
066000950118   X2C                   ELSE
066100921123     C*
066200080407     C     KBLP2         READE     FNBLT01l                               32
066300990906     C*
066400950118    3C     *IN32         DOWEQ     *OFF
066500100414     C                   Z-ADD     BLTfls        ARTfls
066600100414     C                   Z-ADD     BLTNSC        ARTNSC
066700080407     c                   exsr      PesoeVol
066800931125     C*
066900920917     C* CONTROLLO SE SEGNACOLLO GIA' ESISTENTE
067000950905     C                   SETOFF                                       0408
067100950413     C     KSPP          CHAIN     FNART27L                           31
067200080407    4C     *IN31         IFEQ      *OFF
067300950413     C                   SETON                                        04
067400080407    5C     ARTDCM        IFNE      *ZEROS
067500950905     C                   SETON                                        08
067600950413     C                   EXSR      RIEBLT
067700100414     C                   Z-ADD     BLTfls        ARTfls
067800950418     C                   Z-ADD     BLTNSC        ARTNSC
067900080407     c                   exsr      PesoeVol
068000950413     C* Segnacollo già esistente: se già consegnato aggiorno il record
068100950413     C                   UPDATE    FNART000
068200040317     c   03              exsr      ctrdet
068300040317     c
068400080407   x5C                   ELSE
068500040317     c
068600950418     C                   EXSR      RIEBLT
068700100414     C                   Z-ADD     BLTfls        ARTfls
068800950418     C                   Z-ADD     BLTNSC        ARTNSC
068900950413     C                   UNLOCK    FNART27L
069000080407    5C                   END
069100950413     C* Stampo errore
069200950413     C                   EXSR      STAERR
069300080407   x4C                   ELSE
069400980311     C*
069500951011     C                   WRITE     FNART000
069600031202     c   03              exsr      ctrdet
069700080407    4C                   END
069800911014     C*
069900080407     C     KBLP2         READE     FNBLT01l                               32
070000950118    3C                   ENDDO
070100950118    2C                   ENDIF
070200911003     C*
070300000622   1AC                   ENDIF
070400911003     C* SCRIVO FILE
070500000622     C  N06              WRITE     FNARB000
070600000622     C   06              UPDATE    FNARB000
070700080407     C*
070800911003     C* DELETO
070900000622     C     *IN06         IFEQ      *OFF
071000950118     C                   DELETE    FNBLPE00
071100000622     C                   ELSE
071200000622     C                   EXSR      DLTRIC
071300000622     C                   ENDIF
071400960424     C**
071500960424     C* CANCELLO EVENTUALE ANOMALIA 12
071600960424     C                   Z-ADD     12            COMCAA
071700990510     C     KANM1         CHAIN     FNANM01L                           33
071800960424     C* CHIUDO ANOMALIA
071900960424    1C     *IN33         IFEQ      *OFF
072000020809     C                   CLEAR                   FNLR33DS
072100960424     C                   Z-ADD     ANMNR1        D33NRR
072200960424     C                   MOVE      DATEU         D33DCH
072300960424     C                   MOVEL     §7NFS1        D33FSC
072400960424     C                   CALL      'FNLR33R'
072500020809     C                   PARM                    FNLR33DS
072600960424     C                   ENDIF
072700921022     C*
072800000622     C     *IN06         IFEQ      *OFF
072900000111     C     BLPLNA        CHAIN     AZORG01L                           32
073000020218     C  N32              MOVEL     ORGDE3        OG143
073100020218     C   32              CLEAR                   OG143
073200020226     c                   eval      lnantw = §ogntw
073300921022     C* CONTROLLO SE C'ERANO ANOMALIE MANCA BOLLA
073400960827     C*           E ANOMALIE 050-COLLO ARR. NON PART. SU BOLLE LOCALI
073500950123     C     KBLP2         SETLL     FNART001
073600020226     C     lnaNTW        IFNE      'EEX'
073700020226     C     lnaNTW        ANDNE     'EUP'
073800020226     C     lnaNTW        ANDNE     'FED'
073900060322     C     lnaNTW        ANDNE     'DPD'
074000961121     C     KBLP2         READE(N)  FNART001                               32
074100961121     C                   ELSE
074200961121     C     KBLP2         READE     FNART001                               32
074300961121     C                   ENDIF
074400921022     C*
074500961121     C     *IN32         DOWEQ     *OFF
074600950128     C                   EXSR      CTRANM
074700960827     C* . . . . . BOLLA LOCALE , SERIE>0 , FLS *NE LNP
074800960827     C     *IN03         IFEQ      *ON
074900960827     C     ARTNRS        ANDGT     *ZEROS
075000960827     C     ARTFLS        ANDNE     ARTLNP
075100960827     C                   EXSR      ANM050
075200960827     C                   END
075300961121     C**
075400000111     C* SE LNA ESTERA POSSO AGGIORNARE IL RECORD
075500020226     C     lnaNTW        IFNE      'EEX'
075600020226     C     lnaNTW        ANDNE     'EUP'
075700020226     C     lnaNTW        ANDNE     'FED'
075800060322     C     lnaNTW        ANDNE     'DPD'
075900961121     C     KBLP2         READE(N)  FNART001                               32
076000961121     C                   ELSE
076100961121     C     KBLP2         READE     FNART001                               32
076200921022     C                   END
076300961121     C**
076400961121     C                   ENDDO
076500000622     C                   ENDIF
076600911002     C*
076700911002     C                   ENDSR
076800080407     C**---------------------------------------------------------
076900080407     c* imposto peso e volume della partenza sul dettaglio colli
077000080407     C**---------------------------------------------------------
077100080407     c     PesoeVol      BEGSR
077200080407     c                   z-add     bltvuc        artvuc
077300080407     c                   movel     bltfvc        artfvc
077400080407     c                   z-add     bltpuc        artpuc
077500080407     c                   movel     bltfpc        artfpc
077600080407     c                   select
077700080407     c                   when      artvuc>0 and artpuc>0
077800080407     c                   eval      arbfbs='E'
077900080423     c                   when      artvuc>0   and arbfbs=' '
078000080407     c                   eval      arbfbs='V'
078100080423     c                   when      artpuc>0   and arbfbs=' '
078200080407     c                   eval      arbfbs='P'
078300080423     c                   other
078400080423     c                   eval      arbfbs='E'
078500080407     c                   endsl
078600080407     c                   ENDSR
078700911002     C**---------------------------------------------------------
078800911002     C** RIEMPO CAMPI FLARB00F
078900911002     C**---------------------------------------------------------
079000911002     C     RIEARB        BEGSR
079100000622     C* DEVO CLEARARE FORMATO FNARB SOLO SE BOLLA NON PRESENTE
079200000622     C  N06              CLEAR                   FNARB000
079300931125     C*
079400950118     C     BLPCBO        IFNE      SAVCBO
079500920603     C                   MOVEL     '3A'          COD
079600920603     C                   MOVEL     *BLANKS       KEY
079700950118     C                   MOVEL     BLPCBO        KEY
079800920603     C     KTAB          CHAIN     TABEL                              33
079900920603     C  N33              MOVEL     TBLUNI        DS3A
080000920603     C   33              MOVEL     'F1'          §3ATB1
080100920625     C   33              MOVEL     '  '          §3ATB2
080200950118     C                   MOVEL     BLPCBO        SAVCBO            2
080300950118     C                   ENDIF
080400920603     C*
080500911002     C                   MOVEL     BLPLNP        ARBLNP
080600950118     C                   MOVE      BLPAAS        ARBAAS
080700911002     C                   MOVEL     BLPMGS        ARBMGS
080800911002     C                   MOVEL     BLPNRS        ARBNRS
080900911002     C                   MOVEL     BLPNSP        ARBNSP
081000010110     C* L'IMPOSTAZIONE DEL CODICE BOLLA SE STO RICEVENDO UN AGGIORNAMEN
081100010110     C* TO DI BOLLA AUTOGENERATA LA FACCIO QUANDO GESTISCO IL CONTRAS-
081200010110     C* SEGNO
081300010110     C  N06              MOVEL     BLPCBO        ARBCBO
081400950118     C                   MOVEL     BLPFDN        ARBFDN
081500911002     C                   MOVEL     BLPFNS        ARBFNS
081600911002     C                   MOVEL     BLPLNA        ARBLNA
081700911002     C                   MOVEL     BLPKSC        ARBKSC
081800911002     C                   MOVEL     BLPZNC        ARBZNC
081900911002     C                   MOVEL     BLPRSM        ARBRSM
082000911002     C                   MOVEL     BLPINM        ARBINM
082100911002     C                   MOVEL     BLPLOM        ARBLOM
082200911002     C                   MOVEL     BLPPRM        ARBPRM
082300950118     C                   MOVEL     BLPCAM        ARBCAM
082400950118     C                   MOVEL     BLPNZM        ARBNZM
082500911002     C                   MOVEL     BLPRSD        ARBRSD
082600911003     C                   MOVEL     BLPIND        ARBIND
082700911002     C                   MOVEL     BLPLOD        ARBLOD
082800911002     C                   MOVEL     BLPPRD        ARBPRD
082900950118     C                   MOVEL     BLPCAD        ARBCAD
083000950118     C                   MOVEL     BLPNZD        ARBNZD
083100911002     C                   MOVEL     BLPGC1        ARBGC1
083200911002     C                   MOVEL     BLPGC2        ARBGC2
083300911002     C                   MOVEL     BLPCTR        ARBCTR
083400911002     C                   MOVEL     BLPCTS        ARBCTS
083500911002     C                   MOVEL     BLPFTM        ARBFTM
083600950118     C                   MOVEL     BLPFIN        ARBFIN
083700911002     C                   MOVEL     BLPFAP        ARBFAP
083800911002     C                   MOVEL     BLPTSP        ARBTSP
083900950118     C                   Z-ADD     BLPIAS        ARBIAS
084000950123     C                   MOVEL     BLPVAS        ARBVAS
084100911002     C                   MOVEL     BLPNAS        ARBNAS
084200911002     C                   MOVEL     BLPNCL        ARBNCL
084300950118     C                   Z-ADD     BLPPKB        ARBPKB
084400950118     C                   Z-ADD     BLPPKF        ARBPKF
084500000622     C* SE BOLLA GIA' PRESENTE SOSTITUISCO SOLO SE IL VOLUME SU ARB
084600000622     C* NON E' NE' "T" NE' "Z" E SE IL VOLUME RICEVUTO E' > DI QUELLO
084700000622     C* SU ARB
084800000622     C     *IN06         IFEQ      *OFF
084900000622     C     ARBFVF        ORNE      'T'
085000000622     C     ARBFVF        ANDNE     'Z'
085100000622     C     BLPVLF        ANDGT     ARBVLF
085200950118     C                   Z-ADD     BLPVLF        ARBVLF
085300950118     C                   MOVEL     BLPFVF        ARBFVF
085400000622     C                   ENDIF
085500950118     C                   Z-ADD     BLPVLB        ARBVLB
085600950118     C                   MOVEL     BLPFVB        ARBFVB
085700950118     C                   MOVEL     BLPQFT        ARBQFT
085800950118     C                   MOVEL     BLPVMD        ARBVMD
085900950118     C                   MOVEL     BLPVAD        ARBVAD
086000070222     C*
086100911002     C                   MOVEL     BLPCCM        ARBCCM
086200950118     C                   Z-ADD     BLPRMN        ARBRMN
086300950118     C                   MOVEL     BLPRMA        ARBRMA
086400911002     C                   MOVEL     BLPRMO        ARBRMO
086500911002     C                   MOVEL     BLPFLS        ARBFLS
086600911002     C                   MOVEL     BLPNCD        ARBNCD
086700911002     C                   MOVEL     BLPNCA        ARBNCA
086800911002     C                   MOVEL     BLPXCO        ARBXCO
086900950118     C*
087000911002     C                   MOVEL     BLPFFD        ARBFFD
087100950118     C                   MOVE      BLPDCR        ARBDCR
087200911002     C                   MOVEL     BLPHCR        ARBHCR
087300911002     C                   MOVEL     BLPTCR        ARBTCR
087400950118     C                   MOVE      BLPDBR        ARBDBR
087500931215     C                   MOVEL     BLPNFV        ARBNFV
087600950124     C*
087700911002     C                   MOVEL     BLPFLE        ARBFLE
087800911002     C                   MOVEL     BLPTFA        ARBTFA
087900911002     C                   MOVEL     BLPTFP        ARBTFP
088000911002     C                   MOVEL     BLPFEA        ARBFEA
088100911008     C                   MOVEL     BLPFBR        ARBFBR
088200950118     C                   MOVEL     BLPTC1        ARBTC1
088300950118     C                   MOVEL     BLPTC2        ARBTC2
088400950118     C                   MOVEL     BLPGVA        ARBGVA
088500950118     C                   MOVEL     BLPGMA        ARBGMA
088600950118     C                   MOVEL     BLPGGA        ARBGGA
088700001204     C*  SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA N FT1
088800051116     C* BLANK NON LO DEVO AGGIORNARE ALTRIMENTI NON VENGONO TRASMESSE
088900001204     C* LE DATE DI CONSEGNA
089000001204     C     *IN06         IFEQ      *OFF
089100001204     C     ARBFT1        ORNE      *BLANKS
089200931125     C                   MOVEL     'R'           ARBFT1
089300950118     C                   Z-ADD     DATEU         ARBDT1
089400001204     C                   ENDIF
089500970212     C*
089600060220     c* Imposto nel tipo record "A" (se non c'e' lo creo,)
089700060220     C*   IL 1° BYTE DEL CAMPO AR4NOT  E §B4ASO=SIGLA OPERATORE
089800060302     c                   clear                   contaerror
089900060220     c                   eval      wtrc='A'
090000060302     c
090100060302     c* Se errore in write (succede per conflitto con fnlrc1r)
090200060302     c*  ritento l'update
090300060302    1c                   dou       not *in99
090400060302     c                   setoff                                       99
090500060220     c     kar4          chain     fiar401l
090600060302     c                   exsr      Scriviar4A
090700060302     c
090800060302     c* dopo 10 tentativi mando un messaggio in posta ed esco senza
090900060302     c*  impostare il record A
091000060302     c                   add       1             contaerror        2 0
091100060302    2c                   if        contaerror>10
091200060302     C
091300111104     c* Mando e_mail
091400111104     c                   clear                   msgtes
091500111104     c                   clear                   msgdet
091600111104     c                   eval      msgtes='RICEZIONE BOLLE: FNLR14R'
091700111104     c                   eval      msgdet='Non scritto record"A"   -  utente'+
091800111104     c                               KNMUS + '   bolla :' +
091900111104     c                               (%editc(arbaas:'Z'))
092000060302     c                             + (%editc(arblnp:'Z')) + (%editc(arbnrs:'2'))
092100060302     c                             + (%editc(arbnsp:'2'))
092200120305     C                   eval      wrkEml= 'ced@brt.it'
092300111104     C                   call(e)   'TIS701C'
092400111104     C                   parm                    wrkEml          253
092500111104     C                   PARM                    MSGTES           44
092600111104     C                   PARM                    MSGDET         5000
092700060302     C*
092800060302     c                   leave
092900060302    2c                   endif
093000060302    1c                   enddo
093100060302     c
093200060220     c
093300060220     c* Cerco partita iva da impostare su fnarb
093400060220     c                   eval      wtrc='P'
093500060220     c     kar4          chain(N)  fiar401l
093600060220     c                   if        %found(fiar401l)
093700060220     C                   MOVEL     §3ATB1        UNOTB1            1
093800060220     C     UNOTB1        IFEQ      'F'
093900060220     C     §3ATB2        ANDEQ     *BLANKS
094000060220     C                   MOVEL     AR4NOT        ARBCPI
094100060220     C                   ELSE
094200060220     C                   MOVE      AR4NOT        ARBCPI
094300060220     C                   ENDif
094400061031
094500061031     c                   else
094600061031      * se non c'è il fiar4 rcd P aggancio la bolla partenza se bolla unica
094700061031     c                   if        §3atb2 = *blanks
094800061031     c     kblp2         chain     fnblp01l
094900061031     c                   if        %found(fnblp01l)
095000061031     c                   eval      arbcpi = w_blpcpi
095100061031     c                   endif
095200061031     c                   endif
095300061031
095400060220     C                   ENDif
095500030109      *
095600930201     C* A B I L I T A Z I O N E   M E R C E
095700931125     C**
095800000622     C*
095900930201     C* SE FOGLIO VIAGGIO 99999 ABILITO SUBITO
096000000623     C  N06BLPNFV        IFEQ      99999
096100930201     C                   MOVEL     3             ARBAMA
096200950919     C                   ENDIF
096300931125     C**
096400931125     C* A B I L I T A Z I O N E   C / A S S E G N O
096500000623     C*
096600911002     C* PER BOLLE SENZA CONTRASSEGNO
096700051116     c* Vedo se essite C/A per abilitazione
096800051116     c     kblp2         chain     fiar901l
096900051116     c                   if        not %found(fiar901l) or ar9cas=0
097000911002     C                   MOVEL     'S'           ARBACC
097100950119   X1C                   ELSE
097200011211
097300011210      * ------------------
097400011210
097500011210      * NUOVO CONTROLLO C/ASSEGNO
097600011210     c                   clear                   trul21ds
097700011210     c                   eval      i21tla = 'L'
097800011210     c                   eval      i21cbo = blpcbo
097900011210     c                   eval      i21tsp = blptsp
098000011210     c                   eval      i21lnp = blplnp
098100011210     c                   eval      i21nzm = blpnzm
098200011210     c                   eval      i21lna = blplna
098300011210     c                   eval      i21nzd = blpnzd
098400051116     c                   eval      i21tic = ar9tic
098500051116     c                   eval      i21imp = ar9cas
098600051116     c                   eval      i21div = ar9vca
098700011210     c                   eval      i21ute = knmus
098800011210     c                   eval      i21pgm = nompgm
098900011210     c                   call      'TRUL21R'
099000011210     c                   parm                    trul21ds
099100011210
099200011210     c                   if        o21fca = *blanks
099300011210     c                   eval      arbacc = 'S'
099400011210     c                   endif
099500911002     C*
099600950119    1C                   END
099700931125     C**
099800950119     C* A B I L I T A Z I O N E    A S S E G N A T O
099900931125     C**
100000000623   1AC     *IN06         IFEQ      *OFF
100100000623     C*
100200911003     C                   MOVEL     §3ATB1        UNOTB1            1
100300911003     C* FRANCO
100400950119    1C     UNOTB1        IFEQ      'F'
100500930112     C* SE NON C'E' SECONDA BOLLA ABILITO
100600950119    2C     §3ATB2        IFEQ      '  '
100700911003     C                   MOVEL     'S'           ARBACA
100800950119    2C                   END
100900950119    1C                   END
101000050314     c
101100050314     c* Se già tassata, abilito subito l'assegnato
101200050314     c                   if        arbaca=' '
101300050314     c                   if        §3atb2<>*blanks
101400050314     c                   eval      ktrc='2'
101500050314     c                   else
101600050314     c                   eval      ktrc='1'
101700050314     c                   endif
101800050314     c
101900050314     c     kar6          chain     fiar601l
102000050314     c                   if        %found(fiar601l)
102100050314     c                   move      ar6ksc        fineksc
102200050314     c                   if        fineksc<>'9999' and fineksc<>'0000'
102300050314     c                   eval      arbaca='S'
102400050314     c* mi salvo i campi da inmpostare in arbksc e arbctr
102500050314     c                   eval      arbksc=ar6ksc
102600050314     c                   eval      arbctr=ar6ctr
102700050331     c* Se si tratta di bolla di recupero, devo anche chiudere la spedizione
102800050331    3C     §3ATB1        IFEQ      'A3'
102900050331     C* IMPOSTO I CAMPI DELLA CHIUSURA
103000050331     C                   MOVEL     'Y'           ARBICC
103100050331     C                   MOVEL     'S'           ARBICA
103200050331     C                   Z-ADD     DATEU         ARBDCM
103300050331     C                   time                    wora              6 0
103400050331     C                   movel     wora          ARBHMC
103500050331     C                   CLEAR                   ARBFT1
103600050331     C                   CLEAR                   ARBDT1
103700050331     C                   MOVEL     *ALL'9'       ARBNDC
103800050331     C                   Z-ADD     DATEU         ARBDDC
103900050331     C                   MOVEL     'S'           ARBFDC
104000050331     C                   MOVEL     *ALL'9'       ARBNMI
104100050331     C                   Z-ADD     DATEU         ARBDMI
104200050331     C                   MOVEL     *ALL'9'       ARBCUC
104300050331     C                   CLEAR                   ARBFBC
104400050331    3C                   ENDIF
104500050314     c                   endif
104600050314     c                   endif
104700050314     c                   endif
104800050314     c
104900000623   1AC                   ENDIF
105000070322
105100070322      * se LNA o sua capofila abilitata alle telefonate per GEO
105200070329      *  e NO bolle di recupero
105300070329     c                   if        lnageo = 'S' and §3arbl <> 'R'
105400070322     c                   clear                   dsbl4l
105500070322      * Genero record blocco spedizione se:
105600070322      * --> Consegna particolare Appuntamento o Supermercato (non tassative)
105700070322     c                   if        arbtc1 = 'A' or arbtc1 = 'S' or arbtc2 = 'A'
105800070322     c                             or arbtc2 = 'S'
105900070322     c                   eval      §b4lbas = 'S'
106000070322      * se consegna tassativa sblocco
106100070322     c                   if        arbdcr <> *zeros and arbtcr = *blanks
106200070322     c                   clear                   §b4lbas
106300070322     c                   endif
106400070322     c                   endif
106500070322      * --> Contrassegno non abilitato
106600070322     c                   if        arbacc = *blanks
106700070322     c                   eval      §b4ltca = 'S'
106800070322     c                   endif
106900070322      * --> Fermo deposito
107000070322     c                   if        arbffd <> *blanks
107100070322     c                   eval      §b4ltfd = 'S'
107200070322     c                   endif
107300070322      * scrivo record L di fiar4 per la gestione dei blocchi
107400070322     c                   if        dsbl4l <> *blanks
107500070322     c                   clear                   fiar4000
107600070322     c                   eval      ar4aas = blpaas
107700070322     c                   eval      ar4lnp = blplnp
107800070322     c                   eval      ar4nrs = blpnrs
107900070322     c                   eval      ar4nsp = blpnsp
108000070322     c                   eval      ar4trc = 'L'
108100070322     c                   eval      ar4not = dsbl4l
108200070322     c                   eval      ar4ftr = 'R'
108300070322     c                   eval      ar4dtr = dateu
108400070322     c                   eval      ar4duv = dateu
108500070322     c                   write     fiar4000                             99
108600070322     c                   endif
108700070322     c                   endif
108800950118     C*
108900911002     C                   ENDSR
109000070322      *-----------------------------------------------------------------------
109100060302     c     Scriviar4A    BEGSR
109200060302     c                   if        %found(fiar401l)
109300060302     c                   movel     ar4not        dsbl4a
109400060302     c                   else
109500060302     C                   MOVE      BLPAAS        AR4AAS
109600060302     C                   MOVEL     BLPLNP        AR4LNP
109700060302     C                   MOVEL     BLPNRS        AR4NRS
109800060302     C                   MOVEL     BLPNSP        AR4NSP
109900060302     C                   MOVEL     'A'           AR4TRC
110000060302     C                   CLEAR                   AR4NOT
110100060302     C                   CLEAR                   DSBL4A
110200060302     c                   movel     'R'           ar4ftr
110300060302     c                   movel     dateu         ar4DTR
110400060302     c                   endif
110500060302     c
110600060302     c                   movel     dateu         ar4duv
110700060302     C                   MOVEL     BL2SV1        §B4ABM
110800060302
110900060302      * se linea di arrivo con picking attivo o lna per cui stampare i
111000060302      * DDT SI in arrivo
111100060302if  1c                   If        lnapks = 'S'
111200060302 s  2c                   Select
111300060302
111400060302      * per bolla di recupero
111500060302      * imposto che la bolla è già stata stampata
111600060302     c                   When         §3arbl = 'R'
111700060302     c                   If        Bl2Sv1 = 'S'
111800060302     c                   Eval      §B4Abm = 'P'
111900060302     c                   EndIf
112000060302     c                   If        Bl2Sv1 = 'C'
112100060302     c                   Eval      §B4Abm = 'Q'
112200060302     c                   EndIf
112300060302     c                   If        Bl2Sv1 = 'N' or Bl2Sv1 = 'Y'
112400060302     c                   Eval      §B4Abm = 'J'
112500060302     c                   Endif
112600060302     c                   If        Bl2Sv1 = 'K'
112700060302     c                   Eval      §B4Abm = 'W'
112800060302     c                   Endif
112900060302
113000060302      * se bl2sv1 = 'Y'
113100060302      * imposto 'N' x bolla ancora da stampare
113200060302     c                   When      Bl2Sv1 = 'Y'
113300060302     c                   Eval      §B4Abm = 'N'
113400060302
113500060302    2c                   Endsl
113600060302    1c                   Endif
113700060302
113800060302     C     §7AFSR        IFEQ      'Y'
113900060302     C     BL2SV1        ANDEQ     'Y'
114000060302     C     lnpNTW        ANDNE     'DPD'
114100060302     C                   MOVEL     'N'           §B4ABM
114200060302     C                   ENDIF
114300060302     c
114400060302     c* Sigla operatore video
114500060302     C                   MOVE      BL2CEI        §B4ASO
114600060302     C                   MOVEL     DSBL4A        AR4NOT
114700060302     c                   if        %found(fiar401l)
114800060302     c                   update    fiar4000
114900060302     c                   else
115000060302     C                   WRITE     FiAR4000                             99
115100060302     c                   endif
115200060302     c                   endsr
115300911002     C**---------------------------------------------------------
115400911002     C** RIEMPO DETTAGLIO SEGNACOLLI
115500911002     C**---------------------------------------------------------
115600911002     C     RIEBLT        BEGSR
115700950119     C                   CLEAR                   FNART000
115800950118     C                   Z-ADD     ARBAAS        ARTAAS
115900931125     C                   Z-ADD     BLPLNP        ARTLNP
116000911002     C                   Z-ADD     BLPNRS        ARTNRS
116100911002     C                   Z-ADD     BLPNSP        ARTNSP
116200911002     C                   MOVEL     BLPLNA        ARTLNA
116300911002     C                   MOVEL     BLPFLS        ARTFLS
116400950119     C                   MOVEL     'R'           ARTFTR
116500960712     C* SE COLLO LOCALE INDICATO CHE E' ABBINATO
116600960712     C     *IN03         IFEQ      *ON
116700960712     C                   MOVEL     'S'           ARTABN
116800960712     C                   ENDIF
116900950119     C                   ENDSR
117000911014     C**---------------------------------------------------------
117100960424     C** CONTROLLO SE ESISTE ANOMALIA 10-NO REC BOLLA E 12-
117200911014     C**---------------------------------------------------------
117300950128     C     CTRANM        BEGSR
117400960529     C                   Z-ADD     0             OFV               5 0
117500970521     C*
117600970521     C* CHIUDO ANOMALIA 410 SE ESISTE
117700970521     C                   Z-ADD     410           COMCAA
117800990510     C     KANM          CHAIN     FNANM000                           33
117900970521    1C     *IN33         IFEQ      *OFF
118000020809     C                   CLEAR                   FNLR33DS
118100970521     C                   Z-ADD     ANMNR2        D33NRR
118200970521     C                   MOVE      DATEU         D33DCH
118300970521     C                   MOVEL     §7NFS1        D33FSC
118400970521     C                   CALL      'FNLR33R'
118500020809     C                   PARM                    FNLR33DS
118600970521    1C                   END
118700950128     C*
118800960424     C                   Z-ADD     10            COMCAA
118900990510     C     KANM          CHAIN     FNANM000                           33
119000950128     C* CHIUDO ANOMALIA
119100950128    1C     *IN33         IFEQ      *OFF
119200000623     C*
119300020809     C                   CLEAR                   FNLR33DS
119400960415     C                   Z-ADD     ANMNR2        D33NRR
119500960415     C                   MOVE      DATEU         D33DCH
119600960415     C                   MOVEL     §7NFS1        D33FSC
119700960415     C                   CALL      'FNLR33R'
119800020809     C                   PARM                    FNLR33DS
119900960415     C*
120000950128     C* ESEGUO LA CALL AL PGM DI CARICAMENTO SPUNTE PER AGGIORNARE
120100951213     C*  LA BOLLA. NON LO FACCIO SE E' LOCALE
120200961121    2C     *IN98         IFEQ      *OFF
120300951213     C     *IN03         ANDEQ     *OFF
120400021015     c                   Eval      Pa5Mbr = MbrFil
120500021015     c                   Movel     Pa5Mbr4       PA5FIL
120600950128     C                   MOVEL     'F'           PA5FIL
120700950128     C                   MOVE      PARAM5        KPJBU
120800950203     C                   MOVEL     'LR22'        KCOAZ
120900950203     C                   CALL      'BCH10'
121000950203     C                   PARM                    KPJBA
121100950203     C                   SETON                                        98
121200961121    2C                   ENDIF
121300070831     c
121400070831     c* Se esiste anomalia di collo arrivato non partito
121500070831     c*  chiano in update il file fnart e aggiorno i dati della partenza
121600070831     C                   Z-ADD     050           COMCAA
121700070831     C     KANM          CHAIN     FNANM000                           33
121800070831    2C     *IN33         IFEQ      *OFF
121900070831     c     kart          chain     fnart01l
122000070831    3c                   if        %found(fnart01l)
122100070831     c* Se manca data partenza la aggiorno, altrimenti data uscita transito
122200070831    4c                   if        anmcdu=arbtfp
122300070831     C                   Z-ADD     anmdao        ARTDFV
122400070831     C                   Z-ADD     anmnfv        ARTnFV
122500070831     C                   Z-ADD     88            ARTNPP
122600070831    5c                   if        arbtfp=arbtfa
122700070831     C                   Z-ADD     90            ARTNPP
122800070831    5c                   endif
122900070831     C                   movel     'S'           ARTABN
123000070831   x4c                   else
123100070831     C                   Z-ADD     anmdao        ARTDUT
123200070831     C                   Z-ADD     89            ARTPUT
123300070831     C                   Z-ADD     anmcdu        ARTFLP
123400070831     C                   movel     'S'           ARTABN
123500070831    4c                   endif
123600070831     c
123700070831    4c                   if        lnantw<>'EEX' and lnantw<>'EUP' and
123800070831     c                             lnantw<>'FED' and lnantw<>'DPD'
123900070903     c                   update    fnart001
124000070831    4c                   endif
124100070831    3c                   endif
124200070831    2c                   endif
124300961122     C**
124400000111     C** SE LNA ESTERA CONTROLLO SE ESISTE EVENTO COLLO 2 /6A/6
124500020226    2C     lnaNTW        IFEQ      'EEX'
124600020226    2C     lnaNTW        OREQ      'EUP'
124700020226    2C     lnaNTW        OREQ      'FED'
124800060322    2C     lnaNTW        OREQ      'DPD'
124900961122     C                   CLEAR                   WDEV
125000961122     C                   CLEAR                   WCEV
125100961122     C* APRO IL FILE
125200961122    3C     WAPER         IFEQ      ' '
125300961122     C                   OPEN      FNEVS02L
125400961122     C                   MOVEL     'S'           WAPER             1
125500961122    3C                   ENDIF
125600961122     C**
125700961122     C                   MOVEL     'E2 '         KCEV
125800020809    3C                   DO        3             XX
125900961122     C     KEVS          SETLL     FNEVS02L
126000961122     C     KEVS          READE     FNEVS02L                               33
126100961122    4C     *IN33         DOWEQ     *OFF
126200970602    5C     EVSDEV        IFGT      WDEV
126300961122     C     WDEV          OREQ      0
126400961122     C                   MOVEL     EVSDEV        WDEV
126500961122     C                   MOVEL     EVSCEV        WCEV
126600961122    5C                   ENDIF
126700961122     C**
126800961122     C* AGGIORNO L'EVENTO COL NUMERO DI SPEDIZIONE
126900961122     C                   Z-ADD     BLPAAS        EVSAAS
127000961122     C                   Z-ADD     BLPLNP        EVSLNP
127100961122     C                   Z-ADD     BLPNSP        EVSNSP
127200961122     C                   CLEAR                   EVSFTR
127300961122     C                   CLEAR                   EVSDTR
127400961122     C                   UPDATE    FNEVS000
127500961122     C*
127600961122     C     KEVS          READE     FNEVS02L                               33
127700961122    4C                   ENDDO
127800961122     C**
127900961122     C* AL 2 GIRO CERCO LE DISCOR.6  --> COLLO ARRIVATO MA A TERRA
128000020809    4C     XX            IFEQ      1
128100961122     C                   MOVEL     'E6 '         KCEV
128200961122    4C                   ENDIF
128300970602     C* AL 3 GIRO CERCO LE DISCOR.6B --> COLLO ARRIVATO MA IN PARTENZA
128400020809    4C     XX            IFEQ      2
128500970602     C                   MOVEL     'E6B'         KCEV
128600961122    4C                   ENDIF
128700961122    3C                   ENDDO
128800961122     C* SE TROVATA AGGIORNO IL DETTAGLIO COLLI
128900961122    3C     WDEV          IFGT      0
129000961122     C* SE ERA COLLO RIAMSTO A TERRA ARTDTR DEVE ESSERE = 0
129100970602    4C     WCEV          IFEQ      'E6 '
129200961122     C                   CLEAR                   ARTDTR
129300961122     C                   ELSE
129400961122     C* ALTRIMENTI IMPOSTO LA DATA DELL'EVENTO
129500961122     C                   Z-ADD     WDEV          ARTDTR
129600961122    4C                   ENDIF
129700961122     C* AGGIORNO RECORD
129800961122     C                   UPDATE    FNART001
129900961122     C* DISALLOCO RECORD
130000961122   X3C                   ELSE
130100070903     C                   update    FNART001
130200961122    3C                   ENDIF
130300961122     C*
130400961122    2C                   ENDIF
130500960529     C*
130600960529     C* LEGGO LA SPUNTA PARTENZA DEL COLLO
130700040317     C*  PER REPERIRE LA DATA DEL FOGLIO VIAGGIO PARTENZA, AI FINI DI
130800040317     C*  DETERMINARE IL TERMINAL DI ARRIVO DELLA LINEA DI ARRIVO
130900040317     c*  In realtà prendiamo la data della prima spunta partenza che
131000040317     c*  troviamo del collo, che potrebbe essere anche quella di un
131100040317     c*  p.o. di transito o di disguido. Ma sono casi molto rari ed
131200040317     c*  un problema si potrebbe presentare soltanto in caso di
131300040317     c*  cambio del terminal di arrivo della lna tra il momento in
131400040317     c*  cui parte/arriva il collo, ed il momento in cui
131500040317     c*  parte/arrva la bolla -vsito che siamo nel caso di manca rec-
131600040317     c*  con il terminal di arrivo calcolato richiama LS44 solo x le
131700040317     c*  spunte dell'area di arrivo, oper aggiornare la bolla in arr
131800040317     c*  Ma se cambia terminal in base alla data spunta le aree di arr
131900040317     c*  possono essere 2. allora si dovrebbe o duplicare dei controlli
132000040317     c*  che il pgm ls44 già fa, o richimare ls44 in modo che aggiorni
132100040317     c*  solo fnarb Ma data la eccezionalità dei casi, non facciamo
132200040317     c*  nulla e teniamo il pgm come è
132300040317     c*
132400960603     C                   EXSR      DFVPAR
132500961121     C*
132600960617     C* PRENDO IL TERMINAL DI ARRIVO DELLA LNA
132700020809     C                   CLEAR                   FNLV55DS
132800971114     C                   MOVEL     'A'           D55TPT
132900971114     C                   MOVEL     ARTLNP        D55LNP
133000971114     C                   MOVEL     ARTLNA        D55LIN
133100971114     C                   MOVEL     WDFV          D55DRF
133200971114     C                   CALL      'FNLV55R'
133300020809     C                   PARM                    FNLV55DS
133400971114     C                   MOVEL     'S'           WCAL55            1
133500971114     C**
133600971114     C     D55ERR        IFNE      ' '
133700971114     C                   MOVEL     D55LIN        D55TFA
133800971114     C                   ENDIF
133900971114     C                   MOVEL     D55TFA        LNATFA
134000950130     C*
134100950130     C* CERCO UNA SPUNTA DEL DETTAGLIO COLLO
134200070123     C     KSPP          CHAIN     FNBRV07L                           33
134300960529     C*
134400961121    2C     *IN33         DOWEQ     *OFF
134500960705     C*
134600960705     C* SE CATEGORIA 1 ESCLUDO SE IL FOGLIO E' UN NORMALE: ELABORO
134700960705     C*  SOLO SE E' DEFLUENZA
134800961121    3C     BRVNPG        IFEQ      1
134900960705     C                   CLEAR                   KEY
135000960705     C                   CLEAR                   WELAB
135100960705     C*
135200000110     C                   Z-ADD     BRVNFV        KNFV
135300040317     C                   Z-ADD     BRVfgs        Kfgs
135400020809     C     K02FGV01      chain     FNFGV01L                           36
135500031203     c     *in36         ifeq      *off
135600031203     C                   MOVEL     FGVTTR        WTTR
135700031203   X4C                   ELSE
135800960705     C     KFVA          CHAIN     FNFVA01L                           36
135900980623     C                   MOVEL     FVATTR        WTTR
136000031203    4C                   ENDIF
136100960705     C*
136200980623    4C     WTTR          IFNE      *BLANKS
136300980623     C     WTTR          LOOKUP    TDF                                    36
136400980623     C  N36              MOVEL     'N'           WELAB             1
136500961121    4C                   ENDIF
136600960705     C*
136700961121    3C                   ENDIF
136800960705     C*
136900960705     C** ELABORO LA CATEGORIA 1 SOLO PER LA DEFLUENZA E LE ALTRE CATEG
137000961121    3C     BRVNPG        IFNE      1
137100960705     C     WELAB         OREQ      ' '
137200960617     C**
137300960603     C* LEGGERE LE ECCEZIONI TERMINAL
137400960603     C**
137500020809     C                   CLEAR                   FNLV55DS
137600971114     C                   MOVEL     'A'           D55TPT
137700971114     C                   MOVEL     ARTLNP        D55LNP
137800031203     C                   MOVEL     BRVPES        D55LIN
137900971114     C                   MOVEL     WDFV          D55DRF
138000971114     C                   CALL      'FNLV55R'
138100020809     C                   PARM                    FNLV55DS
138200971114     C                   MOVEL     'S'           WCAL55            1
138300971114     C**
138400971114     C     D55ERR        IFNE      ' '
138500971114     C                   MOVEL     D55LIN        D55TFA
138600971114     C                   ENDIF
138700031203     C                   MOVEL     D55TFA        PESTFA
138800960529     C*
138900031203    5C     LNATFA        IFEQ      PESTFA
139000960603     C** IMPOSTO L'AS GESTIONE DELLA ANOMALIA
139100970114     C                   CLEAR                   OFV
139200960529     C                   EXSR      CHIAMO
139300970115     C*
139400960529    5C                   ENDIF
139500961121    3C                   ENDIF
139600960529     C**
139700070123     C     KSPP          READE     FNBRV07L                               33
139800961121    2C                   ENDDO
139900961121     C**
140000961121    1C                   ENDIF
140100950130     C*
140200950130     C                   ENDSR
140300960603     C**---------------------------------------------------------
140400960603     C*  LEGGO LA SPUNTA PARTENZA DEL COLLO PER PRENDERE LA DATA FV
140500960603     C**---------------------------------------------------------
140600960603     C     DFVPAR        BEGSR
140700960603     C                   CLEAR                   WDFV
140800070123     C     KSPP          CHAIN     FNBRV07L                           34
140900960603    1C     *IN34         DOWEQ     *OFF
141000960603    2C     BRVNPG        IFEQ      1
141100960603     C*
141200000110     C                   Z-ADD     BRVNFV        KNFV
141300040317     C                   Z-ADD     BRVfgs        Kfgs
141400020809     C     K02FGV01      chain     FNFGV01L                           35
141500960603     C**
141600960603     C* ESCLUDO I TRAINI DI DEFLUENZA
141700031216    3C     *IN35         IFEQ      *OFF
141800960603     C**
141900031216    5C     FGVTTR        IFNE      ' '
142000031216     C     FGVTTR        LOOKUP    TDF                                    35
142100031216    5C                   ENDIF
142200980623     C**
142300031216    5C     FGVTTR        IFEQ      ' '
142400031216     C     *IN35         OREQ      *OFF
142500980623     C**
142600031216    6C     FGVDFV        IFGT      WDFV
142700031216     C                   MOVEL     FGVDFV        WDFV
142800031216    6C                   ENDIF
142900031216    5C                   ENDIF
143000960603     C**
143100960603   X3C                   ELSE
143200960603     C**
143300960603     C     KFVA          CHAIN     FNFVA01L                           35
143400031216    4c     *in35         ifeq      *off
143500031216    5C     FVATTR        IFNE      ' '
143600031216     C     FVATTR        LOOKUP    TDF                                    35
143700031216    5C                   ENDIF
143800031216     C**
143900031216    5C     FVATTR        IFEQ      ' '
144000031216     C     *IN35         OREQ      *OFF
144100031216    6C     FVADFV        IFGT      WDFV
144200960603     C                   MOVEL     FVADFV        WDFV
144300031216    6C                   ENDIF
144400031216    5c                   endif
144500031216    4c                   endif
144600960603     C*
144700960603    3C                   ENDIF
144800960603    2C                   ENDIF
144900070123     C     KSPP          READE     FNBRV07L                               34
145000960603    1C                   ENDDO
145100960603     C**
145200960603     C* SE NON L'HO TROVATA PRENDO LA DATA DI CREAZIONE DELLA ANOMALIA
145300960603     C     WDFV          IFEQ      0
145400960603     C                   Z-ADD     ANMDAO        WDFV
145500960603     C                   ENDIF
145600960603     C                   ENDSR
145700950130     C**---------------------------------------------------------
145800950130     C** CHAIMO PGM DI AGGIORNAMENTO BOLLE DA SPUNTA
145900950130     C**---------------------------------------------------------
146000950130     C     CHIAMO        BEGSR
146100070228     c                   clear                   fnls45ds
146200070228     c                   movel     'S'           dls45ric
146300070228     c                   move      brvpes        dls45pes
146400070228     c                   move      simfel        dls45tfp
146500070228     c                   call      'FNLS45R'
146600070228     c                   parm                    fnls45ds
146700070228     c                   parm                    dsbrv
146800950130     C*
146900950130     C                   SETON                                        02
147000950130     C                   ENDSR
147100950123     C***--------------------------------------------------------
147200080411     C** invio msg in sede
147300911003     C**---------------------------------------------------------
147400911003     C     STAERR        BEGSR
147500111104     c* Mando e_mail
147600111104     c                   clear                   msgtes
147700111104     c                   clear                   msgdet
147800111104     C                   MOVEL     MSG(1)        msgtes
147900111104     c                   eval      %subst(msgtes:26:3)=%editc(simfel:'X')
148000111104     c                   eval      %subst(msgtes:35:10)=mbrfil
148100080411
148200080411     C                   MOVEL     BLPRSM        STARSM           20
148300080603     c     kblp2         chain     fnblp01l
148400080603     c                   if        not %found(fnblp01l)
148500080603     c                   clear                   w_blpctm
148600080603     c                   endif
148700080411
148800111104     c                   eval      msgdet='Bolla: '+%editc(blpaas:'X') +' '
148900080411     c                             + %editc(blplnp:'X')+' '+%editc(blpnrs:'Z')
149000080411     c                             + ' '+%editc(blpnsp:'Z')+' LNA '
149100111104     c                             + %editc(blplna:'Z')+' Mit:'+ starsm +
149200111104     c                             '       Cod.Trattamento merce:' +W_blpctm
149300080411     c* Bolla già esistente
149400111104     c   06              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(2)
149500111104     c** 06              eval      ym5t03=msg(2)
149600080411     c* non trovato dettaglio colli
149700111104     c   07              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(3)
149800111104     c** 07              eval      ym5t03=msg(3)
149900080411     c* Segnacollo esistente
150000080411     c                   if        *in04
150100111104     c   08              eval      %subst(msg(4):22:7)=%editc(artnsc:'Z')
150200111104     c   08              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(4)
150300111104     c  N08              eval      %subst(msg(5):22:7)=%editc(artnsc:'Z')
150400111104     c  N08              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(5)
150500111104     c** 08              eval      ym5t03=msg(4)
150600111104     c**N08              eval      ym5t03=msg(5)
150700111104     c**                 eval      %subst(msgT03:22:7)=%editc(artnsc:'Z')
150800080411     c                   endif
150900080411     C*
151000120305     C                   eval      wrkEml= 'ced@brt.it'
151100111104     C                   call(e)   'TIS701C'
151200111104     C                   parm                    wrkEml          253
151300111104     C                   PARM                    MSGTES           44
151400111104     C                   PARM                    MSGDET         5000
151500080411     C*
151600000622     C                   EXSR      DLTRIC
151700000622     C                   ENDSR
151800000622     C***--------------------------------------------------------
151900000622     C** DELETO RECORD DA FILE DI RICEZIONE PER BOLLA DOPPIA
152000000622     C**---------------------------------------------------------
152100000622     C     DLTRIC        BEGSR
152200921123     C*
152300921123     C* C A N C E L L O  SOLO SE BOLLA DOPPIA
152400921123     C   06              DO
152500950118     C                   DELETE    FNBLPE00
152600920917     C                   END
152700911003     C*
152800911003     C                   ENDSR
152900040317     C*
153000951011     C* CONTROLLO SE LOCALE SE DEVO AGGIORNARE LA DATA PARTENZA------*
153100031202     C     CTRDET        BEGSR
153200031202     C                   Z-ADD     1             KNPG
153300070123     C     KBRV9         CHAIN     FNBRV09L                           35
153400951011     C* TROVATA SPUNTA LOCALE PARTITA NON SUL FV IN ELABORAZIONE
153500980623     C*  --> AGGIORNO  SE NON SI TRATTA DI FOGLIO DEFLUENZA E SE
153600980623     C*  IL FOGLIO E' CHIUSO
153700031202    1C     *IN35         DOWEQ     *OFF
153800031202   1AC     BRVNFV        IFNE      PARNFV
153900041006     c* ignoro la spunta se fatta su foglio viaggio partenza errato
154000041006     c* (lna del collo non presente sul foglio viaggio)
154100041006     c     brvftr        andne     'L'
154200951011     C*
154300951011     C* DATA PARTENZA MERCE E NUMERO PISTOLA PARTENZA SOLO SE LOCALE
154400031202     C                   Z-ADD     BRVNFV        KNFV
154500040317     C                   Z-ADD     BRVfgs        Kfgs
154600031202     C     K02FGV01      chain     FNFGV01L                           35
154700031202    2C     FGVFCF        IFEQ      'S'
154800980623     C* CONTROLLO IL TIPO TRAINO
154900031202    3C     FGVTTR        IFNE      ' '
155000031202     C     FGVTTR        LOOKUP    TDF                                    36
155100031202    3C                   ENDIF
155200980623     C**
155300031202    3C     FGVTTR        IFEQ      ' '
155400031202     C     *IN36         OREQ      *OFF
155500031203     c                   clear                   flg15
155600031202     C                   CLEAR                   KPJBU
155700031216     c                   movel     param         paramx
155800031216     c                   z-add     brvnfv        parnfv
155900031216     c                   z-add     fgvdfv        pardtp
156000031202     C                   MOVEL     PARAM         KPJBU
156100031202     c                   call      'FNLR15R'
156200031202     c                   parm                    kpjba
156300031202     c                   parm                    dsbrv
156400031203     c                   parm                    flg15             1
156500031216     c                   movel     paramx        param
156600031202    3C                   ENDIF
156700031202    2C                   ENDIF
156800951011     C*
156900031202   1AC                   ENDIF
157000070123     C     KBRV9         READE     FNBRV09L                               35
157100031202    1C                   ENDDO
157200951011     C*
157300031202     C                   ENDSR
157400960826     C*
157500960827     C* CANCELLAZIONE ANOMALIA 50-COLLO ARRIVATO NON PARTITO --------*
157600960826     C     ANM050        BEGSR
157700960826     C                   Z-ADD     50            COMCAA
157800990510     C     KANM          CHAIN     FNANM000                           33
157900960826    1C     *IN33         IFEQ      *OFF
158000020809     C                   CLEAR                   FNLR33DS
158100960826     C                   Z-ADD     ANMNR2        D33NRR
158200960826     C                   MOVE      'S'           D33MAC
158300960826     C                   CALL      'FNLR33R'
158400020809     C                   PARM                    FNLR33DS
158500960826     C                   END
158600960826     C                   ENDSR
158700080411     c*----------------------------------------------------------------
158800911001**         CMD5
158900021008CHKOBJ OBJ(FNBLPE0R) OBJTYPE(*FILE)     MBR(M234567890)
159000911001**         CMD6
159100021008ALCOBJ OBJ((FNBLPE0R *FILE *EXCL           M234567890))
159200911002**         CMD9
159300021008OVRDBF FILE(FNBLPE0R)                   MBR(M234567890)
159400050804**         CmdDSPLOG
159500050804DSPJOBLOG OUTPUT(*PRINT)
159600000622**
159700080411RICEZIONE BOLLE ARRIVO A xxx: MBR Mxxxyyy                               1
159800111104Bolla già esistente .                                                   2
159900111104Non trovato dettaglio colli .                                           3
160000111104Segnacollo esistente xxxxxxx : SOSTITUITO .                             4
160100111104Segnacollo esistente xxxxxxx : NON sostituito .                         5
