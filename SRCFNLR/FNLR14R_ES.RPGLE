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
001400120629     FFNblt01L  IF   E           K DISK
001500120629     FFIART01L  UF   E           K DISK
001600120702     f                                     prefix(arw:3)
001700060220     FFiAR401L  UF A E           K DISK
001800051013     FFIAR601L  if   E           K DISK
001900051116     FFiAR901L  iF   E           K DISK
002000921022     F* BOLLE PARTENZE RICEZIONE
002100950118     FFNBLPE0R  UF   E           K DISK    USROPN
002200921022     F*
002300990510     FFNANM02L  IF   E           K DISK
002400990510     F                                     INFDS(FNANM2)
002500990510     FFNANM01L  IF   E           K DISK
002600990510     F                                     RENAME(FNANM000:FNANM001)
002700990510     F                                     INFDS(FNANM1)
002800961121     FFNEVS02L  UF   E           K DISK    USROPN
002900070123     FFNBRV07L  IF   E           K DISK
003000070123     FFNBRV09L  IF   E           K DISK
003100070123     F                                     RENAME(FNBRV000:FNBRV009)
003200951011     FFNAGB01L  O  A E           K DISK
003300061031     ffnblp01l  if   e           k disk    prefix(w_)
003400030519
003500021008     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
003600021008     D CMD6            S             55    DIM(1) CTDATA PERRCD(1)
003700021008     D CMD9            S             55    DIM(1) CTDATA PERRCD(1)
003800050804
003900050804     d CmdDSPLOG       s             55    Dim(1) Ctdata Perrcd(1)
004000911011     D*
004100980623     D TDF             S              1    DIM(10)                              COSTANTI STAMPA
004200000622     D DVA             S              3    DIM(10)                              DIVISE ACC.ITAA
004300011025     D DEC             S              2  0 DIM(10)                              NUMERO DECIMALI
004400010118     D MSG             S             70    DIM(10) CTDATA PERRCD(1)             MSG POSTA ELET
004500040317     D NFV             S              5  0 DIM(10)                              MSG POSTA ELET
004600020226
004700040317     d kfgs            s                   like(brvfgs)
004800040317     d lnantw          s                   like(§ogntw)
004900051013     d lnpntw          s                   like(§ogntw)
005000030401     d wtfplna         s                   like(d55tfp)
005100020809     D*
005200020809     D KCEV            S                   LIKE(EVSCEV)
005300020809     D WCEV            S                   LIKE(EVSCEV)
005400020809     D WDEV            S                   LIKE(EVSDEV)
005500020809     D WDFV            S                   LIKE(FGVDFV)
005600020809     D KNFV            S                   LIKE(FGVNFV)
005700020809     D KNPG            S                   LIKE(BRVNPG)
005800020809     D LNATFA          S                   LIKE(ARBTFA)
005900031203     D PESTFA          S                   LIKE(ARBTFA)
006000020809     D SAVLNP          S                   LIKE(BLPLNP)
006100031210     D SAVLNA          S                   LIKE(BLPLNA)
006200020809     D WTRC            S                   LIKE(AR4TRC)
006300020809     d WTTR            s                   like(FGVTTR)
006400050314     D ktrc            S                   LIKE(ar6trc)
006500050314     D fineksc         S              4
006600031216     D paramx          S                   LIKE(param)
006700070228     D*COMDFS          S              8  0 INZ
006800070228     D*COMHMS          S              4  0 INZ
006900020809     D COMCAA          S              3  0 INZ
007000020809     D COMNCD          S              8  0 INZ
007100020809     D COMNCA          S              8  0 INZ
007200020809     D W0140           S             14  0 INZ
007300020809     D DATEU           S              8  0 INZ
007400020809     D UDATE8          S              8  0 INZ
007500020809     D COD             S              2    INZ
007600020809     D KEY             S              8    INZ
007700020809     D MBR             S              3  0 INZ
007800020809     D XX              S              3  0 INZ
007900021008     D VAR11           S             11    INZ
008000021008     D VAR12           S             12    INZ
008100021008     D LUNG            S             15  5 INZ(55)
008200020809     D COMMAN          S             80    INZ
008300031210     d lnapks          s              1
008400070322     d lnageo          s              1
008500020809
008600020809     D KPJBA         E DS
008700020809     D DSCV          E DS
008800020809     D DSTV          E DS
008900020809     D DS7N          E DS
009000020809     D DS3A          E DS
009100020809     D DS7A          E DS
009200020809     D DSBL4A        E DS
009300070322     d dsbl4l        e ds
009400020809     D DSBL4J        E DS
009500020809     D DGEI          E DS
009600020809     D DGED          E DS
009700020809     D OG143         E DS
009800070322     d og148         e ds
009900031027     d Og150         e ds
010000020809     D YEURCODS      E DS
010100020809     D UT§DSE0F      E DS
010200020809     D CNCR80        E DS
010300020809     D FNLR33DS      E DS
010400020809     D FNLV55DS      E DS
010500020809     D TIBS02DS      E DS
010600070228     D fnls45ds      E DS
010700020809     d trul21ds      e ds
010800070123     D DSBRV         E DS                  EXTNAME(FNBRV00F)
010900020226
011000950118     D***
011100950118     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
011200950118     D***
011300950118     D WLBDAT          DS                  INZ
011400950118     D  G02DAT                 1      8  0
011500950118     D  G02INV                 9     16  0
011600950118     D  G02ERR                17     17
011700950118     D  G02TGI                18     22  0
011800950919     D* PARAMETRI PASSATI
011900950919     D PARAM           DS
012000950919     D  PARDTP                 1      8  0
012100950919     D  PARNFV                 9     13  0
012200960806     D  PARDEF                14     14
012300051006     D  tentativi            234    236
012400051006     D  LEGFIL               237    246
012500950919     D  MBRFIL               247    256
012600950919     D  ALFFIL               248    250
012700950919     D* PER ESPLOSIONE SPUNTE IN FILE DI RICEZIONE
012800950128     D PARAM5          DS
012900950128     D  PA5FIL               237    246    INZ('FXXX999999')
013000021015     D  PA5MBR4              247    250    INZ
013100021015     D  PA5MBR               247    256    Inz
013200020809     D                 DS
013300020809     D  BLPAAS                 1      4  0
013400020809     D  BLPMGS                 5      8  0
013500020809     D  BLPDSP                 1      8  0
013600020809     D                 DS
013700020809     D  WNUM                   1     13
013800020809     D  NUM                    1     13
013900020809     D                                     DIM(13)
014000020809     D                 DS
014100020809     D  WNUME                  1     14
014200020809     D  NUE                    1     14
014300020809     D                                     DIM(14)
014400990510     D FNANM1          DS
014500960424     D  ANMNR1               397    400B 0
014600990510     D FNANM2          DS
014700960424     D  ANMNR2               397    400B 0
014800000622     D                 DS
014900050804     D* DEFINIZIONE COSTANTI
015000050804     D CUTI            C                   CONST('EDP*      ')
015100011210
015200011210     d                sds
015300011210     d  nompgm                 1     10
015400020809
015500911003     C****************************************************************
015600911003     C*  RIEPILOGO INDICATORI
015700911003     C***************************************************************
015800911016     C* 02    - LACIO PGM ABILITAZIONE MERCE
015900911016     C* 03    - SONO ME STESSO NON SCRIVO CMRIC
016000920917     C* 04    - DETTAGLIO DOPPIO
016100050314     C* 06    - off - bolla non presente
016200921123     C* 07    - MANCA DETTAGLIO COLLI PER BOLLA CON COLLI NO SEQ
016300950905     C* 08    - DETTAGLIO DOPPIO - AGGIORNATO PERCHE' GIA' CONSEGNATO
016400940322     C* 10    - SPUNTA DAL MIO TERMINAL
016500940322     C* 18/19 - INDICATORI DI TIPO RECORD
016600911003     C* 20    - NON ALLOCATI DEGLI AGGETTI ESCO DA PROGRAMMA
016700911003     C* 31    - LETTURA FLBLPE00
016800911003     C* 32/33 - COMODO
016900990906     C* 34    - COMODO
017000960705     C* 35/36 - COMODO
017100911003     C*****************************************************************
017200000000     C     *ENTRY        PLIST
017300000000     C                   PARM                    KPJBA
017400950919     C                   MOVEL     KPJBU         PARAM
017500000000     C                   Z-ADD     1             CODUT
017600911001     C                   CALL      'X§PARUT'
017700020809     C                   PARM                    UT§DSE0F
017800911001     C                   MOVEL     REC80         CNCR80
017900900618     C*---------------------------------------------------------------*
018000950118     C     KBLP2         KLIST
018100950118     C                   KFLD                    BLPAAS
018200950118     C                   KFLD                    BLPLNP
018300950118     C                   KFLD                    BLPNRS
018400950118     C                   KFLD                    BLPNSP
018500050314     C     Kar6          KLIST
018600050314     C                   KFLD                    BLPAAS
018700050314     C                   KFLD                    BLPLNP
018800050314     C                   KFLD                    BLPNRS
018900050314     C                   KFLD                    BLPNSP
019000050314     C                   KFLD                    ktrc
019100070831     C     Kart          KLIST
019200070831     C                   KFLD                    BLPAAS
019300070831     C                   KFLD                    BLPLNP
019400070831     C                   KFLD                    BLPNRS
019500070831     C                   KFLD                    BLPNSP
019600070831     C                   KFLD                    artnsc
019700911014     C     KANM          KLIST
019800911014     C                   KFLD                    ARTFLS
019900911014     C                   KFLD                    ARTLNA
020000911014     C                   KFLD                    ARTNRS
020100950119     C                   KFLD                    ARTNSC
020200020809     C                   KFLD                    COMCAA
020300961121     C     KEVS          KLIST
020400961121     C                   KFLD                    ARTFLS
020500961121     C                   KFLD                    ARTLNA
020600961121     C                   KFLD                    ARTNRS
020700961121     C                   KFLD                    ARTNSC
020800961121     C                   KFLD                    KCEV
020900960424     C     KANM1         KLIST
021000990510     C                   KFLD                    BLPAAS
021100960424     C                   KFLD                    BLPLNP
021200960424     C                   KFLD                    BLPNRS
021300960424     C                   KFLD                    BLPNSP
021400020809     C                   KFLD                    COMCAA
021500911014     C     KSPP          KLIST
021600950413     C                   KFLD                    ARTFLS
021700950413     C                   KFLD                    ARTLNA
021800950413     C                   KFLD                    ARTNRS
021900950119     C                   KFLD                    ARTNSC
022000951011     C     KBRV9         KLIST
022100951011     C                   KFLD                    KNPG
022200951011     C                   KFLD                    ARTFLS
022300951011     C                   KFLD                    ARTLNA
022400951011     C                   KFLD                    ARTNRS
022500951011     C                   KFLD                    ARTNSC
022600951011     C                   KFLD                    SIMFEL
022700911002     C     KTAB          KLIST
022800911002     C                   KFLD                    CODUT
022900020809     C                   KFLD                    COD
023000020809     C                   KFLD                    KEY
023100980623     C     KTAB2         KLIST
023200980623     C                   KFLD                    CODUT
023300020809     C                   KFLD                    COD
023400960529     C     KFVA          KLIST
023500960529     C                   KFLD                    BRVFGS
023600000110     C                   KFLD                    KNFV
023700000622     C     KAR4          KLIST
023800000622     C                   KFLD                    BLPAAS
023900000622     C                   KFLD                    BLPLNP
024000000622     C                   KFLD                    BLPNRS
024100000622     C                   KFLD                    BLPNSP
024200000622     C                   KFLD                    WTRC
024300020809     C     K02FGV01      klist
024400020809     C                   kfld                    KNFV
024500040317     C                   kfld                    kfgs
024600950119     C*---------------------------------------------------------------*
024700911011     C* GIRO DATA DEL GIORNO
024800020809     C                   TIME                    W0140
024900950118     C                   MOVE      W0140         G02DAT
025000911011     C                   MOVEL     *BLANK        G02ERR
025100950118     C                   CALL      'XSRDA8'
025200911011     C                   PARM                    WLBDAT
025300020809     C                   Z-ADD     G02INV        DATEU
025400020809     C                   Z-ADD     G02DAT        UDATE8
025500950214     C*
025600911003     C                   SETON                                        OF
025700040317     c                   eval      xx=1
025800911011     C*
025900911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
026000911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
026100911001     C     MBRFIL        IFEQ      *BLANK
026200021008     C                   MOVEL     '*FIRST'      MBRFIL
026300911001     C                   END
026400911001     C* MEMBRO LUNGO 3
026500020809     C                   MOVEL     ALFFIL        MBR
026600911107     C*
026700911107     C* IMPOSTAZIONI INIZIALI
026800911107     C                   EXSR      CARTAB
026900031027
027000911001     C*
027100911001     C* ALLOCO MEMBRI
027200911001     C                   EXSR      CAEX
027300911001     C   20              GOTO      FINE
027400911001     C*
027500950118     C                   READ      FNBLPE00                               31
027600911003     C*
027700931125     C     *IN31         DOWEQ     *OFF
027800921123     C                   SETOFF                                       040607
027900040317     C                   SETOFF                                       03
028000000622     C     BLPLNP        IFNE      SAVLNP
028100000622     C     BLPLNP        CHAIN     AZORG01L                           32
028200000622     C  N32              MOVEL     ORGDE3        OG143
028300000622     C   32              CLEAR                   OG143
028400020226     c                   eval      lnpntw = §ogntw
028500000622     C                   Z-ADD     BLPLNP        SAVLNP
028600000622     C                   ENDIF
028700031210     c* A cambio di lna devo memorizzare se è lna con picking attivo
028800041215     c* o lna per la quale stampare i DDT SI in arrivo
028900070322      * o se lna abilitata alle 'telefonate GEO'
029000031210     C     BLPLNA        IFNE      SAVLNA
029100060515     c                   clear                   lnapks
029200070322     c                   clear                   lnageo
029300031210     c* controllo se lna gestita da altri
029400031210     c                   clear                   fnlv55ds
029500031210     c                   movel     '6'           D55TPT
029600031210     c                   z-add     blplna        d55lin
029700031210     c                   z-add     dateu         d55drf
029800031210     c                   movel     'S'           wcal55
029900031210     c                   call      'FNLV55R'
030000031210     c                   parm                    fnlv55ds
030100060515      * se è un p.o. gestito da altri controllo se la lna stessa ha il picking attivo
030200060515     c                   if        d55lin <> d55tfa
030300060515     c     d55lin        chain     azorg01l                           32
030400060515     c  n32              movel     orgdf0        og150
030500070322     c  n32              eval      og148 = orgde8
030600060515     c   32              clear                   og150
030700070322     c   32              clear                   og148
030800120604     c**                 If        §OgPck = 'S' and §OgPkd <> 'S'
030900120604     c**                           or §ogsda = 'S'
031000120604     c**                 eval      lnapks = 'S'
031100120604     c**                 endif
031200070322      * controllo se LNA è abilitata a telefonate GEO
031300070322     c                   if        §oggeot = 'S'
031400070322     c                   eval      lnageo = 'S'
031500070322     c                   endif
031600060515     c                   endif
031700060515      * controllo se c'è il picking attivo
031800120604     c**                 if        lnapks = *blanks
031900120604     C**   d55tfa        CHAIN     AZORG01L                           32
032000120604     C**N32              MOVEL     ORGDf0        OG150
032100120604     C** 32              CLEAR                   OG150
032200120604if  5c**                 If        §OgPck = 'S' and §OgPkd <> 'S'
032300120604     c**                           or §ogsda = 'S'
032400120604     c**                 eval      lnapks = 'S'
032500120604     c**                 else
032600120604     c**                 clear                   lnapks
032700120604     c**                 endif
032800120604     c**                 endif
032900120604     c
033000070322      * se lna non abilitata a telefonate GEO controllo la sua capofila
033100070322     c                   if        lnageo = *blanks
033200070322     c     d55tfa        chain     azorg01l                           32
033300070322     c  n32              eval      og148 = orgde8
033400070322     c   32              clear                   og148
033500070322     c                   if        §oggeot = 'S'
033600070322     c                   eval      lnageo = 'S'
033700070322     c                   endif
033800070322     c                   endif
033900031210     C                   Z-ADD     BLPLNA        SAVLNA
034000031210     C                   ENDIF
034100040317     c
034200040317     c* Controllo bolla per bolla se si tratta di locale, non più in base
034300040317     c*  al membro ma in base ai terminal di partenza/arrivo
034400040330    1c                   if        blptfa=blptfp
034500040317     c                   seton                                        03
034600040317     c                   endif
034700000713     C*
034800950118     C     KBLP2         SETLL     FNARB01L                               06
034900060220     c
035000000622     C     *IN06         IFEQ      *ON
035100060220     c
035200060220     C**   §B4ASO        ANDNE     '&'
035300060220     C**   *IN06         OREQ      *ON
035400060220     C**   BL2CEI        ANDNE     '&'
035500000622     C* 06 ON E BOLLA NON AUTOGENERATA - ERRORE GIA' ESISTENTE
035600060220     c* 20/02/06 : tolgo il discorso delle autogenerate perchè valeva solo
035700060220     c*            per Poste; se bolle già presente sempre errore
035800000622     C                   EXSR      STAERR
035900000622     C                   ELSE
036000000622     C* 06 OFF OPPURE ON MA BOLLA AUTOGENERATA: CREO O AGGIORNO BOLLA
036100060220     C** 06KBLP2         CHAIN     FNARB01L                           32
036200000622     C                   EXSR      CRTBOL
036300040317     c
036400040317     c* Memorizzo il numero foglio se bolla locale per la sottomissione
036500040317     c                   if        *in03
036600040317     c     blpnfv        lookup    nfv                                    36
036700040317     c                   if        not *in36
036800040317     c                   z-add     blpnfv        nfv(xx)
036900040317     c                   add       1             xx                3 0
037000040317     c                   endif
037100040317     c                   endif
037200040317     c
037300000622     C                   ENDIF
037400060220     c
037500950118     C                   READ      FNBLPE00                               31
037600911001     C                   END
037700031203     c* richiamo fnlr15r per la chiusura file e la scrittura dell'ultim
037800031203     c* agb
037900031203     c                   movel     'C'           flg15
038000031203     c                   call      'FNLR15R'
038100031203     c                   parm                    kpjba
038200031203     c                   parm                    dsbrv
038300031203     c                   parm                    flg15
038400900911     C*
038500040428     c                   setoff                                       2022
038600900409     C     FINE          TAG
038700040428     c*
038800040428     c* Per 20 on - risottometto ricezione bolle e spunte
038900040428     c                   if        *in20  and not *in22
039000050804     c
039100051006     c                   testn                   tentativi            30
039200051006     c                   move      tentativi     w001a             1
039300051006     c                   if        not *in30 or w001a<'0'
039400051006     c                   eval      tentativi='001'
039500051006     c                   eval      w0030=1
039600051006     c                   else
039700051006     c                   movel     tentativi     w0030             3 0
039800051006     c                   add       1             w0030
039900051006     c                   movel     w0030         tentativi
040000051006     c                   endif
040100051006     c
040200051006     c* max 200 tentativi
040300051006     c                   if        w0030<=200
040400050804     C                   MOVEL     'K'           legfil
040500050804     C                   MOVEl(P)  PARAM         KPJBU
040600040428     c
040700040428     C                   MOVEL     'LR14'        KCOAZ
040800040428     C                   CALL      'BCH10'
040900040428     C                   PARM                    KPJBA
041000040428     c
041100040428     C                   MOVEL     'LR22'        KCOAZ
041200040428     C                   CALL      'BCH10'
041300040428     C                   PARM                    KPJBA
041400040428     C                   SETON                                        98
041500051006     c                   else
041600051006     c
041700051006     c* msg x  pgm non eseguito e da chi allocato
041800051006     c
041900050804     c* richiamo pgm per rifare alcobj
042000051006     c                   call      'FNLR14C1'
042100080407     c                   parm                    fisico           10
042200051006     c                   parm                    mbrfil
042300051006     c
042400050804     c* eseguo dspjoblog per vedere chi mi alloca
042500051006     c                   Movel     CmdDSPLOG(1)  Comman
042600051006     c                   Call      'QCMDEXC'
042700051006     c                   Parm                    Comman
042800051006     c                   Parm                    Lung
042900050804     C
043000111104     c                   clear                   msgtes
043100111104     c                   clear                   msgdet
043200111104     c                   eval      msgtes='RICEZIONE BOLLE: FNLR14R '
043300111104     c                   eval      msgdet='FILE ALLO'+
043400111104     C                             'CATO. VEDI DSPJOBLOG   -   utente:'+knmus
043500050804     C
043600120305     C                   eval      wrkEml= 'ced@brt.it'
043700111104     C                   call(e)   'TIS701C'
043800111104     C                   parm                    wrkEml          253
043900111104     C                   PARM                    MSGTES           44
044000111104     C                   PARM                    MSGDET         5000
044100050804     C*
044200051006     c                   endif
044300050804     c
044400040428    2C                   ENDIF
044500040317     c*
044600040317     c* sottometto lr15 del foglio passato solo se è locale
044700040317     c                   if        nfv(1)>0
044800040317     c     parnfv        lookup    nfv                                    36
044900040317     c                   if        *in36 and pardef<>'S'
045000040317     c                   clear                   kpjbu
045100040317     C                   MOVEL     PARAM         KPJBU
045200040317     C                   CALL      'FNLR15R'
045300040317     c                   PARM                    KPJBA
045400040317     c                   endif
045500040317     c                   endif
045600950919     C*
045700950128     C** LANCIO PGM DI ABILITAZIONE BOLLE ALLA CONSEGNA
045800970509     C     *IN02         IFEQ      *ON
045900970509     C* CHIUDO PGM FNLS44R PER FARE SCRIVERE L'ULTIMO FNAGB
046000070228     C                   MOVEL     'C'           dls45flg
046100070228     C                   CALL      'FNLS45R'
046200070228     c                   parm                    fnls45ds
046300070228     c                   parm                    dsbrv
046400970509     C                   ENDIF
046500971114     C**
046600971114     C     WCAL55        IFEQ      'S'
046700020809     C                   CLEAR                   FNLV55DS
046800971114     C                   MOVEL     'C'           D55TLA
046900971114     C                   CALL      'FNLV55R'
047000020809     C                   PARM                    FNLV55DS
047100971114     C                   ENDIF
047200911016     C*
047300020809     C                   EVAL      *INLR = *ON
047400910322     C*
047500911107     C*--- CARICO TABELLE --------------------------------------------*
047600911107     C     CARTAB        BEGSR
047700921022     C*
047800921022     C* PRENDO TABELLA VARIABILI ARRIVI
047900921022     C                   MOVEL     '7A'          COD
048000921022     C                   MOVEL     *BLANKS       KEY
048100921022     C                   MOVEL     '1'           KEY
048200921022     C     KTAB          CHAIN     TABEL                              33
048300921022     C  N33              MOVEL     TBLUNI        DS7A
048400950130     C   33              CLEAR                   DS7A
048500010110     C* RICERCO FASE
048600921022     C                   MOVEL     '7N'          COD
048700960424     C                   MOVEL(P)  2             KEY
048800921022     C     KTAB          CHAIN     TABEL                              31
048900921022     C  N31              MOVEL     TBLUNI        DS7N
049000980623     C* CARICO TIPI TRAINO DEFLUENZA
049100980623     C                   MOVEL     'TV'          COD
049200020809     C                   CLEAR                   XX
049300980623     C     KTAB2         SETLL     TABEL
049400980623     C     KTAB2         READE     TABEL                                  31
049500980623     C     *IN31         DOWEQ     *OFF
049600980623     C     TBLFLG        IFEQ      ' '
049700980623     C                   MOVEL     TBLUNI        DSTV
049800980623     C     §TVDEF        IFEQ      'S'
049900020809     C                   ADD       1             XX
050000020809     C                   MOVEL     TBLKEY        TDF(XX)
050100980623     C                   ENDIF
050200980623     C                   ENDIF
050300980623     C**
050400980623     C     KTAB2         READE     TABEL                                  31
050500980623     C                   ENDDO
050600000622     C*
050700000622     C* CARICO IN SCHIERA ELENCO DIVISE ACCETTATE IN ITALIA
050800000622     C*
050900000622     C                   MOVEL     'CV'          COD
051000000622     C                   CLEAR                   DVA
051100011025     C                   CLEAR                   DEC
051200020809     C                   CLEAR                   XX
051300000622     C     KTAB2         SETLL     TABEL
051400000622     C     KTAB2         READE     TABEL                                  31
051500000622     C     *IN31         DOWEQ     *OFF
051600000622     C     TBLFLG        IFEQ      ' '
051700000622     C                   MOVEL     TBLUNI        DSCV
051800000622     C     §CVDVA        IFEQ      'S'
051900020809     C                   ADD       1             XX
052000020809     C                   MOVEL     TBLKEY        DVA(XX)
052100011025      * Carico in schiera il numero d decimali della divisa
052200020809     C                   Z-ADD     §CVDEC        DEC(XX)
052300000622     C                   ENDIF
052400000622     C                   ENDIF
052500000622     C**
052600000622     C     KTAB2         READE     TABEL                                  31
052700000622     C                   ENDDO
052800011029      *
052900020809     C                   CLEAR                   TIBS02DS
053000011029     C                   CLEAR                   KPJBU
053100011029     C                   MOVE      'C'           T02MOD
053200011029     C                   MOVE      KNSIF         T02SIF
053300011029     C                   MOVE      'GED'         T02COD
053400011029     C                   MOVEL(P)  1             T02KE1
053500011029     C                   CALL      'TIBS02R'
053600011029     C                   PARM                    KPJBA
053700020809     C                   PARM                    TIBS02DS
053800011029    6C     T02ERR        IFEQ      *BLANKS
053900011029     C                   MOVEL     T02UNI        DGED
054000011029     C                   ELSE
054100011029     C                   CLEAR                   DGED
054200011029    6C                   ENDIF
054300011029      *
054400011029      * Recupero il numero di decimali di §GEDCN
054500011029     C                   MOVEL     'CV'          TBLCOD
054600011029     C                   MOVEL     *BLANKS       TBLKEY
054700011029     C                   MOVEL     §GEDCN        TBLKEY
054800011029     C     KTAB          CHAIN     TABEL                              31
054900011029     C  N31              MOVEL     TBLUNI        DSCV
055000011029     C   31              CLEAR                   DSCV
055100011030      * IMPORTI VARI
055200020809     C                   CLEAR                   TIBS02DS
055300011030     C                   CLEAR                   KPJBU
055400011030     C                   MOVE      'C'           T02MOD                         CONTROLLO
055500011030     C                   MOVE      KNSIF         T02SIF                         S.I.
055600011030     C                   MOVE      'GEI'         T02COD                         TABELLA PROFILI
055700011030     C                   MOVEL(P)  §GEDCN        T02KE1                         PROFILO IN K1
055800011030     C                   CALL      'TIBS02R'
055900011030     C                   PARM                    KPJBA
056000020809     C                   PARM                    TIBS02DS
056100011030    6C     T02ERR        IFEQ      *BLANKS
056200011030     C                   MOVEL     T02UNI        DGEI
056300011030     C                   ELSE
056400011030     C                   CLEAR                   DGEI
056500011030    6C                   ENDIF
056600911107     C                   ENDSR
056700911001     C**---------------------------------------------------------
056800911001     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
056900911001     C**---------------------------------------------------------
057000911001     C     CAEX          BEGSR
057100040428     c                   setoff                                       22
057200020809     C                   MOVE      *BLANK        VAR11
057300020809     C                   MOVE      *BLANK        VAR12
057400911001     C                   MOVE      ')'           VAR11
057500021008     C                   MOVEL     MBRfil        VAR11
057600911001     C                   MOVE      '))'          VAR12
057700021008     C                   MOVEL     MBRfil        VAR12
057800021008     C                   Z-ADD     55            LUNG
057900911001     C*
058000911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
058100911001     C                   SETOFF                                       20
058200950118     C                   MOVE      VAR11         CMD5                            FNBLPE0R
058300911001     C                   MOVEL     *BLANKS       COMMAN
058400020809     C                   MOVEA     CMD5(1)       COMMAN
058500911001     C                   CALL      'QCMDEXC'                            20
058600911001     C                   PARM                    COMMAN
058700911001     C                   PARM                    LUNG
058800040428     c*
058900040428     C* 20 ON - ESCO
059000040428     c                   if        *in20
059100040428     c                   seton                                          22
059200040428     C                   GOTO      ENDCAE
059300040428     c                   endif
059400911002     C* ALLOCO MEMBRO: SE NON RIESCO ESCO DA PROGRAMMA
059500911001     C                   SETOFF                                       20
059600911002     C                   MOVE      VAR12         CMD6
059700911001     C                   MOVEL     *BLANKS       COMMAN
059800911002     C                   MOVEA     CMD6(1)       COMMAN
059900911001     C                   CALL      'QCMDEXC'                            20
060000911001     C                   PARM                    COMMAN
060100911001     C                   PARM                    LUNG
060200911002     C*
060300050804     c                   if        *in20
060400050804     c                   eval      fisico='FNBLPE0R'
060500050804     C                   GOTO      ENDCAE
060600050804     c                   endif
060700911001     C* OVRDBF
060800911002     C                   SETOFF                                       20
060900911002     C                   MOVE      VAR11         CMD9
061000911001     C                   MOVEL     *BLANKS       COMMAN
061100911002     C                   MOVEA     CMD9(1)       COMMAN
061200911002     C                   CALL      'QCMDEXC'                            20
061300911001     C                   PARM                    COMMAN
061400911001     C                   PARM                    LUNG
061500050804     C   20              GOTO      ENDCAE
061600911002     C*
061700950118     C                   OPEN      FNBLPE0R
061800911001     C*
061900911002     C     ENDCAE        ENDSR
062000911002     C**---------------------------------------------------------
062100911002     C** CREO BOLLA ARRIVI
062200911002     C**---------------------------------------------------------
062300980311     C     CRTBOL        BEGSR
062400080407     c*
062500931125     C* S C R I V O   B O L L A
062600990906     C                   EXSR      RIEARB
062700000622     C* LE SEGUENTI SPECIFICHE LE ESEGUO SOLO SE BOLLA NON PRESENTE
062800000622   1AC     *IN06         IFEQ      *OFF
062900911003     C*
063000931125     C* D E T T A G L I O   C O L L I
063100950413     C*
063200990906     C                   EXSR      RIEBLT
063300931125     C*
063400080407     C* Leggo il numero dei segnacolli dal dettaglio bolle partenza
063500080407     C     KBLP2         SETLL     FNBLT01l                               32
063600921123     C*
063700921123     C* 32 OFF-  NON TROVATO IL DETTAGLIO COLLI: STAMPO ERRORE
063800950118    2C     *IN32         IFEQ      *OFF
063900080411     c     §3arbl        andne     'R'
064000090109     c     §3aurc        andne     'S'
064100921123     C                   SETON                                        07
064200921123     C                   EXSR      STAERR
064300950118   X2C                   ELSE
064400921123     C*
064500080407     C     KBLP2         READE     FNBLT01l                               32
064600990906     C*
064700950118    3C     *IN32         DOWEQ     *OFF
064800100414     C                   Z-ADD     BLTfls        ARTfls
064900100414     C                   Z-ADD     BLTNSC        ARTNSC
065000080407     c                   exsr      PesoeVol
065100931125     C*
065200920917     C* CONTROLLO SE SEGNACOLLO GIA' ESISTENTE
065300950905     C                   SETOFF                                       0408
065400950413     C     KSPP          CHAIN     FNART27L                           31
065500080407    4C     *IN31         IFEQ      *OFF
065600950413     C                   SETON                                        04
065700080407    5C     ARTDCM        IFNE      *ZEROS
065800950905     C                   SETON                                        08
065900950413     C                   EXSR      RIEBLT
066000100414     C                   Z-ADD     BLTfls        ARTfls
066100950418     C                   Z-ADD     BLTNSC        ARTNSC
066200080407     c                   exsr      PesoeVol
066300950413     C* Segnacollo già esistente: se già consegnato aggiorno il record
066400950413     C                   UPDATE    FNART000
066500040317     c   03              exsr      ctrdet
066600040317     c
066700080407   x5C                   ELSE
066800040317     c
066900950418     C                   EXSR      RIEBLT
067000100414     C                   Z-ADD     BLTfls        ARTfls
067100950418     C                   Z-ADD     BLTNSC        ARTNSC
067200950413     C                   UNLOCK    FNART27L
067300080407    5C                   END
067400950413     C* Stampo errore
067500950413     C                   EXSR      STAERR
067600080407   x4C                   ELSE
067700980311     C*
067800951011     C                   WRITE     FNART000
067900031202     c   03              exsr      ctrdet
068000080407    4C                   END
068100911014     C*
068200080407     C     KBLP2         READE     FNBLT01l                               32
068300950118    3C                   ENDDO
068400950118    2C                   ENDIF
068500911003     C*
068600120629     c* FIART unico  --> imposto flag gestibile in arrivo
068700120629     C     KBLP2         setll     FIART01l
068800120629     C     KBLP2         reade     FIART01l
068900120629     c                   dow       not %eof(fiart01l)
069000120702     c                   eval      arwbga='S'
069100120629     c                   update    fiart000
069200120629     c
069300120629     C     KBLP2         reade     FIART01l
069400120629     c                   enddo
069500120629     c
069600000622   1AC                   ENDIF
069700911003     C* SCRIVO FILE
069800000622     C  N06              WRITE     FNARB000
069900000622     C   06              UPDATE    FNARB000
070000080407     C*
070100911003     C* DELETO
070200000622     C     *IN06         IFEQ      *OFF
070300950118     C                   DELETE    FNBLPE00
070400000622     C                   ELSE
070500000622     C                   EXSR      DLTRIC
070600000622     C                   ENDIF
070700960424     C**
070800960424     C* CANCELLO EVENTUALE ANOMALIA 12
070900960424     C                   Z-ADD     12            COMCAA
071000990510     C     KANM1         CHAIN     FNANM01L                           33
071100960424     C* CHIUDO ANOMALIA
071200960424    1C     *IN33         IFEQ      *OFF
071300020809     C                   CLEAR                   FNLR33DS
071400960424     C                   Z-ADD     ANMNR1        D33NRR
071500960424     C                   MOVE      DATEU         D33DCH
071600960424     C                   MOVEL     §7NFS1        D33FSC
071700960424     C                   CALL      'FNLR33R'
071800020809     C                   PARM                    FNLR33DS
071900960424     C                   ENDIF
072000921022     C*
072100000622     C     *IN06         IFEQ      *OFF
072200000111     C     BLPLNA        CHAIN     AZORG01L                           32
072300020218     C  N32              MOVEL     ORGDE3        OG143
072400020218     C   32              CLEAR                   OG143
072500020226     c                   eval      lnantw = §ogntw
072600921022     C* CONTROLLO SE C'ERANO ANOMALIE MANCA BOLLA
072700960827     C*           E ANOMALIE 050-COLLO ARR. NON PART. SU BOLLE LOCALI
072800950123     C     KBLP2         SETLL     FNART001
072900020226     C     lnaNTW        IFNE      'EEX'
073000020226     C     lnaNTW        ANDNE     'EUP'
073100020226     C     lnaNTW        ANDNE     'FED'
073200060322     C     lnaNTW        ANDNE     'DPD'
073300961121     C     KBLP2         READE(N)  FNART001                               32
073400961121     C                   ELSE
073500961121     C     KBLP2         READE     FNART001                               32
073600961121     C                   ENDIF
073700921022     C*
073800961121     C     *IN32         DOWEQ     *OFF
073900950128     C                   EXSR      CTRANM
074000960827     C* . . . . . BOLLA LOCALE , SERIE>0 , FLS *NE LNP
074100960827     C     *IN03         IFEQ      *ON
074200960827     C     ARTNRS        ANDGT     *ZEROS
074300960827     C     ARTFLS        ANDNE     ARTLNP
074400960827     C                   EXSR      ANM050
074500960827     C                   END
074600961121     C**
074700000111     C* SE LNA ESTERA POSSO AGGIORNARE IL RECORD
074800020226     C     lnaNTW        IFNE      'EEX'
074900020226     C     lnaNTW        ANDNE     'EUP'
075000020226     C     lnaNTW        ANDNE     'FED'
075100060322     C     lnaNTW        ANDNE     'DPD'
075200961121     C     KBLP2         READE(N)  FNART001                               32
075300961121     C                   ELSE
075400961121     C     KBLP2         READE     FNART001                               32
075500921022     C                   END
075600961121     C**
075700961121     C                   ENDDO
075800000622     C                   ENDIF
075900911002     C*
076000911002     C                   ENDSR
076100080407     C**---------------------------------------------------------
076200080407     c* imposto peso e volume della partenza sul dettaglio colli
076300080407     C**---------------------------------------------------------
076400080407     c     PesoeVol      BEGSR
076500080407     c                   z-add     bltvuc        artvuc
076600080407     c                   movel     bltfvc        artfvc
076700080407     c                   z-add     bltpuc        artpuc
076800080407     c                   movel     bltfpc        artfpc
076900080407     c                   select
077000080407     c                   when      artvuc>0 and artpuc>0
077100080407     c                   eval      arbfbs='E'
077200080423     c                   when      artvuc>0   and arbfbs=' '
077300080407     c                   eval      arbfbs='V'
077400080423     c                   when      artpuc>0   and arbfbs=' '
077500080407     c                   eval      arbfbs='P'
077600080423     c                   other
077700080423     c                   eval      arbfbs='E'
077800080407     c                   endsl
077900080407     c                   ENDSR
078000911002     C**---------------------------------------------------------
078100911002     C** RIEMPO CAMPI FLARB00F
078200911002     C**---------------------------------------------------------
078300911002     C     RIEARB        BEGSR
078400000622     C* DEVO CLEARARE FORMATO FNARB SOLO SE BOLLA NON PRESENTE
078500000622     C  N06              CLEAR                   FNARB000
078600931125     C*
078700950118     C     BLPCBO        IFNE      SAVCBO
078800920603     C                   MOVEL     '3A'          COD
078900920603     C                   MOVEL     *BLANKS       KEY
079000950118     C                   MOVEL     BLPCBO        KEY
079100920603     C     KTAB          CHAIN     TABEL                              33
079200920603     C  N33              MOVEL     TBLUNI        DS3A
079300920603     C   33              MOVEL     'F1'          §3ATB1
079400920625     C   33              MOVEL     '  '          §3ATB2
079500950118     C                   MOVEL     BLPCBO        SAVCBO            2
079600950118     C                   ENDIF
079700920603     C*
079800911002     C                   MOVEL     BLPLNP        ARBLNP
079900950118     C                   MOVE      BLPAAS        ARBAAS
080000911002     C                   MOVEL     BLPMGS        ARBMGS
080100911002     C                   MOVEL     BLPNRS        ARBNRS
080200911002     C                   MOVEL     BLPNSP        ARBNSP
080300010110     C* L'IMPOSTAZIONE DEL CODICE BOLLA SE STO RICEVENDO UN AGGIORNAMEN
080400010110     C* TO DI BOLLA AUTOGENERATA LA FACCIO QUANDO GESTISCO IL CONTRAS-
080500010110     C* SEGNO
080600010110     C  N06              MOVEL     BLPCBO        ARBCBO
080700950118     C                   MOVEL     BLPFDN        ARBFDN
080800911002     C                   MOVEL     BLPFNS        ARBFNS
080900911002     C                   MOVEL     BLPLNA        ARBLNA
081000911002     C                   MOVEL     BLPKSC        ARBKSC
081100911002     C                   MOVEL     BLPZNC        ARBZNC
081200911002     C                   MOVEL     BLPRSM        ARBRSM
081300911002     C                   MOVEL     BLPINM        ARBINM
081400911002     C                   MOVEL     BLPLOM        ARBLOM
081500911002     C                   MOVEL     BLPPRM        ARBPRM
081600950118     C                   MOVEL     BLPCAM        ARBCAM
081700950118     C                   MOVEL     BLPNZM        ARBNZM
081800911002     C                   MOVEL     BLPRSD        ARBRSD
081900911003     C                   MOVEL     BLPIND        ARBIND
082000911002     C                   MOVEL     BLPLOD        ARBLOD
082100911002     C                   MOVEL     BLPPRD        ARBPRD
082200950118     C                   MOVEL     BLPCAD        ARBCAD
082300950118     C                   MOVEL     BLPNZD        ARBNZD
082400911002     C                   MOVEL     BLPGC1        ARBGC1
082500911002     C                   MOVEL     BLPGC2        ARBGC2
082600911002     C                   MOVEL     BLPCTR        ARBCTR
082700911002     C                   MOVEL     BLPCTS        ARBCTS
082800911002     C                   MOVEL     BLPFTM        ARBFTM
082900950118     C                   MOVEL     BLPFIN        ARBFIN
083000911002     C                   MOVEL     BLPFAP        ARBFAP
083100911002     C                   MOVEL     BLPTSP        ARBTSP
083200950118     C                   Z-ADD     BLPIAS        ARBIAS
083300950123     C                   MOVEL     BLPVAS        ARBVAS
083400911002     C                   MOVEL     BLPNAS        ARBNAS
083500911002     C                   MOVEL     BLPNCL        ARBNCL
083600950118     C                   Z-ADD     BLPPKB        ARBPKB
083700950118     C                   Z-ADD     BLPPKF        ARBPKF
083800000622     C* SE BOLLA GIA' PRESENTE SOSTITUISCO SOLO SE IL VOLUME SU ARB
083900000622     C* NON E' NE' "T" NE' "Z" E SE IL VOLUME RICEVUTO E' > DI QUELLO
084000000622     C* SU ARB
084100000622     C     *IN06         IFEQ      *OFF
084200000622     C     ARBFVF        ORNE      'T'
084300000622     C     ARBFVF        ANDNE     'Z'
084400000622     C     BLPVLF        ANDGT     ARBVLF
084500950118     C                   Z-ADD     BLPVLF        ARBVLF
084600950118     C                   MOVEL     BLPFVF        ARBFVF
084700000622     C                   ENDIF
084800950118     C                   Z-ADD     BLPVLB        ARBVLB
084900950118     C                   MOVEL     BLPFVB        ARBFVB
085000950118     C                   MOVEL     BLPQFT        ARBQFT
085100950118     C                   MOVEL     BLPVMD        ARBVMD
085200950118     C                   MOVEL     BLPVAD        ARBVAD
085300070222     C*
085400911002     C                   MOVEL     BLPCCM        ARBCCM
085500950118     C                   Z-ADD     BLPRMN        ARBRMN
085600950118     C                   MOVEL     BLPRMA        ARBRMA
085700911002     C                   MOVEL     BLPRMO        ARBRMO
085800911002     C                   MOVEL     BLPFLS        ARBFLS
085900911002     C                   MOVEL     BLPNCD        ARBNCD
086000911002     C                   MOVEL     BLPNCA        ARBNCA
086100911002     C                   MOVEL     BLPXCO        ARBXCO
086200950118     C*
086300911002     C                   MOVEL     BLPFFD        ARBFFD
086400950118     C                   MOVE      BLPDCR        ARBDCR
086500911002     C                   MOVEL     BLPHCR        ARBHCR
086600911002     C                   MOVEL     BLPTCR        ARBTCR
086700950118     C                   MOVE      BLPDBR        ARBDBR
086800931215     C                   MOVEL     BLPNFV        ARBNFV
086900950124     C*
087000911002     C                   MOVEL     BLPFLE        ARBFLE
087100911002     C                   MOVEL     BLPTFA        ARBTFA
087200911002     C                   MOVEL     BLPTFP        ARBTFP
087300911002     C                   MOVEL     BLPFEA        ARBFEA
087400911008     C                   MOVEL     BLPFBR        ARBFBR
087500950118     C                   MOVEL     BLPTC1        ARBTC1
087600950118     C                   MOVEL     BLPTC2        ARBTC2
087700950118     C                   MOVEL     BLPGVA        ARBGVA
087800950118     C                   MOVEL     BLPGMA        ARBGMA
087900950118     C                   MOVEL     BLPGGA        ARBGGA
088000001204     C*  SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA N FT1
088100051116     C* BLANK NON LO DEVO AGGIORNARE ALTRIMENTI NON VENGONO TRASMESSE
088200001204     C* LE DATE DI CONSEGNA
088300001204     C     *IN06         IFEQ      *OFF
088400001204     C     ARBFT1        ORNE      *BLANKS
088500931125     C                   MOVEL     'R'           ARBFT1
088600950118     C                   Z-ADD     DATEU         ARBDT1
088700001204     C                   ENDIF
088800970212     C*
088900060220     c* Imposto nel tipo record "A" (se non c'e' lo creo,)
089000060220     C*   IL 1° BYTE DEL CAMPO AR4NOT  E §B4ASO=SIGLA OPERATORE
089100060302     c                   clear                   contaerror
089200060220     c                   eval      wtrc='A'
089300060302     c
089400060302     c* Se errore in write (succede per conflitto con fnlrc1r)
089500060302     c*  ritento l'update
089600060302    1c                   dou       not *in99
089700060302     c                   setoff                                       99
089800060220     c     kar4          chain     fiar401l
089900060302     c                   exsr      Scriviar4A
090000060302     c
090100060302     c* dopo 10 tentativi mando un messaggio in posta ed esco senza
090200060302     c*  impostare il record A
090300060302     c                   add       1             contaerror        2 0
090400060302    2c                   if        contaerror>10
090500060302     C
090600111104     c* Mando e_mail
090700111104     c                   clear                   msgtes
090800111104     c                   clear                   msgdet
090900111104     c                   eval      msgtes='RICEZIONE BOLLE: FNLR14R'
091000111104     c                   eval      msgdet='Non scritto record"A"   -  utente'+
091100111104     c                               KNMUS + '   bolla :' +
091200111104     c                               (%editc(arbaas:'Z'))
091300060302     c                             + (%editc(arblnp:'Z')) + (%editc(arbnrs:'2'))
091400060302     c                             + (%editc(arbnsp:'2'))
091500120305     C                   eval      wrkEml= 'ced@brt.it'
091600111104     C                   call(e)   'TIS701C'
091700111104     C                   parm                    wrkEml          253
091800111104     C                   PARM                    MSGTES           44
091900111104     C                   PARM                    MSGDET         5000
092000060302     C*
092100060302     c                   leave
092200060302    2c                   endif
092300060302    1c                   enddo
092400060302     c
092500060220     c
092600060220     c* Cerco partita iva da impostare su fnarb
092700060220     c                   eval      wtrc='P'
092800060220     c     kar4          chain(N)  fiar401l
092900060220     c                   if        %found(fiar401l)
093000060220     C                   MOVEL     §3ATB1        UNOTB1            1
093100060220     C     UNOTB1        IFEQ      'F'
093200060220     C     §3ATB2        ANDEQ     *BLANKS
093300060220     C                   MOVEL     AR4NOT        ARBCPI
093400060220     C                   ELSE
093500060220     C                   MOVE      AR4NOT        ARBCPI
093600060220     C                   ENDif
093700061031
093800061031     c                   else
093900061031      * se non c'è il fiar4 rcd P aggancio la bolla partenza se bolla unica
094000061031     c                   if        §3atb2 = *blanks
094100061031     c     kblp2         chain     fnblp01l
094200061031     c                   if        %found(fnblp01l)
094300061031     c                   eval      arbcpi = w_blpcpi
094400061031     c                   endif
094500061031     c                   endif
094600061031
094700060220     C                   ENDif
094800030109      *
094900930201     C* A B I L I T A Z I O N E   M E R C E
095000931125     C**
095100000622     C*
095200930201     C* SE FOGLIO VIAGGIO 99999 ABILITO SUBITO
095300000623     C  N06BLPNFV        IFEQ      99999
095400930201     C                   MOVEL     3             ARBAMA
095500950919     C                   ENDIF
095600931125     C**
095700931125     C* A B I L I T A Z I O N E   C / A S S E G N O
095800000623     C*
095900911002     C* PER BOLLE SENZA CONTRASSEGNO
096000051116     c* Vedo se essite C/A per abilitazione
096100051116     c     kblp2         chain     fiar901l
096200051116     c                   if        not %found(fiar901l) or ar9cas=0
096300911002     C                   MOVEL     'S'           ARBACC
096400950119   X1C                   ELSE
096500011211
096600011210      * ------------------
096700011210
096800011210      * NUOVO CONTROLLO C/ASSEGNO
096900011210     c                   clear                   trul21ds
097000011210     c                   eval      i21tla = 'L'
097100011210     c                   eval      i21cbo = blpcbo
097200011210     c                   eval      i21tsp = blptsp
097300011210     c                   eval      i21lnp = blplnp
097400011210     c                   eval      i21nzm = blpnzm
097500011210     c                   eval      i21lna = blplna
097600011210     c                   eval      i21nzd = blpnzd
097700051116     c                   eval      i21tic = ar9tic
097800051116     c                   eval      i21imp = ar9cas
097900051116     c                   eval      i21div = ar9vca
098000011210     c                   eval      i21ute = knmus
098100011210     c                   eval      i21pgm = nompgm
098200011210     c                   call      'TRUL21R'
098300011210     c                   parm                    trul21ds
098400011210
098500011210     c                   if        o21fca = *blanks
098600011210     c                   eval      arbacc = 'S'
098700011210     c                   endif
098800911002     C*
098900950119    1C                   END
099000931125     C**
099100950119     C* A B I L I T A Z I O N E    A S S E G N A T O
099200931125     C**
099300000623   1AC     *IN06         IFEQ      *OFF
099400000623     C*
099500911003     C                   MOVEL     §3ATB1        UNOTB1            1
099600911003     C* FRANCO
099700950119    1C     UNOTB1        IFEQ      'F'
099800930112     C* SE NON C'E' SECONDA BOLLA ABILITO
099900950119    2C     §3ATB2        IFEQ      '  '
100000911003     C                   MOVEL     'S'           ARBACA
100100950119    2C                   END
100200950119    1C                   END
100300050314     c
100400050314     c* Se già tassata, abilito subito l'assegnato
100500050314     c                   if        arbaca=' '
100600050314     c                   if        §3atb2<>*blanks
100700050314     c                   eval      ktrc='2'
100800050314     c                   else
100900050314     c                   eval      ktrc='1'
101000050314     c                   endif
101100050314     c
101200050314     c     kar6          chain     fiar601l
101300050314     c                   if        %found(fiar601l)
101400050314     c                   move      ar6ksc        fineksc
101500050314     c                   if        fineksc<>'9999' and fineksc<>'0000'
101600050314     c                   eval      arbaca='S'
101700050314     c* mi salvo i campi da inmpostare in arbksc e arbctr
101800050314     c                   eval      arbksc=ar6ksc
101900050314     c                   eval      arbctr=ar6ctr
102000050331     c* Se si tratta di bolla di recupero, devo anche chiudere la spedizione
102100050331    3C     §3ATB1        IFEQ      'A3'
102200050331     C* IMPOSTO I CAMPI DELLA CHIUSURA
102300050331     C                   MOVEL     'Y'           ARBICC
102400050331     C                   MOVEL     'S'           ARBICA
102500050331     C                   Z-ADD     DATEU         ARBDCM
102600050331     C                   time                    wora              6 0
102700050331     C                   movel     wora          ARBHMC
102800050331     C                   CLEAR                   ARBFT1
102900050331     C                   CLEAR                   ARBDT1
103000050331     C                   MOVEL     *ALL'9'       ARBNDC
103100050331     C                   Z-ADD     DATEU         ARBDDC
103200050331     C                   MOVEL     'S'           ARBFDC
103300050331     C                   MOVEL     *ALL'9'       ARBNMI
103400050331     C                   Z-ADD     DATEU         ARBDMI
103500050331     C                   MOVEL     *ALL'9'       ARBCUC
103600050331     C                   CLEAR                   ARBFBC
103700050331    3C                   ENDIF
103800050314     c                   endif
103900050314     c                   endif
104000050314     c                   endif
104100050314     c
104200000623   1AC                   ENDIF
104300070322
104400070322      * se LNA o sua capofila abilitata alle telefonate per GEO
104500070329      *  e NO bolle di recupero
104600070329     c                   if        lnageo = 'S' and §3arbl <> 'R'
104700070322     c                   clear                   dsbl4l
104800070322      * Genero record blocco spedizione se:
104900070322      * --> Consegna particolare Appuntamento o Supermercato (non tassative)
105000070322     c                   if        arbtc1 = 'A' or arbtc1 = 'S' or arbtc2 = 'A'
105100070322     c                             or arbtc2 = 'S'
105200070322     c                   eval      §b4lbas = 'S'
105300070322      * se consegna tassativa sblocco
105400070322     c                   if        arbdcr <> *zeros and arbtcr = *blanks
105500070322     c                   clear                   §b4lbas
105600070322     c                   endif
105700070322     c                   endif
105800070322      * --> Contrassegno non abilitato
105900070322     c                   if        arbacc = *blanks
106000070322     c                   eval      §b4ltca = 'S'
106100070322     c                   endif
106200070322      * --> Fermo deposito
106300070322     c                   if        arbffd <> *blanks
106400070322     c                   eval      §b4ltfd = 'S'
106500070322     c                   endif
106600070322      * scrivo record L di fiar4 per la gestione dei blocchi
106700070322     c                   if        dsbl4l <> *blanks
106800070322     c                   clear                   fiar4000
106900070322     c                   eval      ar4aas = blpaas
107000070322     c                   eval      ar4lnp = blplnp
107100070322     c                   eval      ar4nrs = blpnrs
107200070322     c                   eval      ar4nsp = blpnsp
107300070322     c                   eval      ar4trc = 'L'
107400070322     c                   eval      ar4not = dsbl4l
107500070322     c                   eval      ar4ftr = 'R'
107600070322     c                   eval      ar4dtr = dateu
107700070322     c                   eval      ar4duv = dateu
107800070322     c                   write     fiar4000                             99
107900070322     c                   endif
108000070322     c                   endif
108100950118     C*
108200911002     C                   ENDSR
108300070322      *-----------------------------------------------------------------------
108400060302     c     Scriviar4A    BEGSR
108500060302     c                   if        %found(fiar401l)
108600060302     c                   movel     ar4not        dsbl4a
108700060302     c                   else
108800060302     C                   MOVE      BLPAAS        AR4AAS
108900060302     C                   MOVEL     BLPLNP        AR4LNP
109000060302     C                   MOVEL     BLPNRS        AR4NRS
109100060302     C                   MOVEL     BLPNSP        AR4NSP
109200060302     C                   MOVEL     'A'           AR4TRC
109300060302     C                   CLEAR                   AR4NOT
109400060302     C                   CLEAR                   DSBL4A
109500060302     c                   movel     'R'           ar4ftr
109600060302     c                   movel     dateu         ar4DTR
109700060302     c                   endif
109800060302     c
109900060302     c                   movel     dateu         ar4duv
110000120604     c* ES --> dal momento che non stampiamo più i DDT ma escono con le distinte,
110100120604     c*        non serve più impostare il falg stampa DDT ma lo tengo come inviato
110200120604     c*        dalla partenza
110300060302     C                   MOVEL     BL2SV1        §B4ABM
110400060302
110500060302      * se linea di arrivo con picking attivo o lna per cui stampare i
110600060302      * DDT SI in arrivo
110700120604if  1c**                 If        lnapks = 'S'
110800120604 s  2c**                 Select
110900060302
111000060302      * per bolla di recupero
111100060302      * imposto che la bolla è già stata stampata
111200120604     c**                 When         §3arbl = 'R'
111300120604     c**                 If        Bl2Sv1 = 'S'
111400120604     c**                 Eval      §B4Abm = 'P'
111500120604     c**                 EndIf
111600120604     c**                 If        Bl2Sv1 = 'C'
111700120604     c**                 Eval      §B4Abm = 'Q'
111800120604     c**                 EndIf
111900120604     c**                 If        Bl2Sv1 = 'N' or Bl2Sv1 = 'Y'
112000120604     c**                 Eval      §B4Abm = 'J'
112100120604     c**                 Endif
112200120604     c**                 If        Bl2Sv1 = 'K'
112300120604     c**                 Eval      §B4Abm = 'W'
112400120604     c*+                 Endif
112500060302
112600060302      * se bl2sv1 = 'Y'
112700060302      * imposto 'N' x bolla ancora da stampare
112800120604     c**                 When      Bl2Sv1 = 'Y'
112900120604     c**                 Eval      §B4Abm = 'N'
113000060302
113100120604    2c**                 Endsl
113200120604    1c**                 Endif
113300060302
113400120604     C**   §7AFSR        IFEQ      'Y'
113500120604     C**   BL2SV1        ANDEQ     'Y'
113600120604     C**   lnpNTW        ANDNE     'DPD'
113700120604     C**                 MOVEL     'N'           §B4ABM
113800120604     C**                 ENDIF
113900120604     c*
114000060302     c
114100060302     c* Sigla operatore video
114200060302     C                   MOVE      BL2CEI        §B4ASO
114300060302     C                   MOVEL     DSBL4A        AR4NOT
114400060302     c                   if        %found(fiar401l)
114500060302     c                   update    fiar4000
114600060302     c                   else
114700060302     C                   WRITE     FiAR4000                             99
114800060302     c                   endif
114900060302     c                   endsr
115000911002     C**---------------------------------------------------------
115100911002     C** RIEMPO DETTAGLIO SEGNACOLLI
115200911002     C**---------------------------------------------------------
115300911002     C     RIEBLT        BEGSR
115400950119     C                   CLEAR                   FNART000
115500950118     C                   Z-ADD     ARBAAS        ARTAAS
115600931125     C                   Z-ADD     BLPLNP        ARTLNP
115700911002     C                   Z-ADD     BLPNRS        ARTNRS
115800911002     C                   Z-ADD     BLPNSP        ARTNSP
115900911002     C                   MOVEL     BLPLNA        ARTLNA
116000911002     C                   MOVEL     BLPFLS        ARTFLS
116100950119     C                   MOVEL     'R'           ARTFTR
116200960712     C* SE COLLO LOCALE INDICATO CHE E' ABBINATO
116300960712     C     *IN03         IFEQ      *ON
116400960712     C                   MOVEL     'S'           ARTABN
116500960712     C                   ENDIF
116600950119     C                   ENDSR
116700911014     C**---------------------------------------------------------
116800960424     C** CONTROLLO SE ESISTE ANOMALIA 10-NO REC BOLLA E 12-
116900911014     C**---------------------------------------------------------
117000950128     C     CTRANM        BEGSR
117100960529     C                   Z-ADD     0             OFV               5 0
117200970521     C*
117300970521     C* CHIUDO ANOMALIA 410 SE ESISTE
117400970521     C                   Z-ADD     410           COMCAA
117500990510     C     KANM          CHAIN     FNANM000                           33
117600970521    1C     *IN33         IFEQ      *OFF
117700020809     C                   CLEAR                   FNLR33DS
117800970521     C                   Z-ADD     ANMNR2        D33NRR
117900970521     C                   MOVE      DATEU         D33DCH
118000970521     C                   MOVEL     §7NFS1        D33FSC
118100970521     C                   CALL      'FNLR33R'
118200020809     C                   PARM                    FNLR33DS
118300970521    1C                   END
118400950128     C*
118500960424     C                   Z-ADD     10            COMCAA
118600990510     C     KANM          CHAIN     FNANM000                           33
118700950128     C* CHIUDO ANOMALIA
118800950128    1C     *IN33         IFEQ      *OFF
118900000623     C*
119000020809     C                   CLEAR                   FNLR33DS
119100960415     C                   Z-ADD     ANMNR2        D33NRR
119200960415     C                   MOVE      DATEU         D33DCH
119300960415     C                   MOVEL     §7NFS1        D33FSC
119400960415     C                   CALL      'FNLR33R'
119500020809     C                   PARM                    FNLR33DS
119600960415     C*
119700950128     C* ESEGUO LA CALL AL PGM DI CARICAMENTO SPUNTE PER AGGIORNARE
119800951213     C*  LA BOLLA. NON LO FACCIO SE E' LOCALE
119900961121    2C     *IN98         IFEQ      *OFF
120000951213     C     *IN03         ANDEQ     *OFF
120100021015     c                   Eval      Pa5Mbr = MbrFil
120200021015     c                   Movel     Pa5Mbr4       PA5FIL
120300950128     C                   MOVEL     'F'           PA5FIL
120400950128     C                   MOVE      PARAM5        KPJBU
120500950203     C                   MOVEL     'LR22'        KCOAZ
120600950203     C                   CALL      'BCH10'
120700950203     C                   PARM                    KPJBA
120800950203     C                   SETON                                        98
120900961121    2C                   ENDIF
121000070831     c
121100070831     c* Se esiste anomalia di collo arrivato non partito
121200070831     c*  chiano in update il file fnart e aggiorno i dati della partenza
121300070831     C                   Z-ADD     050           COMCAA
121400070831     C     KANM          CHAIN     FNANM000                           33
121500070831    2C     *IN33         IFEQ      *OFF
121600070831     c     kart          chain     fnart01l
121700070831    3c                   if        %found(fnart01l)
121800070831     c* Se manca data partenza la aggiorno, altrimenti data uscita transito
121900070831    4c                   if        anmcdu=arbtfp
122000070831     C                   Z-ADD     anmdao        ARTDFV
122100070831     C                   Z-ADD     anmnfv        ARTnFV
122200070831     C                   Z-ADD     88            ARTNPP
122300070831    5c                   if        arbtfp=arbtfa
122400070831     C                   Z-ADD     90            ARTNPP
122500070831    5c                   endif
122600070831     C                   movel     'S'           ARTABN
122700070831   x4c                   else
122800070831     C                   Z-ADD     anmdao        ARTDUT
122900070831     C                   Z-ADD     89            ARTPUT
123000070831     C                   Z-ADD     anmcdu        ARTFLP
123100070831     C                   movel     'S'           ARTABN
123200070831    4c                   endif
123300070831     c
123400070831    4c                   if        lnantw<>'EEX' and lnantw<>'EUP' and
123500070831     c                             lnantw<>'FED' and lnantw<>'DPD'
123600070903     c                   update    fnart001
123700070831    4c                   endif
123800070831    3c                   endif
123900070831    2c                   endif
124000961122     C**
124100000111     C** SE LNA ESTERA CONTROLLO SE ESISTE EVENTO COLLO 2 /6A/6
124200020226    2C     lnaNTW        IFEQ      'EEX'
124300020226    2C     lnaNTW        OREQ      'EUP'
124400020226    2C     lnaNTW        OREQ      'FED'
124500060322    2C     lnaNTW        OREQ      'DPD'
124600961122     C                   CLEAR                   WDEV
124700961122     C                   CLEAR                   WCEV
124800961122     C* APRO IL FILE
124900961122    3C     WAPER         IFEQ      ' '
125000961122     C                   OPEN      FNEVS02L
125100961122     C                   MOVEL     'S'           WAPER             1
125200961122    3C                   ENDIF
125300961122     C**
125400961122     C                   MOVEL     'E2 '         KCEV
125500020809    3C                   DO        3             XX
125600961122     C     KEVS          SETLL     FNEVS02L
125700961122     C     KEVS          READE     FNEVS02L                               33
125800961122    4C     *IN33         DOWEQ     *OFF
125900970602    5C     EVSDEV        IFGT      WDEV
126000961122     C     WDEV          OREQ      0
126100961122     C                   MOVEL     EVSDEV        WDEV
126200961122     C                   MOVEL     EVSCEV        WCEV
126300961122    5C                   ENDIF
126400961122     C**
126500961122     C* AGGIORNO L'EVENTO COL NUMERO DI SPEDIZIONE
126600961122     C                   Z-ADD     BLPAAS        EVSAAS
126700961122     C                   Z-ADD     BLPLNP        EVSLNP
126800961122     C                   Z-ADD     BLPNSP        EVSNSP
126900961122     C                   CLEAR                   EVSFTR
127000961122     C                   CLEAR                   EVSDTR
127100961122     C                   UPDATE    FNEVS000
127200961122     C*
127300961122     C     KEVS          READE     FNEVS02L                               33
127400961122    4C                   ENDDO
127500961122     C**
127600961122     C* AL 2 GIRO CERCO LE DISCOR.6  --> COLLO ARRIVATO MA A TERRA
127700020809    4C     XX            IFEQ      1
127800961122     C                   MOVEL     'E6 '         KCEV
127900961122    4C                   ENDIF
128000970602     C* AL 3 GIRO CERCO LE DISCOR.6B --> COLLO ARRIVATO MA IN PARTENZA
128100020809    4C     XX            IFEQ      2
128200970602     C                   MOVEL     'E6B'         KCEV
128300961122    4C                   ENDIF
128400961122    3C                   ENDDO
128500961122     C* SE TROVATA AGGIORNO IL DETTAGLIO COLLI
128600961122    3C     WDEV          IFGT      0
128700961122     C* SE ERA COLLO RIAMSTO A TERRA ARTDTR DEVE ESSERE = 0
128800970602    4C     WCEV          IFEQ      'E6 '
128900961122     C                   CLEAR                   ARTDTR
129000961122     C                   ELSE
129100961122     C* ALTRIMENTI IMPOSTO LA DATA DELL'EVENTO
129200961122     C                   Z-ADD     WDEV          ARTDTR
129300961122    4C                   ENDIF
129400961122     C* AGGIORNO RECORD
129500961122     C                   UPDATE    FNART001
129600961122     C* DISALLOCO RECORD
129700961122   X3C                   ELSE
129800070903     C                   update    FNART001
129900961122    3C                   ENDIF
130000961122     C*
130100961122    2C                   ENDIF
130200960529     C*
130300960529     C* LEGGO LA SPUNTA PARTENZA DEL COLLO
130400040317     C*  PER REPERIRE LA DATA DEL FOGLIO VIAGGIO PARTENZA, AI FINI DI
130500040317     C*  DETERMINARE IL TERMINAL DI ARRIVO DELLA LINEA DI ARRIVO
130600040317     c*  In realtà prendiamo la data della prima spunta partenza che
130700040317     c*  troviamo del collo, che potrebbe essere anche quella di un
130800040317     c*  p.o. di transito o di disguido. Ma sono casi molto rari ed
130900040317     c*  un problema si potrebbe presentare soltanto in caso di
131000040317     c*  cambio del terminal di arrivo della lna tra il momento in
131100040317     c*  cui parte/arriva il collo, ed il momento in cui
131200040317     c*  parte/arrva la bolla -vsito che siamo nel caso di manca rec-
131300040317     c*  con il terminal di arrivo calcolato richiama LS44 solo x le
131400040317     c*  spunte dell'area di arrivo, oper aggiornare la bolla in arr
131500040317     c*  Ma se cambia terminal in base alla data spunta le aree di arr
131600040317     c*  possono essere 2. allora si dovrebbe o duplicare dei controlli
131700040317     c*  che il pgm ls44 già fa, o richimare ls44 in modo che aggiorni
131800040317     c*  solo fnarb Ma data la eccezionalità dei casi, non facciamo
131900040317     c*  nulla e teniamo il pgm come è
132000040317     c*
132100960603     C                   EXSR      DFVPAR
132200961121     C*
132300960617     C* PRENDO IL TERMINAL DI ARRIVO DELLA LNA
132400020809     C                   CLEAR                   FNLV55DS
132500971114     C                   MOVEL     'A'           D55TPT
132600971114     C                   MOVEL     ARTLNP        D55LNP
132700971114     C                   MOVEL     ARTLNA        D55LIN
132800971114     C                   MOVEL     WDFV          D55DRF
132900971114     C                   CALL      'FNLV55R'
133000020809     C                   PARM                    FNLV55DS
133100971114     C                   MOVEL     'S'           WCAL55            1
133200971114     C**
133300971114     C     D55ERR        IFNE      ' '
133400971114     C                   MOVEL     D55LIN        D55TFA
133500971114     C                   ENDIF
133600971114     C                   MOVEL     D55TFA        LNATFA
133700950130     C*
133800950130     C* CERCO UNA SPUNTA DEL DETTAGLIO COLLO
133900070123     C     KSPP          CHAIN     FNBRV07L                           33
134000960529     C*
134100961121    2C     *IN33         DOWEQ     *OFF
134200960705     C*
134300960705     C* SE CATEGORIA 1 ESCLUDO SE IL FOGLIO E' UN NORMALE: ELABORO
134400960705     C*  SOLO SE E' DEFLUENZA
134500961121    3C     BRVNPG        IFEQ      1
134600960705     C                   CLEAR                   KEY
134700960705     C                   CLEAR                   WELAB
134800960705     C*
134900000110     C                   Z-ADD     BRVNFV        KNFV
135000040317     C                   Z-ADD     BRVfgs        Kfgs
135100020809     C     K02FGV01      chain     FNFGV01L                           36
135200031203     c     *in36         ifeq      *off
135300031203     C                   MOVEL     FGVTTR        WTTR
135400031203   X4C                   ELSE
135500960705     C     KFVA          CHAIN     FNFVA01L                           36
135600980623     C                   MOVEL     FVATTR        WTTR
135700031203    4C                   ENDIF
135800960705     C*
135900980623    4C     WTTR          IFNE      *BLANKS
136000980623     C     WTTR          LOOKUP    TDF                                    36
136100980623     C  N36              MOVEL     'N'           WELAB             1
136200961121    4C                   ENDIF
136300960705     C*
136400961121    3C                   ENDIF
136500960705     C*
136600960705     C** ELABORO LA CATEGORIA 1 SOLO PER LA DEFLUENZA E LE ALTRE CATEG
136700961121    3C     BRVNPG        IFNE      1
136800960705     C     WELAB         OREQ      ' '
136900960617     C**
137000960603     C* LEGGERE LE ECCEZIONI TERMINAL
137100960603     C**
137200020809     C                   CLEAR                   FNLV55DS
137300971114     C                   MOVEL     'A'           D55TPT
137400971114     C                   MOVEL     ARTLNP        D55LNP
137500031203     C                   MOVEL     BRVPES        D55LIN
137600971114     C                   MOVEL     WDFV          D55DRF
137700971114     C                   CALL      'FNLV55R'
137800020809     C                   PARM                    FNLV55DS
137900971114     C                   MOVEL     'S'           WCAL55            1
138000971114     C**
138100971114     C     D55ERR        IFNE      ' '
138200971114     C                   MOVEL     D55LIN        D55TFA
138300971114     C                   ENDIF
138400031203     C                   MOVEL     D55TFA        PESTFA
138500960529     C*
138600031203    5C     LNATFA        IFEQ      PESTFA
138700960603     C** IMPOSTO L'AS GESTIONE DELLA ANOMALIA
138800970114     C                   CLEAR                   OFV
138900960529     C                   EXSR      CHIAMO
139000970115     C*
139100960529    5C                   ENDIF
139200961121    3C                   ENDIF
139300960529     C**
139400070123     C     KSPP          READE     FNBRV07L                               33
139500961121    2C                   ENDDO
139600961121     C**
139700961121    1C                   ENDIF
139800950130     C*
139900950130     C                   ENDSR
140000960603     C**---------------------------------------------------------
140100960603     C*  LEGGO LA SPUNTA PARTENZA DEL COLLO PER PRENDERE LA DATA FV
140200960603     C**---------------------------------------------------------
140300960603     C     DFVPAR        BEGSR
140400960603     C                   CLEAR                   WDFV
140500070123     C     KSPP          CHAIN     FNBRV07L                           34
140600960603    1C     *IN34         DOWEQ     *OFF
140700960603    2C     BRVNPG        IFEQ      1
140800960603     C*
140900000110     C                   Z-ADD     BRVNFV        KNFV
141000040317     C                   Z-ADD     BRVfgs        Kfgs
141100020809     C     K02FGV01      chain     FNFGV01L                           35
141200960603     C**
141300960603     C* ESCLUDO I TRAINI DI DEFLUENZA
141400031216    3C     *IN35         IFEQ      *OFF
141500960603     C**
141600031216    5C     FGVTTR        IFNE      ' '
141700031216     C     FGVTTR        LOOKUP    TDF                                    35
141800031216    5C                   ENDIF
141900980623     C**
142000031216    5C     FGVTTR        IFEQ      ' '
142100031216     C     *IN35         OREQ      *OFF
142200980623     C**
142300031216    6C     FGVDFV        IFGT      WDFV
142400031216     C                   MOVEL     FGVDFV        WDFV
142500031216    6C                   ENDIF
142600031216    5C                   ENDIF
142700960603     C**
142800960603   X3C                   ELSE
142900960603     C**
143000960603     C     KFVA          CHAIN     FNFVA01L                           35
143100031216    4c     *in35         ifeq      *off
143200031216    5C     FVATTR        IFNE      ' '
143300031216     C     FVATTR        LOOKUP    TDF                                    35
143400031216    5C                   ENDIF
143500031216     C**
143600031216    5C     FVATTR        IFEQ      ' '
143700031216     C     *IN35         OREQ      *OFF
143800031216    6C     FVADFV        IFGT      WDFV
143900960603     C                   MOVEL     FVADFV        WDFV
144000031216    6C                   ENDIF
144100031216    5c                   endif
144200031216    4c                   endif
144300960603     C*
144400960603    3C                   ENDIF
144500960603    2C                   ENDIF
144600070123     C     KSPP          READE     FNBRV07L                               34
144700960603    1C                   ENDDO
144800960603     C**
144900960603     C* SE NON L'HO TROVATA PRENDO LA DATA DI CREAZIONE DELLA ANOMALIA
145000960603     C     WDFV          IFEQ      0
145100960603     C                   Z-ADD     ANMDAO        WDFV
145200960603     C                   ENDIF
145300960603     C                   ENDSR
145400950130     C**---------------------------------------------------------
145500950130     C** CHAIMO PGM DI AGGIORNAMENTO BOLLE DA SPUNTA
145600950130     C**---------------------------------------------------------
145700950130     C     CHIAMO        BEGSR
145800070228     c                   clear                   fnls45ds
145900070228     c                   movel     'S'           dls45ric
146000070228     c                   move      brvpes        dls45pes
146100070228     c                   move      simfel        dls45tfp
146200070228     c                   call      'FNLS45R'
146300070228     c                   parm                    fnls45ds
146400070228     c                   parm                    dsbrv
146500950130     C*
146600950130     C                   SETON                                        02
146700950130     C                   ENDSR
146800950123     C***--------------------------------------------------------
146900080411     C** invio msg in sede
147000911003     C**---------------------------------------------------------
147100911003     C     STAERR        BEGSR
147200111104     c* Mando e_mail
147300111104     c                   clear                   msgtes
147400111104     c                   clear                   msgdet
147500111104     C                   MOVEL     MSG(1)        msgtes
147600111104     c                   eval      %subst(msgtes:26:3)=%editc(simfel:'X')
147700111104     c                   eval      %subst(msgtes:35:10)=mbrfil
147800080411
147900080411     C                   MOVEL     BLPRSM        STARSM           20
148000080603     c     kblp2         chain     fnblp01l
148100080603     c                   if        not %found(fnblp01l)
148200080603     c                   clear                   w_blpctm
148300080603     c                   endif
148400080411
148500111104     c                   eval      msgdet='Bolla: '+%editc(blpaas:'X') +' '
148600080411     c                             + %editc(blplnp:'X')+' '+%editc(blpnrs:'Z')
148700080411     c                             + ' '+%editc(blpnsp:'Z')+' LNA '
148800111104     c                             + %editc(blplna:'Z')+' Mit:'+ starsm +
148900111104     c                             '       Cod.Trattamento merce:' +W_blpctm
149000080411     c* Bolla già esistente
149100111104     c   06              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(2)
149200111104     c** 06              eval      ym5t03=msg(2)
149300080411     c* non trovato dettaglio colli
149400111104     c   07              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(3)
149500111104     c** 07              eval      ym5t03=msg(3)
149600080411     c* Segnacollo esistente
149700080411     c                   if        *in04
149800111104     c   08              eval      %subst(msg(4):22:7)=%editc(artnsc:'Z')
149900111104     c   08              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(4)
150000111104     c  N08              eval      %subst(msg(5):22:7)=%editc(artnsc:'Z')
150100111104     c  N08              eval      msgdet=%trim(msgdet) + '  !!  ' +msg(5)
150200111104     c** 08              eval      ym5t03=msg(4)
150300111104     c**N08              eval      ym5t03=msg(5)
150400111104     c**                 eval      %subst(msgT03:22:7)=%editc(artnsc:'Z')
150500080411     c                   endif
150600080411     C*
150700120305     C                   eval      wrkEml= 'ced@brt.it'
150800111104     C                   call(e)   'TIS701C'
150900111104     C                   parm                    wrkEml          253
151000111104     C                   PARM                    MSGTES           44
151100111104     C                   PARM                    MSGDET         5000
151200080411     C*
151300000622     C                   EXSR      DLTRIC
151400000622     C                   ENDSR
151500000622     C***--------------------------------------------------------
151600000622     C** DELETO RECORD DA FILE DI RICEZIONE PER BOLLA DOPPIA
151700000622     C**---------------------------------------------------------
151800000622     C     DLTRIC        BEGSR
151900921123     C*
152000921123     C* C A N C E L L O  SOLO SE BOLLA DOPPIA
152100921123     C   06              DO
152200950118     C                   DELETE    FNBLPE00
152300920917     C                   END
152400911003     C*
152500911003     C                   ENDSR
152600040317     C*
152700951011     C* CONTROLLO SE LOCALE SE DEVO AGGIORNARE LA DATA PARTENZA------*
152800031202     C     CTRDET        BEGSR
152900031202     C                   Z-ADD     1             KNPG
153000070123     C     KBRV9         CHAIN     FNBRV09L                           35
153100951011     C* TROVATA SPUNTA LOCALE PARTITA NON SUL FV IN ELABORAZIONE
153200980623     C*  --> AGGIORNO  SE NON SI TRATTA DI FOGLIO DEFLUENZA E SE
153300980623     C*  IL FOGLIO E' CHIUSO
153400031202    1C     *IN35         DOWEQ     *OFF
153500031202   1AC     BRVNFV        IFNE      PARNFV
153600041006     c* ignoro la spunta se fatta su foglio viaggio partenza errato
153700041006     c* (lna del collo non presente sul foglio viaggio)
153800041006     c     brvftr        andne     'L'
153900951011     C*
154000951011     C* DATA PARTENZA MERCE E NUMERO PISTOLA PARTENZA SOLO SE LOCALE
154100031202     C                   Z-ADD     BRVNFV        KNFV
154200040317     C                   Z-ADD     BRVfgs        Kfgs
154300031202     C     K02FGV01      chain     FNFGV01L                           35
154400031202    2C     FGVFCF        IFEQ      'S'
154500980623     C* CONTROLLO IL TIPO TRAINO
154600031202    3C     FGVTTR        IFNE      ' '
154700031202     C     FGVTTR        LOOKUP    TDF                                    36
154800031202    3C                   ENDIF
154900980623     C**
155000031202    3C     FGVTTR        IFEQ      ' '
155100031202     C     *IN36         OREQ      *OFF
155200031203     c                   clear                   flg15
155300031202     C                   CLEAR                   KPJBU
155400031216     c                   movel     param         paramx
155500031216     c                   z-add     brvnfv        parnfv
155600031216     c                   z-add     fgvdfv        pardtp
155700031202     C                   MOVEL     PARAM         KPJBU
155800031202     c                   call      'FNLR15R'
155900031202     c                   parm                    kpjba
156000031202     c                   parm                    dsbrv
156100031203     c                   parm                    flg15             1
156200031216     c                   movel     paramx        param
156300031202    3C                   ENDIF
156400031202    2C                   ENDIF
156500951011     C*
156600031202   1AC                   ENDIF
156700070123     C     KBRV9         READE     FNBRV09L                               35
156800031202    1C                   ENDDO
156900951011     C*
157000031202     C                   ENDSR
157100960826     C*
157200960827     C* CANCELLAZIONE ANOMALIA 50-COLLO ARRIVATO NON PARTITO --------*
157300960826     C     ANM050        BEGSR
157400960826     C                   Z-ADD     50            COMCAA
157500990510     C     KANM          CHAIN     FNANM000                           33
157600960826    1C     *IN33         IFEQ      *OFF
157700020809     C                   CLEAR                   FNLR33DS
157800960826     C                   Z-ADD     ANMNR2        D33NRR
157900960826     C                   MOVE      'S'           D33MAC
158000960826     C                   CALL      'FNLR33R'
158100020809     C                   PARM                    FNLR33DS
158200960826     C                   END
158300960826     C                   ENDSR
158400080411     c*----------------------------------------------------------------
158500911001**         CMD5
158600021008CHKOBJ OBJ(FNBLPE0R) OBJTYPE(*FILE)     MBR(M234567890)
158700911001**         CMD6
158800021008ALCOBJ OBJ((FNBLPE0R *FILE *EXCL           M234567890))
158900911002**         CMD9
159000021008OVRDBF FILE(FNBLPE0R)                   MBR(M234567890)
159100050804**         CmdDSPLOG
159200050804DSPJOBLOG OUTPUT(*PRINT)
159300000622**
159400080411RICEZIONE BOLLE ARRIVO A xxx: MBR Mxxxyyy                               1
159500111104Bolla già esistente .                                                   2
159600111104Non trovato dettaglio colli .                                           3
159700111104Segnacollo esistente xxxxxxx : SOSTITUITO .                             4
159800111104Segnacollo esistente xxxxxxx : NON sostituito .                         5
