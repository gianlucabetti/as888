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
008700050804     d tnsym5cds     e ds
008800020809     D DS7N          E DS
008900020809     D DS3A          E DS
009000020809     D DS7A          E DS
009100020809     D DSBL4A        E DS
009200070322     d dsbl4l        e ds
009300020809     D DSBL4J        E DS
009400020809     D DGEI          E DS
009500020809     D DGED          E DS
009600020809     D OG143         E DS
009700070322     d og148         e ds
009800031027     d Og150         e ds
009900020809     D YEURCODS      E DS
010000020809     D UT§DSE0F      E DS
010100020809     D CNCR80        E DS
010200020809     D FNLR33DS      E DS
010300020809     D FNLV55DS      E DS
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
031400060515     c                   If        §OgPck = 'S' and §OgPkd <> 'S'
031500060515     c                             or §ogsda = 'S'
031600060515     c                   eval      lnapks = 'S'
031700060515     c                   endif
031800070322      * controllo se LNA è abilitata a telefonate GEO
031900070322     c                   if        §oggeot = 'S'
032000070322     c                   eval      lnageo = 'S'
032100070322     c                   endif
032200060515     c                   endif
032300060515      * controllo se c'è il picking attivo
032400060515     c                   if        lnapks = *blanks
032500031210     C     d55tfa        CHAIN     AZORG01L                           32
032600031210     C  N32              MOVEL     ORGDf0        OG150
032700031210     C   32              CLEAR                   OG150
032800040616if  5c                   If        §OgPck = 'S' and §OgPkd <> 'S'
032900041215     c                             or §ogsda = 'S'
033000031210     c                   eval      lnapks = 'S'
033100031210     c                   else
033200031210     c                   clear                   lnapks
033300031210     c                   endif
033400060515     c                   endif
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
044800050804     c* Mando msg a posta
044900051006     c                   clear                   tnsym5cds
045000050804     C* FILIALE A CUI INVIARE IL MESSAGGIO
045100051006     C                   MOVEL     046           ym5fde
045200050804     C* UTENTE  A CUI INVIARE IL MESSAGGIO
045300051006     C                   MOVEL     CUTI          ym5ude
045400050804     C* TESTATA MESSAGGIO
045500051006     c                   eval      ym5tes='RICEZIONE BOLLE: FNLR14R -FILE ALLO'+
045600051006     C                             'CATO. VEDI DSPJOBLOG'
045700050804     C
045800051006     c                   eval      ym5t01='UTENTE :   '+ KNMUS
045900051006     c                   eval      ym5rig=1
046000050804     C*
046100051006     C                   CALL      'TNSYM5C'
046200051006     C                   PARM                    tnsym5cds
046300051006     c                   endif
046400050804     c
046500040428    2C                   ENDIF
046600040317     c*
046700040317     c* sottometto lr15 del foglio passato solo se è locale
046800040317     c                   if        nfv(1)>0
046900040317     c     parnfv        lookup    nfv                                    36
047000040317     c                   if        *in36 and pardef<>'S'
047100040317     c                   clear                   kpjbu
047200040317     C                   MOVEL     PARAM         KPJBU
047300040317     C                   CALL      'FNLR15R'
047400040317     c                   PARM                    KPJBA
047500040317     c                   endif
047600040317     c                   endif
047700950919     C*
047800950128     C** LANCIO PGM DI ABILITAZIONE BOLLE ALLA CONSEGNA
047900970509     C     *IN02         IFEQ      *ON
048000970509     C* CHIUDO PGM FNLS44R PER FARE SCRIVERE L'ULTIMO FNAGB
048100070228     C                   MOVEL     'C'           dls45flg
048200070228     C                   CALL      'FNLS45R'
048300070228     c                   parm                    fnls45ds
048400070228     c                   parm                    dsbrv
048500970509     C                   ENDIF
048600971114     C**
048700971114     C     WCAL55        IFEQ      'S'
048800020809     C                   CLEAR                   FNLV55DS
048900971114     C                   MOVEL     'C'           D55TLA
049000971114     C                   CALL      'FNLV55R'
049100020809     C                   PARM                    FNLV55DS
049200971114     C                   ENDIF
049300911016     C*
049400020809     C                   EVAL      *INLR = *ON
049500910322     C*
049600911107     C*--- CARICO TABELLE --------------------------------------------*
049700911107     C     CARTAB        BEGSR
049800921022     C*
049900921022     C* PRENDO TABELLA VARIABILI ARRIVI
050000921022     C                   MOVEL     '7A'          COD
050100921022     C                   MOVEL     *BLANKS       KEY
050200921022     C                   MOVEL     '1'           KEY
050300921022     C     KTAB          CHAIN     TABEL                              33
050400921022     C  N33              MOVEL     TBLUNI        DS7A
050500950130     C   33              CLEAR                   DS7A
050600010110     C* RICERCO FASE
050700921022     C                   MOVEL     '7N'          COD
050800960424     C                   MOVEL(P)  2             KEY
050900921022     C     KTAB          CHAIN     TABEL                              31
051000921022     C  N31              MOVEL     TBLUNI        DS7N
051100980623     C* CARICO TIPI TRAINO DEFLUENZA
051200980623     C                   MOVEL     'TV'          COD
051300020809     C                   CLEAR                   XX
051400980623     C     KTAB2         SETLL     TABEL
051500980623     C     KTAB2         READE     TABEL                                  31
051600980623     C     *IN31         DOWEQ     *OFF
051700980623     C     TBLFLG        IFEQ      ' '
051800980623     C                   MOVEL     TBLUNI        DSTV
051900980623     C     §TVDEF        IFEQ      'S'
052000020809     C                   ADD       1             XX
052100020809     C                   MOVEL     TBLKEY        TDF(XX)
052200980623     C                   ENDIF
052300980623     C                   ENDIF
052400980623     C**
052500980623     C     KTAB2         READE     TABEL                                  31
052600980623     C                   ENDDO
052700000622     C*
052800000622     C* CARICO IN SCHIERA ELENCO DIVISE ACCETTATE IN ITALIA
052900000622     C*
053000000622     C                   MOVEL     'CV'          COD
053100000622     C                   CLEAR                   DVA
053200011025     C                   CLEAR                   DEC
053300020809     C                   CLEAR                   XX
053400000622     C     KTAB2         SETLL     TABEL
053500000622     C     KTAB2         READE     TABEL                                  31
053600000622     C     *IN31         DOWEQ     *OFF
053700000622     C     TBLFLG        IFEQ      ' '
053800000622     C                   MOVEL     TBLUNI        DSCV
053900000622     C     §CVDVA        IFEQ      'S'
054000020809     C                   ADD       1             XX
054100020809     C                   MOVEL     TBLKEY        DVA(XX)
054200011025      * Carico in schiera il numero d decimali della divisa
054300020809     C                   Z-ADD     §CVDEC        DEC(XX)
054400000622     C                   ENDIF
054500000622     C                   ENDIF
054600000622     C**
054700000622     C     KTAB2         READE     TABEL                                  31
054800000622     C                   ENDDO
054900011029      *
055000020809     C                   CLEAR                   TIBS02DS
055100011029     C                   CLEAR                   KPJBU
055200011029     C                   MOVE      'C'           T02MOD
055300011029     C                   MOVE      KNSIF         T02SIF
055400011029     C                   MOVE      'GED'         T02COD
055500011029     C                   MOVEL(P)  1             T02KE1
055600011029     C                   CALL      'TIBS02R'
055700011029     C                   PARM                    KPJBA
055800020809     C                   PARM                    TIBS02DS
055900011029    6C     T02ERR        IFEQ      *BLANKS
056000011029     C                   MOVEL     T02UNI        DGED
056100011029     C                   ELSE
056200011029     C                   CLEAR                   DGED
056300011029    6C                   ENDIF
056400011029      *
056500011029      * Recupero il numero di decimali di §GEDCN
056600011029     C                   MOVEL     'CV'          TBLCOD
056700011029     C                   MOVEL     *BLANKS       TBLKEY
056800011029     C                   MOVEL     §GEDCN        TBLKEY
056900011029     C     KTAB          CHAIN     TABEL                              31
057000011029     C  N31              MOVEL     TBLUNI        DSCV
057100011029     C   31              CLEAR                   DSCV
057200011030      * IMPORTI VARI
057300020809     C                   CLEAR                   TIBS02DS
057400011030     C                   CLEAR                   KPJBU
057500011030     C                   MOVE      'C'           T02MOD                         CONTROLLO
057600011030     C                   MOVE      KNSIF         T02SIF                         S.I.
057700011030     C                   MOVE      'GEI'         T02COD                         TABELLA PROFILI
057800011030     C                   MOVEL(P)  §GEDCN        T02KE1                         PROFILO IN K1
057900011030     C                   CALL      'TIBS02R'
058000011030     C                   PARM                    KPJBA
058100020809     C                   PARM                    TIBS02DS
058200011030    6C     T02ERR        IFEQ      *BLANKS
058300011030     C                   MOVEL     T02UNI        DGEI
058400011030     C                   ELSE
058500011030     C                   CLEAR                   DGEI
058600011030    6C                   ENDIF
058700911107     C                   ENDSR
058800911001     C**---------------------------------------------------------
058900911001     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
059000911001     C**---------------------------------------------------------
059100911001     C     CAEX          BEGSR
059200040428     c                   setoff                                       22
059300020809     C                   MOVE      *BLANK        VAR11
059400020809     C                   MOVE      *BLANK        VAR12
059500911001     C                   MOVE      ')'           VAR11
059600021008     C                   MOVEL     MBRfil        VAR11
059700911001     C                   MOVE      '))'          VAR12
059800021008     C                   MOVEL     MBRfil        VAR12
059900021008     C                   Z-ADD     55            LUNG
060000911001     C*
060100911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
060200911001     C                   SETOFF                                       20
060300950118     C                   MOVE      VAR11         CMD5                            FNBLPE0R
060400911001     C                   MOVEL     *BLANKS       COMMAN
060500020809     C                   MOVEA     CMD5(1)       COMMAN
060600911001     C                   CALL      'QCMDEXC'                            20
060700911001     C                   PARM                    COMMAN
060800911001     C                   PARM                    LUNG
060900040428     c*
061000040428     C* 20 ON - ESCO
061100040428     c                   if        *in20
061200040428     c                   seton                                          22
061300040428     C                   GOTO      ENDCAE
061400040428     c                   endif
061500911002     C* ALLOCO MEMBRO: SE NON RIESCO ESCO DA PROGRAMMA
061600911001     C                   SETOFF                                       20
061700911002     C                   MOVE      VAR12         CMD6
061800911001     C                   MOVEL     *BLANKS       COMMAN
061900911002     C                   MOVEA     CMD6(1)       COMMAN
062000911001     C                   CALL      'QCMDEXC'                            20
062100911001     C                   PARM                    COMMAN
062200911001     C                   PARM                    LUNG
062300911002     C*
062400050804     c                   if        *in20
062500050804     c                   eval      fisico='FNBLPE0R'
062600050804     C                   GOTO      ENDCAE
062700050804     c                   endif
062800911001     C* OVRDBF
062900911002     C                   SETOFF                                       20
063000911002     C                   MOVE      VAR11         CMD9
063100911001     C                   MOVEL     *BLANKS       COMMAN
063200911002     C                   MOVEA     CMD9(1)       COMMAN
063300911002     C                   CALL      'QCMDEXC'                            20
063400911001     C                   PARM                    COMMAN
063500911001     C                   PARM                    LUNG
063600050804     C   20              GOTO      ENDCAE
063700911002     C*
063800950118     C                   OPEN      FNBLPE0R
063900911001     C*
064000911002     C     ENDCAE        ENDSR
064100911002     C**---------------------------------------------------------
064200911002     C** CREO BOLLA ARRIVI
064300911002     C**---------------------------------------------------------
064400980311     C     CRTBOL        BEGSR
064500080407     c*
064600931125     C* S C R I V O   B O L L A
064700990906     C                   EXSR      RIEARB
064800000622     C* LE SEGUENTI SPECIFICHE LE ESEGUO SOLO SE BOLLA NON PRESENTE
064900000622   1AC     *IN06         IFEQ      *OFF
065000911003     C*
065100931125     C* D E T T A G L I O   C O L L I
065200950413     C*
065300990906     C                   EXSR      RIEBLT
065400931125     C*
065500080407     C* Leggo il numero dei segnacolli dal dettaglio bolle partenza
065600080407     C     KBLP2         SETLL     FNBLT01l                               32
065700921123     C*
065800921123     C* 32 OFF-  NON TROVATO IL DETTAGLIO COLLI: STAMPO ERRORE
065900950118    2C     *IN32         IFEQ      *OFF
066000080411     c     §3arbl        andne     'R'
066100090109     c     §3aurc        andne     'S'
066200921123     C                   SETON                                        07
066300921123     C                   EXSR      STAERR
066400950118   X2C                   ELSE
066500921123     C*
066600080407     C     KBLP2         READE     FNBLT01l                               32
066700990906     C*
066800950118    3C     *IN32         DOWEQ     *OFF
066900100414     C                   Z-ADD     BLTfls        ARTfls
067000100414     C                   Z-ADD     BLTNSC        ARTNSC
067100080407     c                   exsr      PesoeVol
067200931125     C*
067300920917     C* CONTROLLO SE SEGNACOLLO GIA' ESISTENTE
067400950905     C                   SETOFF                                       0408
067500950413     C     KSPP          CHAIN     FNART27L                           31
067600080407    4C     *IN31         IFEQ      *OFF
067700950413     C                   SETON                                        04
067800080407    5C     ARTDCM        IFNE      *ZEROS
067900950905     C                   SETON                                        08
068000950413     C                   EXSR      RIEBLT
068100100414     C                   Z-ADD     BLTfls        ARTfls
068200950418     C                   Z-ADD     BLTNSC        ARTNSC
068300080407     c                   exsr      PesoeVol
068400950413     C* Segnacollo già esistente: se già consegnato aggiorno il record
068500950413     C                   UPDATE    FNART000
068600040317     c   03              exsr      ctrdet
068700040317     c
068800080407   x5C                   ELSE
068900040317     c
069000950418     C                   EXSR      RIEBLT
069100100414     C                   Z-ADD     BLTfls        ARTfls
069200950418     C                   Z-ADD     BLTNSC        ARTNSC
069300950413     C                   UNLOCK    FNART27L
069400080407    5C                   END
069500950413     C* Stampo errore
069600950413     C                   EXSR      STAERR
069700080407   x4C                   ELSE
069800980311     C*
069900951011     C                   WRITE     FNART000
070000031202     c   03              exsr      ctrdet
070100080407    4C                   END
070200911014     C*
070300080407     C     KBLP2         READE     FNBLT01l                               32
070400950118    3C                   ENDDO
070500950118    2C                   ENDIF
070600911003     C*
070700000622   1AC                   ENDIF
070800911003     C* SCRIVO FILE
070900000622     C  N06              WRITE     FNARB000
071000000622     C   06              UPDATE    FNARB000
071100080407     C*
071200911003     C* DELETO
071300000622     C     *IN06         IFEQ      *OFF
071400950118     C                   DELETE    FNBLPE00
071500000622     C                   ELSE
071600000622     C                   EXSR      DLTRIC
071700000622     C                   ENDIF
071800960424     C**
071900960424     C* CANCELLO EVENTUALE ANOMALIA 12
072000960424     C                   Z-ADD     12            COMCAA
072100990510     C     KANM1         CHAIN     FNANM01L                           33
072200960424     C* CHIUDO ANOMALIA
072300960424    1C     *IN33         IFEQ      *OFF
072400020809     C                   CLEAR                   FNLR33DS
072500960424     C                   Z-ADD     ANMNR1        D33NRR
072600960424     C                   MOVE      DATEU         D33DCH
072700960424     C                   MOVEL     §7NFS1        D33FSC
072800960424     C                   CALL      'FNLR33R'
072900020809     C                   PARM                    FNLR33DS
073000960424     C                   ENDIF
073100921022     C*
073200000622     C     *IN06         IFEQ      *OFF
073300000111     C     BLPLNA        CHAIN     AZORG01L                           32
073400020218     C  N32              MOVEL     ORGDE3        OG143
073500020218     C   32              CLEAR                   OG143
073600020226     c                   eval      lnantw = §ogntw
073700921022     C* CONTROLLO SE C'ERANO ANOMALIE MANCA BOLLA
073800960827     C*           E ANOMALIE 050-COLLO ARR. NON PART. SU BOLLE LOCALI
073900950123     C     KBLP2         SETLL     FNART001
074000020226     C     lnaNTW        IFNE      'EEX'
074100020226     C     lnaNTW        ANDNE     'EUP'
074200020226     C     lnaNTW        ANDNE     'FED'
074300060322     C     lnaNTW        ANDNE     'DPD'
074400961121     C     KBLP2         READE(N)  FNART001                               32
074500961121     C                   ELSE
074600961121     C     KBLP2         READE     FNART001                               32
074700961121     C                   ENDIF
074800921022     C*
074900961121     C     *IN32         DOWEQ     *OFF
075000950128     C                   EXSR      CTRANM
075100960827     C* . . . . . BOLLA LOCALE , SERIE>0 , FLS *NE LNP
075200960827     C     *IN03         IFEQ      *ON
075300960827     C     ARTNRS        ANDGT     *ZEROS
075400960827     C     ARTFLS        ANDNE     ARTLNP
075500960827     C                   EXSR      ANM050
075600960827     C                   END
075700961121     C**
075800000111     C* SE LNA ESTERA POSSO AGGIORNARE IL RECORD
075900020226     C     lnaNTW        IFNE      'EEX'
076000020226     C     lnaNTW        ANDNE     'EUP'
076100020226     C     lnaNTW        ANDNE     'FED'
076200060322     C     lnaNTW        ANDNE     'DPD'
076300961121     C     KBLP2         READE(N)  FNART001                               32
076400961121     C                   ELSE
076500961121     C     KBLP2         READE     FNART001                               32
076600921022     C                   END
076700961121     C**
076800961121     C                   ENDDO
076900000622     C                   ENDIF
077000911002     C*
077100911002     C                   ENDSR
077200080407     C**---------------------------------------------------------
077300080407     c* imposto peso e volume della partenza sul dettaglio colli
077400080407     C**---------------------------------------------------------
077500080407     c     PesoeVol      BEGSR
077600080407     c                   z-add     bltvuc        artvuc
077700080407     c                   movel     bltfvc        artfvc
077800080407     c                   z-add     bltpuc        artpuc
077900080407     c                   movel     bltfpc        artfpc
078000080407     c                   select
078100080407     c                   when      artvuc>0 and artpuc>0
078200080407     c                   eval      arbfbs='E'
078300080423     c                   when      artvuc>0   and arbfbs=' '
078400080407     c                   eval      arbfbs='V'
078500080423     c                   when      artpuc>0   and arbfbs=' '
078600080407     c                   eval      arbfbs='P'
078700080423     c                   other
078800080423     c                   eval      arbfbs='E'
078900080407     c                   endsl
079000080407     c                   ENDSR
079100911002     C**---------------------------------------------------------
079200911002     C** RIEMPO CAMPI FLARB00F
079300911002     C**---------------------------------------------------------
079400911002     C     RIEARB        BEGSR
079500000622     C* DEVO CLEARARE FORMATO FNARB SOLO SE BOLLA NON PRESENTE
079600000622     C  N06              CLEAR                   FNARB000
079700931125     C*
079800950118     C     BLPCBO        IFNE      SAVCBO
079900920603     C                   MOVEL     '3A'          COD
080000920603     C                   MOVEL     *BLANKS       KEY
080100950118     C                   MOVEL     BLPCBO        KEY
080200920603     C     KTAB          CHAIN     TABEL                              33
080300920603     C  N33              MOVEL     TBLUNI        DS3A
080400920603     C   33              MOVEL     'F1'          §3ATB1
080500920625     C   33              MOVEL     '  '          §3ATB2
080600950118     C                   MOVEL     BLPCBO        SAVCBO            2
080700950118     C                   ENDIF
080800920603     C*
080900911002     C                   MOVEL     BLPLNP        ARBLNP
081000950118     C                   MOVE      BLPAAS        ARBAAS
081100911002     C                   MOVEL     BLPMGS        ARBMGS
081200911002     C                   MOVEL     BLPNRS        ARBNRS
081300911002     C                   MOVEL     BLPNSP        ARBNSP
081400010110     C* L'IMPOSTAZIONE DEL CODICE BOLLA SE STO RICEVENDO UN AGGIORNAMEN
081500010110     C* TO DI BOLLA AUTOGENERATA LA FACCIO QUANDO GESTISCO IL CONTRAS-
081600010110     C* SEGNO
081700010110     C  N06              MOVEL     BLPCBO        ARBCBO
081800950118     C                   MOVEL     BLPFDN        ARBFDN
081900911002     C                   MOVEL     BLPFNS        ARBFNS
082000911002     C                   MOVEL     BLPLNA        ARBLNA
082100911002     C                   MOVEL     BLPKSC        ARBKSC
082200911002     C                   MOVEL     BLPZNC        ARBZNC
082300911002     C                   MOVEL     BLPRSM        ARBRSM
082400911002     C                   MOVEL     BLPINM        ARBINM
082500911002     C                   MOVEL     BLPLOM        ARBLOM
082600911002     C                   MOVEL     BLPPRM        ARBPRM
082700950118     C                   MOVEL     BLPCAM        ARBCAM
082800950118     C                   MOVEL     BLPNZM        ARBNZM
082900911002     C                   MOVEL     BLPRSD        ARBRSD
083000911003     C                   MOVEL     BLPIND        ARBIND
083100911002     C                   MOVEL     BLPLOD        ARBLOD
083200911002     C                   MOVEL     BLPPRD        ARBPRD
083300950118     C                   MOVEL     BLPCAD        ARBCAD
083400950118     C                   MOVEL     BLPNZD        ARBNZD
083500911002     C                   MOVEL     BLPGC1        ARBGC1
083600911002     C                   MOVEL     BLPGC2        ARBGC2
083700911002     C                   MOVEL     BLPCTR        ARBCTR
083800911002     C                   MOVEL     BLPCTS        ARBCTS
083900911002     C                   MOVEL     BLPFTM        ARBFTM
084000950118     C                   MOVEL     BLPFIN        ARBFIN
084100911002     C                   MOVEL     BLPFAP        ARBFAP
084200911002     C                   MOVEL     BLPTSP        ARBTSP
084300950118     C                   Z-ADD     BLPIAS        ARBIAS
084400950123     C                   MOVEL     BLPVAS        ARBVAS
084500911002     C                   MOVEL     BLPNAS        ARBNAS
084600911002     C                   MOVEL     BLPNCL        ARBNCL
084700950118     C                   Z-ADD     BLPPKB        ARBPKB
084800950118     C                   Z-ADD     BLPPKF        ARBPKF
084900000622     C* SE BOLLA GIA' PRESENTE SOSTITUISCO SOLO SE IL VOLUME SU ARB
085000000622     C* NON E' NE' "T" NE' "Z" E SE IL VOLUME RICEVUTO E' > DI QUELLO
085100000622     C* SU ARB
085200000622     C     *IN06         IFEQ      *OFF
085300000622     C     ARBFVF        ORNE      'T'
085400000622     C     ARBFVF        ANDNE     'Z'
085500000622     C     BLPVLF        ANDGT     ARBVLF
085600950118     C                   Z-ADD     BLPVLF        ARBVLF
085700950118     C                   MOVEL     BLPFVF        ARBFVF
085800000622     C                   ENDIF
085900950118     C                   Z-ADD     BLPVLB        ARBVLB
086000950118     C                   MOVEL     BLPFVB        ARBFVB
086100950118     C                   MOVEL     BLPQFT        ARBQFT
086200950118     C                   MOVEL     BLPVMD        ARBVMD
086300950118     C                   MOVEL     BLPVAD        ARBVAD
086400070222     C*
086500911002     C                   MOVEL     BLPCCM        ARBCCM
086600950118     C                   Z-ADD     BLPRMN        ARBRMN
086700950118     C                   MOVEL     BLPRMA        ARBRMA
086800911002     C                   MOVEL     BLPRMO        ARBRMO
086900911002     C                   MOVEL     BLPFLS        ARBFLS
087000911002     C                   MOVEL     BLPNCD        ARBNCD
087100911002     C                   MOVEL     BLPNCA        ARBNCA
087200911002     C                   MOVEL     BLPXCO        ARBXCO
087300950118     C*
087400911002     C                   MOVEL     BLPFFD        ARBFFD
087500950118     C                   MOVE      BLPDCR        ARBDCR
087600911002     C                   MOVEL     BLPHCR        ARBHCR
087700911002     C                   MOVEL     BLPTCR        ARBTCR
087800950118     C                   MOVE      BLPDBR        ARBDBR
087900931215     C                   MOVEL     BLPNFV        ARBNFV
088000950124     C*
088100911002     C                   MOVEL     BLPFLE        ARBFLE
088200911002     C                   MOVEL     BLPTFA        ARBTFA
088300911002     C                   MOVEL     BLPTFP        ARBTFP
088400911002     C                   MOVEL     BLPFEA        ARBFEA
088500911008     C                   MOVEL     BLPFBR        ARBFBR
088600950118     C                   MOVEL     BLPTC1        ARBTC1
088700950118     C                   MOVEL     BLPTC2        ARBTC2
088800950118     C                   MOVEL     BLPGVA        ARBGVA
088900950118     C                   MOVEL     BLPGMA        ARBGMA
089000950118     C                   MOVEL     BLPGGA        ARBGGA
089100001204     C*  SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA N FT1
089200051116     C* BLANK NON LO DEVO AGGIORNARE ALTRIMENTI NON VENGONO TRASMESSE
089300001204     C* LE DATE DI CONSEGNA
089400001204     C     *IN06         IFEQ      *OFF
089500001204     C     ARBFT1        ORNE      *BLANKS
089600931125     C                   MOVEL     'R'           ARBFT1
089700950118     C                   Z-ADD     DATEU         ARBDT1
089800001204     C                   ENDIF
089900970212     C*
090000060220     c* Imposto nel tipo record "A" (se non c'e' lo creo,)
090100060220     C*   IL 1° BYTE DEL CAMPO AR4NOT  E §B4ASO=SIGLA OPERATORE
090200060302     c                   clear                   contaerror
090300060220     c                   eval      wtrc='A'
090400060302     c
090500060302     c* Se errore in write (succede per conflitto con fnlrc1r)
090600060302     c*  ritento l'update
090700060302    1c                   dou       not *in99
090800060302     c                   setoff                                       99
090900060220     c     kar4          chain     fiar401l
091000060302     c                   exsr      Scriviar4A
091100060302     c
091200060302     c* dopo 10 tentativi mando un messaggio in posta ed esco senza
091300060302     c*  impostare il record A
091400060302     c                   add       1             contaerror        2 0
091500060302    2c                   if        contaerror>10
091600060302     C
091700060302     c* Mando msg a posta
091800060302     c                   clear                   tnsym5cds
091900060302     C* FILIALE A CUI INVIARE IL MESSAGGIO
092000060302     C                   MOVEL     046           ym5fde
092100060302     C* UTENTE  A CUI INVIARE IL MESSAGGIO
092200060302     C                   MOVEL     CUTI          ym5ude
092300060302     C* TESTATA MESSAGGIO
092400060302     c                   eval      ym5tes='RICEZIONE BOLLE: FNLR14R     - Non '+
092500060302     C                             'scritto record"A" '
092600060302     C
092700060302     c                   eval      ym5t01='UTENTE :   '+ KNMUS
092800060302     c                   eval      ym5t02='Bolla  : '+ (%editc(arbaas:'Z'))
092900060302     c                             + (%editc(arblnp:'Z')) + (%editc(arbnrs:'2'))
093000060302     c                             + (%editc(arbnsp:'2'))
093100060302     c                   eval      ym5rig=2
093200060302     C*
093300060302     C                   CALL      'TNSYM5C'
093400060302     C                   PARM                    tnsym5cds
093500060302     c
093600060302     c                   leave
093700060302    2c                   endif
093800060302    1c                   enddo
093900060302     c
094000060220     c
094100060220     c* Cerco partita iva da impostare su fnarb
094200060220     c                   eval      wtrc='P'
094300060220     c     kar4          chain(N)  fiar401l
094400060220     c                   if        %found(fiar401l)
094500060220     C                   MOVEL     §3ATB1        UNOTB1            1
094600060220     C     UNOTB1        IFEQ      'F'
094700060220     C     §3ATB2        ANDEQ     *BLANKS
094800060220     C                   MOVEL     AR4NOT        ARBCPI
094900060220     C                   ELSE
095000060220     C                   MOVE      AR4NOT        ARBCPI
095100060220     C                   ENDif
095200061031
095300061031     c                   else
095400061031      * se non c'è il fiar4 rcd P aggancio la bolla partenza se bolla unica
095500061031     c                   if        §3atb2 = *blanks
095600061031     c     kblp2         chain     fnblp01l
095700061031     c                   if        %found(fnblp01l)
095800061031     c                   eval      arbcpi = w_blpcpi
095900061031     c                   endif
096000061031     c                   endif
096100061031
096200060220     C                   ENDif
096300030109      *
096400930201     C* A B I L I T A Z I O N E   M E R C E
096500931125     C**
096600000622     C*
096700930201     C* SE FOGLIO VIAGGIO 99999 ABILITO SUBITO
096800000623     C  N06BLPNFV        IFEQ      99999
096900930201     C                   MOVEL     3             ARBAMA
097000950919     C                   ENDIF
097100931125     C**
097200931125     C* A B I L I T A Z I O N E   C / A S S E G N O
097300000623     C*
097400911002     C* PER BOLLE SENZA CONTRASSEGNO
097500051116     c* Vedo se essite C/A per abilitazione
097600051116     c     kblp2         chain     fiar901l
097700051116     c                   if        not %found(fiar901l) or ar9cas=0
097800911002     C                   MOVEL     'S'           ARBACC
097900950119   X1C                   ELSE
098000011211
098100011210      * ------------------
098200011210
098300011210      * NUOVO CONTROLLO C/ASSEGNO
098400011210     c                   clear                   trul21ds
098500011210     c                   eval      i21tla = 'L'
098600011210     c                   eval      i21cbo = blpcbo
098700011210     c                   eval      i21tsp = blptsp
098800011210     c                   eval      i21lnp = blplnp
098900011210     c                   eval      i21nzm = blpnzm
099000011210     c                   eval      i21lna = blplna
099100011210     c                   eval      i21nzd = blpnzd
099200051116     c                   eval      i21tic = ar9tic
099300051116     c                   eval      i21imp = ar9cas
099400051116     c                   eval      i21div = ar9vca
099500011210     c                   eval      i21ute = knmus
099600011210     c                   eval      i21pgm = nompgm
099700011210     c                   call      'TRUL21R'
099800011210     c                   parm                    trul21ds
099900011210
100000011210     c                   if        o21fca = *blanks
100100011210     c                   eval      arbacc = 'S'
100200011210     c                   endif
100300911002     C*
100400950119    1C                   END
100500931125     C**
100600950119     C* A B I L I T A Z I O N E    A S S E G N A T O
100700931125     C**
100800000623   1AC     *IN06         IFEQ      *OFF
100900000623     C*
101000911003     C                   MOVEL     §3ATB1        UNOTB1            1
101100911003     C* FRANCO
101200950119    1C     UNOTB1        IFEQ      'F'
101300930112     C* SE NON C'E' SECONDA BOLLA ABILITO
101400950119    2C     §3ATB2        IFEQ      '  '
101500911003     C                   MOVEL     'S'           ARBACA
101600950119    2C                   END
101700950119    1C                   END
101800050314     c
101900050314     c* Se già tassata, abilito subito l'assegnato
102000050314     c                   if        arbaca=' '
102100050314     c                   if        §3atb2<>*blanks
102200050314     c                   eval      ktrc='2'
102300050314     c                   else
102400050314     c                   eval      ktrc='1'
102500050314     c                   endif
102600050314     c
102700050314     c     kar6          chain     fiar601l
102800050314     c                   if        %found(fiar601l)
102900050314     c                   move      ar6ksc        fineksc
103000050314     c                   if        fineksc<>'9999' and fineksc<>'0000'
103100050314     c                   eval      arbaca='S'
103200050314     c* mi salvo i campi da inmpostare in arbksc e arbctr
103300050314     c                   eval      arbksc=ar6ksc
103400050314     c                   eval      arbctr=ar6ctr
103500050331     c* Se si tratta di bolla di recupero, devo anche chiudere la spedizione
103600050331    3C     §3ATB1        IFEQ      'A3'
103700050331     C* IMPOSTO I CAMPI DELLA CHIUSURA
103800050331     C                   MOVEL     'Y'           ARBICC
103900050331     C                   MOVEL     'S'           ARBICA
104000050331     C                   Z-ADD     DATEU         ARBDCM
104100050331     C                   time                    wora              6 0
104200050331     C                   movel     wora          ARBHMC
104300050331     C                   CLEAR                   ARBFT1
104400050331     C                   CLEAR                   ARBDT1
104500050331     C                   MOVEL     *ALL'9'       ARBNDC
104600050331     C                   Z-ADD     DATEU         ARBDDC
104700050331     C                   MOVEL     'S'           ARBFDC
104800050331     C                   MOVEL     *ALL'9'       ARBNMI
104900050331     C                   Z-ADD     DATEU         ARBDMI
105000050331     C                   MOVEL     *ALL'9'       ARBCUC
105100050331     C                   CLEAR                   ARBFBC
105200050331    3C                   ENDIF
105300050314     c                   endif
105400050314     c                   endif
105500050314     c                   endif
105600050314     c
105700000623   1AC                   ENDIF
105800070322
105900070322      * se LNA o sua capofila abilitata alle telefonate per GEO
106000070329      *  e NO bolle di recupero
106100070329     c                   if        lnageo = 'S' and §3arbl <> 'R'
106200070322     c                   clear                   dsbl4l
106300070322      * Genero record blocco spedizione se:
106400070322      * --> Consegna particolare Appuntamento o Supermercato (non tassative)
106500070322     c                   if        arbtc1 = 'A' or arbtc1 = 'S' or arbtc2 = 'A'
106600070322     c                             or arbtc2 = 'S'
106700070322     c                   eval      §b4lbas = 'S'
106800070322      * se consegna tassativa sblocco
106900070322     c                   if        arbdcr <> *zeros and arbtcr = *blanks
107000070322     c                   clear                   §b4lbas
107100070322     c                   endif
107200070322     c                   endif
107300070322      * --> Contrassegno non abilitato
107400070322     c                   if        arbacc = *blanks
107500070322     c                   eval      §b4ltca = 'S'
107600070322     c                   endif
107700070322      * --> Fermo deposito
107800070322     c                   if        arbffd <> *blanks
107900070322     c                   eval      §b4ltfd = 'S'
108000070322     c                   endif
108100070322      * scrivo record L di fiar4 per la gestione dei blocchi
108200070322     c                   if        dsbl4l <> *blanks
108300070322     c                   clear                   fiar4000
108400070322     c                   eval      ar4aas = blpaas
108500070322     c                   eval      ar4lnp = blplnp
108600070322     c                   eval      ar4nrs = blpnrs
108700070322     c                   eval      ar4nsp = blpnsp
108800070322     c                   eval      ar4trc = 'L'
108900070322     c                   eval      ar4not = dsbl4l
109000070322     c                   eval      ar4ftr = 'R'
109100070322     c                   eval      ar4dtr = dateu
109200070322     c                   eval      ar4duv = dateu
109300070322     c                   write     fiar4000                             99
109400070322     c                   endif
109500070322     c                   endif
109600950118     C*
109700911002     C                   ENDSR
109800070322      *-----------------------------------------------------------------------
109900060302     c     Scriviar4A    BEGSR
110000060302     c                   if        %found(fiar401l)
110100060302     c                   movel     ar4not        dsbl4a
110200060302     c                   else
110300060302     C                   MOVE      BLPAAS        AR4AAS
110400060302     C                   MOVEL     BLPLNP        AR4LNP
110500060302     C                   MOVEL     BLPNRS        AR4NRS
110600060302     C                   MOVEL     BLPNSP        AR4NSP
110700060302     C                   MOVEL     'A'           AR4TRC
110800060302     C                   CLEAR                   AR4NOT
110900060302     C                   CLEAR                   DSBL4A
111000060302     c                   movel     'R'           ar4ftr
111100060302     c                   movel     dateu         ar4DTR
111200060302     c                   endif
111300060302     c
111400060302     c                   movel     dateu         ar4duv
111500060302     C                   MOVEL     BL2SV1        §B4ABM
111600060302
111700060302      * se linea di arrivo con picking attivo o lna per cui stampare i
111800060302      * DDT SI in arrivo
111900060302if  1c                   If        lnapks = 'S'
112000060302 s  2c                   Select
112100060302
112200060302      * per bolla di recupero
112300060302      * imposto che la bolla è già stata stampata
112400060302     c                   When         §3arbl = 'R'
112500060302     c                   If        Bl2Sv1 = 'S'
112600060302     c                   Eval      §B4Abm = 'P'
112700060302     c                   EndIf
112800060302     c                   If        Bl2Sv1 = 'C'
112900060302     c                   Eval      §B4Abm = 'Q'
113000060302     c                   EndIf
113100060302     c                   If        Bl2Sv1 = 'N' or Bl2Sv1 = 'Y'
113200060302     c                   Eval      §B4Abm = 'J'
113300060302     c                   Endif
113400060302     c                   If        Bl2Sv1 = 'K'
113500060302     c                   Eval      §B4Abm = 'W'
113600060302     c                   Endif
113700060302
113800060302      * se bl2sv1 = 'Y'
113900060302      * imposto 'N' x bolla ancora da stampare
114000060302     c                   When      Bl2Sv1 = 'Y'
114100060302     c                   Eval      §B4Abm = 'N'
114200060302
114300060302    2c                   Endsl
114400060302    1c                   Endif
114500060302
114600060302     C     §7AFSR        IFEQ      'Y'
114700060302     C     BL2SV1        ANDEQ     'Y'
114800060302     C     lnpNTW        ANDNE     'DPD'
114900060302     C                   MOVEL     'N'           §B4ABM
115000060302     C                   ENDIF
115100060302     c
115200060302     c* Sigla operatore video
115300060302     C                   MOVE      BL2CEI        §B4ASO
115400060302     C                   MOVEL     DSBL4A        AR4NOT
115500060302     c                   if        %found(fiar401l)
115600060302     c                   update    fiar4000
115700060302     c                   else
115800060302     C                   WRITE     FiAR4000                             99
115900060302     c                   endif
116000060302     c                   endsr
116100911002     C**---------------------------------------------------------
116200911002     C** RIEMPO DETTAGLIO SEGNACOLLI
116300911002     C**---------------------------------------------------------
116400911002     C     RIEBLT        BEGSR
116500950119     C                   CLEAR                   FNART000
116600950118     C                   Z-ADD     ARBAAS        ARTAAS
116700931125     C                   Z-ADD     BLPLNP        ARTLNP
116800911002     C                   Z-ADD     BLPNRS        ARTNRS
116900911002     C                   Z-ADD     BLPNSP        ARTNSP
117000911002     C                   MOVEL     BLPLNA        ARTLNA
117100911002     C                   MOVEL     BLPFLS        ARTFLS
117200950119     C                   MOVEL     'R'           ARTFTR
117300960712     C* SE COLLO LOCALE INDICATO CHE E' ABBINATO
117400960712     C     *IN03         IFEQ      *ON
117500960712     C                   MOVEL     'S'           ARTABN
117600960712     C                   ENDIF
117700950119     C                   ENDSR
117800911014     C**---------------------------------------------------------
117900960424     C** CONTROLLO SE ESISTE ANOMALIA 10-NO REC BOLLA E 12-
118000911014     C**---------------------------------------------------------
118100950128     C     CTRANM        BEGSR
118200960529     C                   Z-ADD     0             OFV               5 0
118300970521     C*
118400970521     C* CHIUDO ANOMALIA 410 SE ESISTE
118500970521     C                   Z-ADD     410           COMCAA
118600990510     C     KANM          CHAIN     FNANM000                           33
118700970521    1C     *IN33         IFEQ      *OFF
118800020809     C                   CLEAR                   FNLR33DS
118900970521     C                   Z-ADD     ANMNR2        D33NRR
119000970521     C                   MOVE      DATEU         D33DCH
119100970521     C                   MOVEL     §7NFS1        D33FSC
119200970521     C                   CALL      'FNLR33R'
119300020809     C                   PARM                    FNLR33DS
119400970521    1C                   END
119500950128     C*
119600960424     C                   Z-ADD     10            COMCAA
119700990510     C     KANM          CHAIN     FNANM000                           33
119800950128     C* CHIUDO ANOMALIA
119900950128    1C     *IN33         IFEQ      *OFF
120000000623     C*
120100020809     C                   CLEAR                   FNLR33DS
120200960415     C                   Z-ADD     ANMNR2        D33NRR
120300960415     C                   MOVE      DATEU         D33DCH
120400960415     C                   MOVEL     §7NFS1        D33FSC
120500960415     C                   CALL      'FNLR33R'
120600020809     C                   PARM                    FNLR33DS
120700960415     C*
120800950128     C* ESEGUO LA CALL AL PGM DI CARICAMENTO SPUNTE PER AGGIORNARE
120900951213     C*  LA BOLLA. NON LO FACCIO SE E' LOCALE
121000961121    2C     *IN98         IFEQ      *OFF
121100951213     C     *IN03         ANDEQ     *OFF
121200021015     c                   Eval      Pa5Mbr = MbrFil
121300021015     c                   Movel     Pa5Mbr4       PA5FIL
121400950128     C                   MOVEL     'F'           PA5FIL
121500950128     C                   MOVE      PARAM5        KPJBU
121600950203     C                   MOVEL     'LR22'        KCOAZ
121700950203     C                   CALL      'BCH10'
121800950203     C                   PARM                    KPJBA
121900950203     C                   SETON                                        98
122000961121    2C                   ENDIF
122100070831     c
122200070831     c* Se esiste anomalia di collo arrivato non partito
122300070831     c*  chiano in update il file fnart e aggiorno i dati della partenza
122400070831     C                   Z-ADD     050           COMCAA
122500070831     C     KANM          CHAIN     FNANM000                           33
122600070831    2C     *IN33         IFEQ      *OFF
122700070831     c     kart          chain     fnart01l
122800070831    3c                   if        %found(fnart01l)
122900070831     c* Se manca data partenza la aggiorno, altrimenti data uscita transito
123000070831    4c                   if        anmcdu=arbtfp
123100070831     C                   Z-ADD     anmdao        ARTDFV
123200070831     C                   Z-ADD     anmnfv        ARTnFV
123300070831     C                   Z-ADD     88            ARTNPP
123400070831    5c                   if        arbtfp=arbtfa
123500070831     C                   Z-ADD     90            ARTNPP
123600070831    5c                   endif
123700070831     C                   movel     'S'           ARTABN
123800070831   x4c                   else
123900070831     C                   Z-ADD     anmdao        ARTDUT
124000070831     C                   Z-ADD     89            ARTPUT
124100070831     C                   Z-ADD     anmcdu        ARTFLP
124200070831     C                   movel     'S'           ARTABN
124300070831    4c                   endif
124400070831     c
124500070831    4c                   if        lnantw<>'EEX' and lnantw<>'EUP' and
124600070831     c                             lnantw<>'FED' and lnantw<>'DPD'
124700070903     c                   update    fnart001
124800070831    4c                   endif
124900070831    3c                   endif
125000070831    2c                   endif
125100961122     C**
125200000111     C** SE LNA ESTERA CONTROLLO SE ESISTE EVENTO COLLO 2 /6A/6
125300020226    2C     lnaNTW        IFEQ      'EEX'
125400020226    2C     lnaNTW        OREQ      'EUP'
125500020226    2C     lnaNTW        OREQ      'FED'
125600060322    2C     lnaNTW        OREQ      'DPD'
125700961122     C                   CLEAR                   WDEV
125800961122     C                   CLEAR                   WCEV
125900961122     C* APRO IL FILE
126000961122    3C     WAPER         IFEQ      ' '
126100961122     C                   OPEN      FNEVS02L
126200961122     C                   MOVEL     'S'           WAPER             1
126300961122    3C                   ENDIF
126400961122     C**
126500961122     C                   MOVEL     'E2 '         KCEV
126600020809    3C                   DO        3             XX
126700961122     C     KEVS          SETLL     FNEVS02L
126800961122     C     KEVS          READE     FNEVS02L                               33
126900961122    4C     *IN33         DOWEQ     *OFF
127000970602    5C     EVSDEV        IFGT      WDEV
127100961122     C     WDEV          OREQ      0
127200961122     C                   MOVEL     EVSDEV        WDEV
127300961122     C                   MOVEL     EVSCEV        WCEV
127400961122    5C                   ENDIF
127500961122     C**
127600961122     C* AGGIORNO L'EVENTO COL NUMERO DI SPEDIZIONE
127700961122     C                   Z-ADD     BLPAAS        EVSAAS
127800961122     C                   Z-ADD     BLPLNP        EVSLNP
127900961122     C                   Z-ADD     BLPNSP        EVSNSP
128000961122     C                   CLEAR                   EVSFTR
128100961122     C                   CLEAR                   EVSDTR
128200961122     C                   UPDATE    FNEVS000
128300961122     C*
128400961122     C     KEVS          READE     FNEVS02L                               33
128500961122    4C                   ENDDO
128600961122     C**
128700961122     C* AL 2 GIRO CERCO LE DISCOR.6  --> COLLO ARRIVATO MA A TERRA
128800020809    4C     XX            IFEQ      1
128900961122     C                   MOVEL     'E6 '         KCEV
129000961122    4C                   ENDIF
129100970602     C* AL 3 GIRO CERCO LE DISCOR.6B --> COLLO ARRIVATO MA IN PARTENZA
129200020809    4C     XX            IFEQ      2
129300970602     C                   MOVEL     'E6B'         KCEV
129400961122    4C                   ENDIF
129500961122    3C                   ENDDO
129600961122     C* SE TROVATA AGGIORNO IL DETTAGLIO COLLI
129700961122    3C     WDEV          IFGT      0
129800961122     C* SE ERA COLLO RIAMSTO A TERRA ARTDTR DEVE ESSERE = 0
129900970602    4C     WCEV          IFEQ      'E6 '
130000961122     C                   CLEAR                   ARTDTR
130100961122     C                   ELSE
130200961122     C* ALTRIMENTI IMPOSTO LA DATA DELL'EVENTO
130300961122     C                   Z-ADD     WDEV          ARTDTR
130400961122    4C                   ENDIF
130500961122     C* AGGIORNO RECORD
130600961122     C                   UPDATE    FNART001
130700961122     C* DISALLOCO RECORD
130800961122   X3C                   ELSE
130900070903     C                   update    FNART001
131000961122    3C                   ENDIF
131100961122     C*
131200961122    2C                   ENDIF
131300960529     C*
131400960529     C* LEGGO LA SPUNTA PARTENZA DEL COLLO
131500040317     C*  PER REPERIRE LA DATA DEL FOGLIO VIAGGIO PARTENZA, AI FINI DI
131600040317     C*  DETERMINARE IL TERMINAL DI ARRIVO DELLA LINEA DI ARRIVO
131700040317     c*  In realtà prendiamo la data della prima spunta partenza che
131800040317     c*  troviamo del collo, che potrebbe essere anche quella di un
131900040317     c*  p.o. di transito o di disguido. Ma sono casi molto rari ed
132000040317     c*  un problema si potrebbe presentare soltanto in caso di
132100040317     c*  cambio del terminal di arrivo della lna tra il momento in
132200040317     c*  cui parte/arriva il collo, ed il momento in cui
132300040317     c*  parte/arrva la bolla -vsito che siamo nel caso di manca rec-
132400040317     c*  con il terminal di arrivo calcolato richiama LS44 solo x le
132500040317     c*  spunte dell'area di arrivo, oper aggiornare la bolla in arr
132600040317     c*  Ma se cambia terminal in base alla data spunta le aree di arr
132700040317     c*  possono essere 2. allora si dovrebbe o duplicare dei controlli
132800040317     c*  che il pgm ls44 già fa, o richimare ls44 in modo che aggiorni
132900040317     c*  solo fnarb Ma data la eccezionalità dei casi, non facciamo
133000040317     c*  nulla e teniamo il pgm come è
133100040317     c*
133200960603     C                   EXSR      DFVPAR
133300961121     C*
133400960617     C* PRENDO IL TERMINAL DI ARRIVO DELLA LNA
133500020809     C                   CLEAR                   FNLV55DS
133600971114     C                   MOVEL     'A'           D55TPT
133700971114     C                   MOVEL     ARTLNP        D55LNP
133800971114     C                   MOVEL     ARTLNA        D55LIN
133900971114     C                   MOVEL     WDFV          D55DRF
134000971114     C                   CALL      'FNLV55R'
134100020809     C                   PARM                    FNLV55DS
134200971114     C                   MOVEL     'S'           WCAL55            1
134300971114     C**
134400971114     C     D55ERR        IFNE      ' '
134500971114     C                   MOVEL     D55LIN        D55TFA
134600971114     C                   ENDIF
134700971114     C                   MOVEL     D55TFA        LNATFA
134800950130     C*
134900950130     C* CERCO UNA SPUNTA DEL DETTAGLIO COLLO
135000070123     C     KSPP          CHAIN     FNBRV07L                           33
135100960529     C*
135200961121    2C     *IN33         DOWEQ     *OFF
135300960705     C*
135400960705     C* SE CATEGORIA 1 ESCLUDO SE IL FOGLIO E' UN NORMALE: ELABORO
135500960705     C*  SOLO SE E' DEFLUENZA
135600961121    3C     BRVNPG        IFEQ      1
135700960705     C                   CLEAR                   KEY
135800960705     C                   CLEAR                   WELAB
135900960705     C*
136000000110     C                   Z-ADD     BRVNFV        KNFV
136100040317     C                   Z-ADD     BRVfgs        Kfgs
136200020809     C     K02FGV01      chain     FNFGV01L                           36
136300031203     c     *in36         ifeq      *off
136400031203     C                   MOVEL     FGVTTR        WTTR
136500031203   X4C                   ELSE
136600960705     C     KFVA          CHAIN     FNFVA01L                           36
136700980623     C                   MOVEL     FVATTR        WTTR
136800031203    4C                   ENDIF
136900960705     C*
137000980623    4C     WTTR          IFNE      *BLANKS
137100980623     C     WTTR          LOOKUP    TDF                                    36
137200980623     C  N36              MOVEL     'N'           WELAB             1
137300961121    4C                   ENDIF
137400960705     C*
137500961121    3C                   ENDIF
137600960705     C*
137700960705     C** ELABORO LA CATEGORIA 1 SOLO PER LA DEFLUENZA E LE ALTRE CATEG
137800961121    3C     BRVNPG        IFNE      1
137900960705     C     WELAB         OREQ      ' '
138000960617     C**
138100960603     C* LEGGERE LE ECCEZIONI TERMINAL
138200960603     C**
138300020809     C                   CLEAR                   FNLV55DS
138400971114     C                   MOVEL     'A'           D55TPT
138500971114     C                   MOVEL     ARTLNP        D55LNP
138600031203     C                   MOVEL     BRVPES        D55LIN
138700971114     C                   MOVEL     WDFV          D55DRF
138800971114     C                   CALL      'FNLV55R'
138900020809     C                   PARM                    FNLV55DS
139000971114     C                   MOVEL     'S'           WCAL55            1
139100971114     C**
139200971114     C     D55ERR        IFNE      ' '
139300971114     C                   MOVEL     D55LIN        D55TFA
139400971114     C                   ENDIF
139500031203     C                   MOVEL     D55TFA        PESTFA
139600960529     C*
139700031203    5C     LNATFA        IFEQ      PESTFA
139800960603     C** IMPOSTO L'AS GESTIONE DELLA ANOMALIA
139900970114     C                   CLEAR                   OFV
140000960529     C                   EXSR      CHIAMO
140100970115     C*
140200960529    5C                   ENDIF
140300961121    3C                   ENDIF
140400960529     C**
140500070123     C     KSPP          READE     FNBRV07L                               33
140600961121    2C                   ENDDO
140700961121     C**
140800961121    1C                   ENDIF
140900950130     C*
141000950130     C                   ENDSR
141100960603     C**---------------------------------------------------------
141200960603     C*  LEGGO LA SPUNTA PARTENZA DEL COLLO PER PRENDERE LA DATA FV
141300960603     C**---------------------------------------------------------
141400960603     C     DFVPAR        BEGSR
141500960603     C                   CLEAR                   WDFV
141600070123     C     KSPP          CHAIN     FNBRV07L                           34
141700960603    1C     *IN34         DOWEQ     *OFF
141800960603    2C     BRVNPG        IFEQ      1
141900960603     C*
142000000110     C                   Z-ADD     BRVNFV        KNFV
142100040317     C                   Z-ADD     BRVfgs        Kfgs
142200020809     C     K02FGV01      chain     FNFGV01L                           35
142300960603     C**
142400960603     C* ESCLUDO I TRAINI DI DEFLUENZA
142500031216    3C     *IN35         IFEQ      *OFF
142600960603     C**
142700031216    5C     FGVTTR        IFNE      ' '
142800031216     C     FGVTTR        LOOKUP    TDF                                    35
142900031216    5C                   ENDIF
143000980623     C**
143100031216    5C     FGVTTR        IFEQ      ' '
143200031216     C     *IN35         OREQ      *OFF
143300980623     C**
143400031216    6C     FGVDFV        IFGT      WDFV
143500031216     C                   MOVEL     FGVDFV        WDFV
143600031216    6C                   ENDIF
143700031216    5C                   ENDIF
143800960603     C**
143900960603   X3C                   ELSE
144000960603     C**
144100960603     C     KFVA          CHAIN     FNFVA01L                           35
144200031216    4c     *in35         ifeq      *off
144300031216    5C     FVATTR        IFNE      ' '
144400031216     C     FVATTR        LOOKUP    TDF                                    35
144500031216    5C                   ENDIF
144600031216     C**
144700031216    5C     FVATTR        IFEQ      ' '
144800031216     C     *IN35         OREQ      *OFF
144900031216    6C     FVADFV        IFGT      WDFV
145000960603     C                   MOVEL     FVADFV        WDFV
145100031216    6C                   ENDIF
145200031216    5c                   endif
145300031216    4c                   endif
145400960603     C*
145500960603    3C                   ENDIF
145600960603    2C                   ENDIF
145700070123     C     KSPP          READE     FNBRV07L                               34
145800960603    1C                   ENDDO
145900960603     C**
146000960603     C* SE NON L'HO TROVATA PRENDO LA DATA DI CREAZIONE DELLA ANOMALIA
146100960603     C     WDFV          IFEQ      0
146200960603     C                   Z-ADD     ANMDAO        WDFV
146300960603     C                   ENDIF
146400960603     C                   ENDSR
146500950130     C**---------------------------------------------------------
146600950130     C** CHAIMO PGM DI AGGIORNAMENTO BOLLE DA SPUNTA
146700950130     C**---------------------------------------------------------
146800950130     C     CHIAMO        BEGSR
146900070228     c                   clear                   fnls45ds
147000070228     c                   movel     'S'           dls45ric
147100070228     c                   move      brvpes        dls45pes
147200070228     c                   move      simfel        dls45tfp
147300070228     c                   call      'FNLS45R'
147400070228     c                   parm                    fnls45ds
147500070228     c                   parm                    dsbrv
147600950130     C*
147700950130     C                   SETON                                        02
147800950130     C                   ENDSR
147900950123     C***--------------------------------------------------------
148000080411     C** invio msg in sede
148100911003     C**---------------------------------------------------------
148200911003     C     STAERR        BEGSR
148300080411     c* Mando msg a posta
148400080411     c                   clear                   tnsym5cds
148500080411     C* FILIALE A CUI INVIARE IL MESSAGGIO
148600080411     C                   MOVEL     046           ym5fde
148700080411     C* UTENTE  A CUI INVIARE IL MESSAGGIO
148800080411     C                   MOVEL     CUTI          ym5ude
148900080411     C                   MOVEL     MSG(1)        ym5tes
149000080411     c                   eval      %subst(ym5tes:26:3)=%editc(simfel:'X')
149100080411     c                   eval      %subst(ym5tes:35:10)=mbrfil
149200080411
149300080411     C                   MOVEL     BLPRSM        STARSM           20
149400080603     c     kblp2         chain     fnblp01l
149500080603     c                   if        not %found(fnblp01l)
149600080603     c                   clear                   w_blpctm
149700080603     c                   endif
149800080411
149900080411     c                   eval      ym5t01='Bolla: '+%editc(blpaas:'X') +' '
150000080411     c                             + %editc(blplnp:'X')+' '+%editc(blpnrs:'Z')
150100080411     c                             + ' '+%editc(blpnsp:'Z')+' LNA '
150200080411     c                             + %editc(blplna:'Z')+' Mit:'+ starsm
150300080603     c                   eval      ym5t02='Cod.Trattamento merce:' +W_blpctm
150400080411     c* Bolla già esistente
150500080603     c   06              eval      ym5t03=msg(2)
150600080411     c* non trovato dettaglio colli
150700080603     c   07              eval      ym5t03=msg(3)
150800080411     c* Segnacollo esistente
150900080411     c                   if        *in04
151000080603     c   08              eval      ym5t03=msg(4)
151100080603     c  N08              eval      ym5t03=msg(5)
151200080605     c                   eval      %subst(ym5T03:22:7)=%editc(artnsc:'Z')
151300080411     c                   endif
151400911003     C*
151500080603     c                   eval      ym5rig=3
151600080411     C*
151700080411     C                   CALL      'TNSYM5C'
151800080411     C                   PARM                    tnsym5cds
151900080411     C*
152000000622     C                   EXSR      DLTRIC
152100000622     C                   ENDSR
152200000622     C***--------------------------------------------------------
152300000622     C** DELETO RECORD DA FILE DI RICEZIONE PER BOLLA DOPPIA
152400000622     C**---------------------------------------------------------
152500000622     C     DLTRIC        BEGSR
152600921123     C*
152700921123     C* C A N C E L L O  SOLO SE BOLLA DOPPIA
152800921123     C   06              DO
152900950118     C                   DELETE    FNBLPE00
153000920917     C                   END
153100911003     C*
153200911003     C                   ENDSR
153300040317     C*
153400951011     C* CONTROLLO SE LOCALE SE DEVO AGGIORNARE LA DATA PARTENZA------*
153500031202     C     CTRDET        BEGSR
153600031202     C                   Z-ADD     1             KNPG
153700070123     C     KBRV9         CHAIN     FNBRV09L                           35
153800951011     C* TROVATA SPUNTA LOCALE PARTITA NON SUL FV IN ELABORAZIONE
153900980623     C*  --> AGGIORNO  SE NON SI TRATTA DI FOGLIO DEFLUENZA E SE
154000980623     C*  IL FOGLIO E' CHIUSO
154100031202    1C     *IN35         DOWEQ     *OFF
154200031202   1AC     BRVNFV        IFNE      PARNFV
154300041006     c* ignoro la spunta se fatta su foglio viaggio partenza errato
154400041006     c* (lna del collo non presente sul foglio viaggio)
154500041006     c     brvftr        andne     'L'
154600951011     C*
154700951011     C* DATA PARTENZA MERCE E NUMERO PISTOLA PARTENZA SOLO SE LOCALE
154800031202     C                   Z-ADD     BRVNFV        KNFV
154900040317     C                   Z-ADD     BRVfgs        Kfgs
155000031202     C     K02FGV01      chain     FNFGV01L                           35
155100031202    2C     FGVFCF        IFEQ      'S'
155200980623     C* CONTROLLO IL TIPO TRAINO
155300031202    3C     FGVTTR        IFNE      ' '
155400031202     C     FGVTTR        LOOKUP    TDF                                    36
155500031202    3C                   ENDIF
155600980623     C**
155700031202    3C     FGVTTR        IFEQ      ' '
155800031202     C     *IN36         OREQ      *OFF
155900031203     c                   clear                   flg15
156000031202     C                   CLEAR                   KPJBU
156100031216     c                   movel     param         paramx
156200031216     c                   z-add     brvnfv        parnfv
156300031216     c                   z-add     fgvdfv        pardtp
156400031202     C                   MOVEL     PARAM         KPJBU
156500031202     c                   call      'FNLR15R'
156600031202     c                   parm                    kpjba
156700031202     c                   parm                    dsbrv
156800031203     c                   parm                    flg15             1
156900031216     c                   movel     paramx        param
157000031202    3C                   ENDIF
157100031202    2C                   ENDIF
157200951011     C*
157300031202   1AC                   ENDIF
157400070123     C     KBRV9         READE     FNBRV09L                               35
157500031202    1C                   ENDDO
157600951011     C*
157700031202     C                   ENDSR
157800960826     C*
157900960827     C* CANCELLAZIONE ANOMALIA 50-COLLO ARRIVATO NON PARTITO --------*
158000960826     C     ANM050        BEGSR
158100960826     C                   Z-ADD     50            COMCAA
158200990510     C     KANM          CHAIN     FNANM000                           33
158300960826    1C     *IN33         IFEQ      *OFF
158400020809     C                   CLEAR                   FNLR33DS
158500960826     C                   Z-ADD     ANMNR2        D33NRR
158600960826     C                   MOVE      'S'           D33MAC
158700960826     C                   CALL      'FNLR33R'
158800020809     C                   PARM                    FNLR33DS
158900960826     C                   END
159000960826     C                   ENDSR
159100080411     c*----------------------------------------------------------------
159200911001**         CMD5
159300021008CHKOBJ OBJ(FNBLPE0R) OBJTYPE(*FILE)     MBR(M234567890)
159400911001**         CMD6
159500021008ALCOBJ OBJ((FNBLPE0R *FILE *EXCL           M234567890))
159600911002**         CMD9
159700021008OVRDBF FILE(FNBLPE0R)                   MBR(M234567890)
159800050804**         CmdDSPLOG
159900050804DSPJOBLOG OUTPUT(*PRINT)
160000000622**
160100080411RICEZIONE BOLLE ARRIVO A xxx: MBR Mxxxyyy                               1
160200080411Bolla già esistente                                                     2
160300080411Non trovato dettaglio colli                                             3
160400080411Segnacollo esistente xxxxxxx : SOSTITUITO                               4
160500080411Segnacollo esistente xxxxxxx : NON sostituito                           5
