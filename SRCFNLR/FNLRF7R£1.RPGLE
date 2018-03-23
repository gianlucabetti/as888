000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200030710      *--------------------------------------------------------------*
000300030908     FFNFVV02L  iF   E           K DISK
000400030708     FFNFVV01L  UF   E           K DISK    COMMIT
000500950524     F                                     RENAME(FNFVV000:FNFVV001)
000600070129     FFNBRV01L  IF   E           K DISK
000601110208     FFNBRV02L  IF   E           K DISK
000602110208     F                                     RENAME(FNbrv000:FNbrv002)
000700030721     FFNARB01L  uF   E           K DISK    commit
000800990921     FFIAR601L  IF   E           K DISK
000900950524     FFNART27L  IF   E           K DISK
001000950524     FFNLBL02L  IF   E           K DISK
001100950525     FAZCLN01L  IF   E           K DISK    USROPN
001300950525     FFNDST01L  IF   E           K DISK
001400041013     FFiqdt01L  IF   E           K DISK
001500021204     Ffiapd01L  IF   E           K DISK
001600950525     FFNARBY1L  UF A E           K DISK
001700030721     FFNARBZ0F  iF A E           K DISK
001800950526     FTABEL00F  IF   E           K DISK    USROPN
001900101124     FTntbe01l  IF   E           K DISK
002000101124     Fwfnlrq11l uf a E           k DISK
002100101118     Fwfnlrq20f o    E             DISK
002200030728     FFNLRF7P   O    E             PRINTER OFLIND(*IN80)
002300950612     F                                     USROPN
002400950524      *--------------------------------------------------------------*
002500950524      * Schiere
002600950524      *--------------------------------------------------------------*
002700950524     D CDA             S              2    DIM(10)
002800950525     D CDF             S              2    DIM(10)
002900950525     D MES             S              1    DIM(31)
003000951211     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
003100951211     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
003200020506     D £6              S              3  0 DIM(30)                              LINEE ARRIVI
003500101125     D der             S              1    DIM(26)                              FILIALI POSTE
003501110209     c* fogli ima del gg
003502110209     D ima             S              6  0 DIM(10)                              FILIALI POSTE
003600950524      *--------------------------------------------------------------*
003700950524      *   DS
003800950524      *--------------------------------------------------------------*
003900950525     D WLBDA8          DS                  INZ
004000950525     D  G02DAT                 1      8  0
004100950525     D  G02INV                 9     16  0
004200950525     D  G02ERR                17     17
004300950525     D  G02TGI                18     22  0
004301110208     D fnls46ds      e ds                  inz
004400040827     D OG143         E DS
004500950524     D KPJBA         E DS
004600101123     D dsqa          e ds
004700030721     D ddatiute      e ds
004800101123     D wfnlrq1ds     e ds                  extname(wfnlrq10F)
004900101123     D wfnlrq2ds     e ds                  extname(wfnlrq20F)
005000040713     D dsapd         e ds                  extname(fiapd00F)
005100040713     D dsapds        e ds                  extname(fiapd00F) prefix(s)
005200040713     D azuteds       e ds                  extname(AZUTE00F)
005300030718     D tibs34ds      E DS                  inz
005400081014     D fnlrv7ds      e ds                  inz
005500031017     c* ds parametri pgm fnlr39r blocco/sblocco
005600031017     d dslr02        e ds                  inz
005700031017     d trul06ds      e ds
005800020506     d  lin                    1     90  0 dim(30)
005900031017     c* ds passaggio parametri del pgm
006000950524     D FNLR59        E DS                  EXTNAME(FNLR59DS)
006100031017     c* ds passaggio parametri pgm stampa distinta
006200950524     D FNLR55        E DS                  EXTNAME(FNLR55DS)
006300951211     D DS3A          E DS
006400040713     D arbndcx         S              7    INZ
006500040713     D arbaasx         S              4    INZ
006600040713     D arbggx          S              2    INZ
006700040713     D arbmgsx         S              2    INZ
006800040713     D arbnrsx         S              2    INZ
006900040713     D arbnspx         S              7    INZ
007000040713     D arblnpx         S              3    INZ
007100040713     D DESBRV          S             90    INZ
007200030908     D savndd          S                   INZ like(d59ndd)
007300030908     D savnda          S                   INZ like(d59nda)
007400030728     D CMAT            C                   CONST('MATTINO   ')
007500030724     D CPOM            C                   CONST('POMERIGGIO')
007600040930     D                SDS
007700040930     D  NOMPGM                 1     10
007800950524      *--------------------------------------------------------------*
007900950524      * Ridenomino i campi dei codici anomalie in schiere
008000950524      *--------------------------------------------------------------*
008100950524     IFNARBY00
008200950525     I              ARBA01                      CDF(1)
008300950525     I              ARBA02                      CDF(2)
008400950525     I              ARBA03                      CDF(3)
008500950525     I              ARBA04                      CDF(4)
008600950525     I              ARBA05                      CDF(5)
008700950525     I              ARBA06                      CDF(6)
008800950525     I              ARBA07                      CDF(7)
008900950525     I              ARBA08                      CDF(8)
009000950525     I              ARBA09                      CDF(9)
009100950525     I              ARBA10                      CDF(10)
009200950524     C*--------------------------------------------------------------*
009300950524     C*  Ciclo principale
009400950524     C*--------------------------------------------------------------*
009500950613     C*  Imposto la chiave per il posizionamento
009600950524     C                   Z-ADD     4             KNPG
009700030722     C                   Z-ADD     D59nda        Knfv
009800030722     C                   Z-ADD     D59FGS        KFGS
009900031016      *  mi posiziono sulla distinta
010000030908     C     KFVV1         CHAIN(n)  FNFVV01L                           3132
010100030729     C                   IF        NOT *IN31  AND NOT *IN32
010200030909     C                             AND FVVDFV = D59DDT  and fvvfcf <> 'S'
010300030909     c                             and fvvatb <> '*'
010400030725     C                   EXSR      GESDIS
010500030725     C                   END
010600950911     C*  Richiamo pgm. per l'ultima volta x farli chiudere in LR
010700950911     C                   EXSR      CLOFIL
010800950525     C*
010900950524     C                   SETON                                        LR
011000030708     C*--------------------------------------------------------------*
011100030708     C*  Gestione singola distinta
011200030708     C*--------------------------------------------------------------*
011300030708     C     GESDIS        BEGSR
011400030721     c*
011500030718     c                   do
011600030718     C                   MOVEL     'N'           D59ANM
011700030718     C                   Z-ADD     FVVNFV        KNFV
011800041013     C                   Z-ADD     FVVdfv        Kdfv
011900030718     C                   Z-ADD     FVVNFV        KNFV1
012000041013     C                   Z-ADD     FVVNFV        KNFV2
012100030708     c* aggancio la distinta
012200030708     C     KDST          CHAIN     FNDST01L
012300030708     c                   if        not %found(fndst01l)
012400030708     c                   leave
012500030805     c                   else
012600041013     c* utilizzato il campo del numero delle bolle x bancali visto che non è
012700041013     c* + utilizzato in quanto le bolle vengono sempre ricalcolate
012800041013     C     Kqdt          CHAIN     Fiqdt01L
012900050207     c                   if        %found(fiqdt01l) and qdtatb = ' '
013000041013     c                   z-add     QDTNBNAF      d59nbl
013100041013     c                   else
013200041013     c                   z-add     dstnbn        d59nbl
013300050207     c                   clear                   QDTFNBAF
013400050207     c                   clear                   QDTNBNAF
013500030708     c                   end
013600041013     c                   end
013700030708     c* aggancio padroncino
013800030708     C     kapd          CHAIN     fiapd01L
013900030708     c                   if        not %found(fiapd01l)
014000030708     c                   leave
014100030708     c                   end
014200030909     C*  Richiamo pgm x allocare la distinta ed azzerare quelle posta
014300030909     c                   z-add     d59ndd        savndd
014400030909     c                   z-add     d59nda        savnda
014500030909     c                   z-add     fvvnfv        d59ndd
014600030909     c                   z-add     fvvnfv        d59nda
014700030909     C                   MOVEL(p)  FNLR59        KPJBU
014800030909     C                   CALL      'FNLRF7R1'
014900030909     C                   PARM                    KPJBA
015000030909     C                   MOVEL(p)  KPJBU         FNLR59
015100030909     c                   z-add     savndd        d59ndd
015200030909     c                   z-add     savnda        d59nda
015300030909     c* alloco la distinta
015400030909     C     Keyfvv        CHAIN     FNFVV01L                           3132
015500030909     C                   IF        NOT *IN31  AND NOT *IN32
015600030909     C                             AND FVVDFV = D59DDT and fvvfcf = '§'
015700950613     C                   Z-ADD     FVVNFV        D59NDT
015800030723     C                   MOVE      APDPDR        D59PDR
015900030723     C                   MOVE      FVVFPP        D59FPP
016000030723     C                   MOVEL     APDRSC        D59DSF
016100030721     c* STAMPO ANOMALIE PARTENDO DALLE BOLLE
016200030723     C                   MOVEL(p)  FNLR59        KPJBU
016300030718     C                   CALL      'FNLRF7R4'
016400030718     C                   PARM                    KPJBA
016500030723     c                   movel(p)  kpjbu         FNLR59
016600030722     C*  Richiamo pgm x calcolo stop spedizioni manuali
016700030721     C                   MOVEL(p)  FNLR59        KPJBU
016800030721     C                   CALL      'FNLRF7R3'
016900030721     C                   PARM                    KPJBA
017000030721     C                   MOVEL(p)  KPJBU         FNLR59
017100030718     C* scrivo errori in fnarby leggendo le spunte
017200950524     C                   EXSR      DETBRV
017300030801     c* se ho stampato anomalie da bolla salto 2 righe in stampa
017400101201     c                   if        d59lin <> 0  and
017500101201     c                             (d59sqa = ' ' or
017600101201     c                             d59sqa = 'T')
017700030801     c                   write     salto
017800030801     c                   add       2             lin
017900030801     c                   end
018000030718     C* stampo anomalie da spunte e metto eventuali spedizioni
018100030718     c* poste in distinta
018200030721     C                   MOVEL(p)  FNLR59        KPJBU
018300030703     C                   CALL      'FNLRF7R2'
018400950524     C                   PARM                    KPJBA
018500030721     C                   MOVEL(p)  KPJBU         FNLR59
018600030909     C* sblocca foglio e stampa
018700030718     c                   exsr      sblodis
018800030908     c                   end
018900030909     C                   ENDdo                                                  Trovate spunte
019000950524     C*
019100030718     C                   ENDSR
019200950524     C*--------------------------------------------------------------*
019300950524     C*  Dettaglio singola spunta
019400950524     C*--------------------------------------------------------------*
019500950524     C     DETBRV        BEGSR
019600030718     C*
019700070129     C     KBRV          setll     FNBRV01L
019800030718     C                   DO        *hival
019900070129     C     KBRV          READE     FNBRV01L
020000070129     c                   if        %eof(fnbrv01l)
020100030718     c                   leave
020200030718     c                   end
020201110208     c* cancella la spunta 3 o 4 + vecchia
020205110209     c                   if        kcdaz = 'LRQ1' and xi <> 0
020206110208     c                   exsr      srls46
020207110208     c* cancellata spunta 4
020208110208     c                   if        canc4 ='1'
020209110208     c                   iter
020210110208     c                   end
020211110208     c                   end
020300030718     C*
020400030718     C                   Z-ADD     0             X                 3 0
020500030718     C                   CLEAR                   FNARBY00
020600030718     C                   MOVEA     *BLANKS       CDA
020700030718     C*
020800950524     C*  E R R O R I      B L O C C A N T I  ! ! ! ! !
020900030718     C*
021000950609     C*  CONTROLLO SE SPARATO COLLO DI ALTRA FILIALE
021100950609     C     BRVLNA        LOOKUP    £6                                     39
021200950609     C     *IN39         IFEQ      '0'
021300030804     c                   exsr      srtesta
021400030728     C                   EVAL      DESBRV = 'SPUNTATO COLLO DI ALTRO P.O.'
021500040713     C                   EVAL      DESBol = '++++ +++ ++ +++++++  ++/++'
021502110414     c                   clear                   destsp
021600101201     c* stampa
021700101201     c                   if        d59sqa = ' ' or
021800101201     c                             d59sqa = 'T'
021900030728     C                   WRITE     ERR1
022000101201     c                   end
022100101201     c* video
022200101201     c                   if        d59sqa = 'S' or
022300101201     c                             d59sqa = 'T'
022400101123     c                   eval      tbeke2 = '001'
022500101118     c                   exsr      srsqa
022600101118     c                   end
022700030728     C                   ADD       1             D59LIN
022800030718     C                   iter
022900030718     c                   end
023000950524     C*  Mi posiziono dalle spunte sul dettaglio segnacolli
023100110121     C                   Z-ADD     BRVLNP        KFLS
023200110121     C                   Z-ADD     BRVLNA        KLNA
023300110121     C                   Z-ADD     BRVNRS        KNRSa
023400110121     C                   Z-ADD     BRVNSC        KNSC
023500030718     C     KART          CHAIN     FNART27L
023600030718     C                   IF        not %found(fnart27l)
023700030804     c                   exsr      srtesta
023800030728     C                   EVAL      DESBRV = 'MANCA RECORD BOLLA'
023900040713     C                   EVAL      DESBol = '++++ +++ ++ +++++++  ++/++'
023901110414     c                   clear                   destsp
024000101124     c* stampa
024100101201     c                   if        d59sqa = ' ' or
024200101201     c                             d59sqa = 'T'
024300101124     C                   WRITE     ERR1
024400101201     c                   end
024500101124     c* video
024600101201     c                   if        d59sqa = 'S' or
024700101201     c                             d59sqa = 'T'
024800101123     c                   eval      tbeke2 = '002'
024900101118     c                   exsr      srsqa
025000101118     c                   end
025100030728     C                   ADD       1             D59LIN
025200030718     C                   iter
025300030718     c                   end
025400030718     C* Se i dati della spedizione sono uguali salto controlli xchè già
025500040714     c* eseguiti
025600030718     C     WNSP          IFeq      ARTNSP
025700030718     C     WNRS          andeq     ARTNRS
025800030718     C     WLNP          andeq     ARTLNP
025900030718     C     WAAS          andeq     ARTAAS
026000040714     c                   exsr      srerr03
026100030718     c                   iter
026200081107     c                   else
026300081107     c                   clear                   flgdoppi          1 0
026400030718     c                   end
026500040714     C*  a cambio spedizione
026600040714     C                   Z-ADD     ARTAAS        WAAS
026700950529     C                   Z-ADD     ARTLNP        WLNP
026800950529     C                   Z-ADD     ARTNRS        WNRS
026900950529     C                   Z-ADD     ARTNSP        WNSP
027000040714     C                   Z-ADD     ARTAAS        KAAS
027100040714     C                   Z-ADD     ARTLNP        KLNP
027200040714     C                   Z-ADD     ARTNRS        KNRS
027300040714     C                   Z-ADD     ARTNSP        KNSP
027400040714     C     KARB          CHAIN     FNARB01L                           36
027500950530     C                   MOVEL     ARBFBC        OLDFBC
027600950524     C*  Manca bolla
027700950525     C     *IN36         IFEQ      '1'
027800950525     C                   Z-ADD     ARTAAS        ARBAAS
027900950525     C                   Z-ADD     ARTLNP        ARBLNP
028000950525     C                   Z-ADD     ARTNRS        ARBNRS
028100950525     C                   Z-ADD     ARTNSP        ARBNSP
028200950524     C                   ADD       1             X
028300950524     C                   MOVEL     '01'          CDA(X)
028400030718     C                   exsr      sragg
028500030718     c                   iter
028600950524     C                   END
028700081014     c* per prima memorizzo segnazione di probabile etichetta doppia
028800081107     c                   exsr      srdoppi
028900081014     c* più di 3 ricorrenze trovate di cui almeno una anche in partenza
029000090615     c                   if        OLRV7NRIC >= 3 and OLRV7PART ='S'
029100081014     C                   ADD       1             X
029200081014     C                   MOVEL     '17'          CDA(X)
029300081014     C                   exsr      sragg
029400081107     c                   eval      flgdoppi = 1
029500081014     C                   END
029600040827     c* dal 15/8/04 le sped.poste vengono immesse manualmente eccetto SDA
029700040827     c                   movel     arbaas        arbdsp            8 0
029800040827     c                   move      arbmgs        arbdsp
029900030721     C*  Bolla già consegnata
030000030721     C     ARBDCM        IFNE      0
030100030721     C                   ADD       1             X
030200030721     C                   MOVEL     '02'          CDA(X)
030300030721     C     X             IFEQ      10
030400030721     C                   exsr      sragg
030500030721     c                   iter
030600030721     C                   END
030700030721     C                   END
030800950524     C*  Controllo se spedizione mamma
030900950524     C                   Z-ADD     ARTAAS        KAAP
031000950524     C                   Z-ADD     ARTLNP        KLPP
031100950524     C                   Z-ADD     ARTNRS        KNRP
031200950529     C                   Z-ADD     ARTNSP        KNSP1
031300950524     C                   MOVEL     'N'           WTRBOL            1
031400950529     C                   MOVEL     'N'           WBOLOK            1
031500950524     C     KLBL          SETGT     FNLBL02L
031600950524     C     KLBL          READPE    FNLBL02L                               34
031700950524     C*  Se lo è cerco l'ultima bolla figlia sbloccata da consegnare
031800950524     C     *IN34         IFEQ      '0'
031900950524     C                   EXSR      CHKFGL
032000950524     C                   END
032100950524     C*  Se esistono figlie ma sono tutte bloccate emetto msg. errore
032200950524     C     WTRBOL        IFEQ      'S'
032300950529     C     WBOLOK        ANDEQ     'N'
032400950615     C                   MOVEL     *BLANKS       OLDFBC
032500950524     C                   ADD       1             X
032600950615     C*  SE NON HO TROVATO FIGLIE MI RIPOSIZIONO SU MAMMA
032700950615     C     *IN36         IFEQ      '1'
032800950615     C                   MOVEL     '14'          CDA(X)
032900950615     C                   Z-ADD     ARTAAS        KAAS
033000950615     C                   Z-ADD     ARTLNP        KLNP
033100950615     C                   Z-ADD     ARTNRS        KNRS
033200950615     C                   Z-ADD     ARTNSP        KNSP
033300950615     C     KARB          CHAIN     FNARB01L                           36
033400040827     c* dal 15/8/04 le sped.poste vengono immesse manualmente eccetto SDA
033500040827     c                   movel     arbaas        arbdsp            8 0
033600040827     c                   move      arbmgs        arbdsp
033700950615     C                   ELSE
033800030923     C     ARBFBC        IFNE      *BLANKS
033900950615     C                   MOVEL     '04'          CDA(X)
034000950615     C                   END
034100030923     C                   END
034200950524     C     X             IFEQ      10
034300030718     C                   exsr      sragg
034400030718     c                   iter
034500950524     C                   END
034600950524     C                   END
034700030912     C*  Se bolla non legata ...
034800950524     C     WTRBOL        IFEQ      'N'
034900070530     C*  Bolla non in distinta errore solo se sono nel pgm di "quadratura"
035000070530     C                   IF        arbndc = 0  and
035100101216     C*                            (kcdaz = 'LRF6' or kcdaz = 'LRF7'
035200101216     C*                            or kcdaz = 'LRF0' or kcdaz = 'LRF4')
035300101216     c                             kcdaz<>'LR59' and kcdaz<>'LR60'
035400030912     C                   ADD       1             X
035500040415     C                   MOVEL     '15'          CDA(X)
035600030912     C     X             IFEQ      10
035700030912     C                   exsr      sragg
035800030912     c                   iter
035900030912     C                   END
036000030912     C                   END
036100030912     C*  Se bolla non legata controllo se bloccata
036200030912     C     ARBFBC        ifne      *BLANKS
036300950525     C                   MOVEL     'N'           WFIGL             1            bolla figlia
036400950524     C                   EXSR      SBLOCC
036500950524     C     ARBFBC        IFNE      *BLANKS
036600950530     C                   MOVEL     *BLANKS       OLDFBC
036700950530     C                   ADD       1             X
036800950524     C                   MOVEL     '04'          CDA(X)
036900950524     C     X             IFEQ      10
037000030718     C                   exsr      sragg
037100030718     c                   iter
037200950524     C                   END
037300030912     C                   END
037400950524     C                   END
037500950524     C                   END
037600040714     C*  Bolla in consegna su altra distinta (non memorizzo + l'errore 03
037700040714     c*  ma stampo subito)
037800040714     C     ARBNDC        IFNE      0
037900040714     C     ARBNDC        ANDNE     FVVNFV
038000040714     C     ARBDCM        ANDEQ     0
038100040714     c                   exsr      srerr03
038200040714     c                   unlock    fnarb01l
038300040714     c                   iter
038400040714     C                   END
038500040713     C*                  END
038600950524     C*  Controllo se porto assegnato non tassato
038700950524     C     ARBACA        IFNE      'S'
038800950524     C                   ADD       1             X
038900950524     C                   MOVEL     '05'          CDA(X)
039000950524     C     X             IFEQ      10
039100030718     C                   exsr      sragg
039200030718     c                   iter
039300950524     C                   END
039400950524     C                   END
039500030718     C*
039600031016     C*  Se non ci sono errori bloccanti procedo al calcolo degli stop.
039700070530     C*  solo se pgm "da spunte"
039800070530     c                   if        arbngd = 0 and
039900070530     c                             (kcdaz='LR59' or kcdaz='LR60')
040000950526     C                   EXSR      CTRSTP
040100030723     c                   end
040200030718     C*
040300950524     C*  E R R O R I    N O N    B L O C C A N T I  ! ! ! ! !
040400030718     C*
040500950525     C     OLDFBC        IFNE      *BLANK
040600950524     C     ARBFBC        ANDEQ     *BLANK
040700950524     C                   ADD       1             X
040800950524     C                   MOVEL     '06'          CDA(X)
040900950524     C     X             IFEQ      10
041000030718     C                   exsr      sragg
041100030718     c                   iter
041200950524     C                   END
041300950524     C                   END
041400950524     C*  Spedizione da abilitare all'incasso C/ass.
041500950524     C     ARBACC        IFNE      'S'
041600950524     C                   MOVEL     'S'           ARBACC
041700950524     C                   ADD       1             X
041800950524     C                   MOVEL     '07'          CDA(X)
041900950524     C     X             IFEQ      10
042000030718     C                   exsr      sragg
042100030718     c                   iter
042200950524     C                   END
042300950524     C                   END
042400031110     C* Spedizione da abilitare alla consegna merce
042500070530     C* aggiorno ARBAMA solo se sto lanciando "da spunte"
042600950524     C     ARBAMA        IFEQ      0
042700950524     C                   Z-ADD     3             ARBAMA
042800070530     c                   if        arbngd = 0 and
042900070530     c                             (kcdaz='LR59' or kcdaz='LR60')
043000070530     c                   else
043100031110     C                   ADD       1             X
043200031110     C                   MOVEL     '08'          CDA(X)
043300031110     C     X             IFEQ      10
043400031110     C                   exsr      sragg
043500031110     c                   iter
043600031110     C                   END
043700031110     C                   END
043800950524     C                   END
043900950524     C*  Consegnata sped. fermo deposito con padroncino no 9999
044000950524     C     ARBFFD        IFEQ      'S'
044100051108     C     ARBFFD        oreq      'M'
044200950602     C     APDFCM        IFNE      'S'
044300950524     C                   ADD       1             X
044400950524     C                   MOVEL     '09'          CDA(X)
044500950524     C     X             IFEQ      10
044600030718     C                   exsr      sragg
044700030718     c                   iter
044800950524     C                   END
044900950524     C                   END
045000950524     C                   END
045100950524     C*  Controllo se spedizione consegnata in anticipo
045200950524     C     ARBDCR        IFGE      D59DDT
045300950524     C     ARBTCR        ANDEQ     'D'
045400950524     C     ARBDCR        ORGT      D59DDT
045500950524     C     ARBTCR        ANDEQ     ' '
045600950524     C                   ADD       1             X
045700950524     C                   MOVEL     '10'          CDA(X)
045800950524     C     X             IFEQ      10
045900030718     C                   exsr      sragg
046000030718     c                   iter
046100950524     C                   END
046200950524     C                   END
046300950524     C*  Calcolo turno chiusura
046400950524     C                   EXSR      TURNO
046500030725     C*  aggiorno fnarb SE POSTE e fnarby
046600030718     C                   EXSR      sragg
046700950529     C*
046800030718     C                   ENDdo
046900030718     C*
047000950529     C                   ENDSR
047100030721     C*--------------------------------------------------------------*
047200081014     C*  bolla legata ad altra distinta
047300030721     C*--------------------------------------------------------------*
047400040714     C     srerr03       BEGSR
047500081107     c*
047600081107     c                   exsr      srdoppi
047700040714     c*
047800040714     C     ARBNDC        IFNE      0
047900040714     C     ARBNDC        ANDNE     FVVNFV
048000040714     C     ARBDCM        ANDEQ     0
048100040714     C     *in36         ANDEQ     *off
048200040714     c                   exsr      srtesta
048300040714     C                   EVAL      DESBRV = 'COLLO SPUNTATO DI DISTINTA NR.'
048400040714     C                   EVAL      arbndcx=%editc(arbndc:'Z')
048500040714     C                   EVAL      %subst(desbrv: 31: 7)=arbndcx
048600040714     C                   EVAL      %subst(desbrv: 39: 6)='PADR. '
048700040714     c* salvo i dati dell'autista della distinta che sto quadrando
048800040714     c* per non sporcarli
048900040714     c                   eval      dsapds = dsapd
049000040714     c     kapd2         chain     fiapd01l
049100040714     C                   EVAL      %subst(desbrv: 45: 30)=apdrsc
049200040714     c                   eval      dsapd = dsapds
049300040714     c* riferimenti bolla prendo quelli del file FNART xchè i riferimenti
049400040714     c* spedizione del file FNARB la seconda volta sono puliti x effetto
049500040714     c* della clear di fnarby0f
049600040714     C                   movel     artaas        arbaasx
049700040714     C                   EVAL      %subst(desbol: 1: 4)=arbaasx
049800040714     C                   EVAL      arblnpx=%editc(artlnp:'Z')
049900040714     C                   EVAL      %subst(desbol: 6: 3)=arblnpx
050000040714     C                   movel     artnrs        arbnrsx
050100040714     C                   EVAL      %subst(desbol: 10: 2)=arbnrsx
050200040714     C                   EVAL      arbnspx=%editc(artnsp:'Z')
050300040714     C                   EVAL      %subst(desbol: 13: 7)=arbnspx
050400040714     C                   EVAL      arbggx=%subst(%editc(arbmgs:'Z'): 3: 2)
050500040714     C                   EVAL      %subst(desbol: 22: 2)= arbggx
050600040714     C                   EVAL      %subst(desbol: 24: 1)='/'
050700040714     C                   movel     arbmgs        arbmgsx
050800040714     C                   EVAL      %subst(desbol: 25: 2)=arbmgsx
050801110414     c                   eval      destsp = arbtsp
050900101124     c* stampa
051000101201     c                   if        d59sqa = ' ' or
051100101201     c                             d59sqa = 'T'
051200101124     C                   WRITE     ERR1
051300101201     c                   end
051400101124     c* video
051500101201     c                   if        d59sqa = 'S' or
051600101201     c                             d59sqa = 'T'
051700101123     c                   eval      tbeke2 = '003'
051800101118     c                   exsr      srsqa
051900101118     c                   end
052000040714     C                   ADD       1             D59LIN
052100081107     c* più di 3 ricorrenze trovate di cui almeno una anche in partenza
052200090615     c                   if        OLRV7NRIC >= 3 and OLRV7PART ='S'
052300081014     c                   exsr      srtesta
052400081014     C                   EVAL      DESBRV = 'COLLO DOPPIO'
052500101124     c* stampa
052600101201     c                   if        d59sqa = ' ' or
052700101201     c                             d59sqa = 'T'
052800081014     C                   WRITE     ERR1
052900101201     c                   end
053000101124     c* video
053100101201     c                   if        d59sqa = 'S' or
053200101201     c                             d59sqa = 'T'
053300101123     c                   eval      tbeke2 = '004'
053400101118     c                   exsr      srsqa
053500101118     c                   end
053600081014     C                   ADD       1             D59LIN
053700040714     C                   END
053800081107     C                   END
053900081107     c* se stessa distinta e stessa spedizione memorizzo l'anomalia
054000081107     c* di collo doppio se non già segnalata nei colli precedenti
054100081107     c                   if        not *in36 and flgdoppi = 0
054200081107     c                             and arbdcm = 0 and arbndc = fvvnfv
054300090615     c                             and OLRV7NRIC >= 3 and OLRV7PART ='S'
054400081107     C                   ADD       1             X
054500081107     C                   MOVEL     '17'          CDA(X)
054600100105     c* rileggo FNARB: se sono nel pgm "da spunte" perchè posso averlo
054700100105     c* rilasciato
054800100105     c                   if        arbngd = 0 and
054900100105     c                             (kcdaz='LR59' or kcdaz='LR60')
055000100105     C                   Z-ADD     ARtAAS        KAAS
055100100105     C                   Z-ADD     ARtLNP        KLNP
055200100105     C                   Z-ADD     ARtNRS        KNRS
055300100105     C                   Z-ADD     ARtNSP        KNSP
055400100105     C     KARB          CHAIN     FNARB01L                           36
055500100105     c                   end
055600081107     C                   exsr      sragg
055700081107     c                   eval      flgdoppi = 1
055800081107     c                   end
055900040714     c*
056000040714     C                   ENDSR
056100081014     C*--------------------------------------------------------------*
056200081014     C*  Cerco figlia
056300081014     C*--------------------------------------------------------------*
056400081107     c     srdoppi       BEGSR
056500081107     c*
056600081107     c* verifico eventuali etichette doppie e stampo subito
056700081107     c                   clear                   fnlrv7ds
056800081107     c                   eval      ILRV7FLS=artfls
056900081107     c                   eval      ILRV7LNA=artlna
057000081107     c                   eval      ILRV7NRS=artnrs
057100081107     c                   eval      ILRV7NSC=artnsc
057200081107     c                   movel(p)  fnlrv7ds      kpjbu
057300081107     c                   call      'FNLRV7R'
057400081107     c                   parm                    kpjba
057500081107     c                   movel     kpjbu         fnlrv7ds
057600081107     c*
057700081107     C                   ENDSR
057800081107     C*--------------------------------------------------------------*
057900081107     C*  Cerco figlia
058000081107     C*--------------------------------------------------------------*
058100081107     C     CHKFGL        BEGSR
058200030721     C*  Se bolla con C/assegno controllo se da abilitare
058300030721     C     WBOLOK        DOWEQ     'N'
058400030721     C     *IN34         ANDEQ     '0'
058500030721     C     *IN36         ANDEQ     '0'
058600030721     C     LBLLAP        IFEQ      LBLLAN
058700030721     C                   MOVEL     'S'           WTRBOL
058800030721     C                   Z-ADD     LBLAAN        KAAS
058900030721     C                   Z-ADD     LBLLPN        KLNP
059000030721     C                   Z-ADD     LBLNRN        KNRS
059100030721     C                   Z-ADD     LBLNSN        KNSP
059200030721     C     KARB          CHAIN     FNARB01L                           36
059300030721     C     *IN36         IFEQ      '0'
059400040827     c* dal 15/8/04 le sped.poste vengono immesse manualmente eccetto SDA
059500040827     c                   movel     arbaas        arbdsp            8 0
059600040827     c                   move      arbmgs        arbdsp
059700070530     C*  Bolla non in distinta errore solo se sono nel pgm di "quadratura"
059800070530     C                   IF        arbndc = 0 and
059900031103     C                             (kcdaz = 'LRF6' or kcdaz = 'LRF7'
060000040426     C                              or kcdaz = 'LRF0' or kcdaz = 'LRF4')
060100030721     C                   ADD       1             X
060200030721     C                   MOVEL     '15'          CDA(X)
060300030721     C     X             IFEQ      10
060400030721     C                   exsr      sragg
060500030721     C                   END
060600030721     C                   else
060700030721     C     ARBNDC        ifne      9999999
060800030721     C                   MOVEL     ARBFBC        OLDFBC
060900030721     C*  Se bolla bloccata controllo se sbloccabile
061000030721     C     ARBFBC        IFNE      *BLANKS
061100030721     C                   MOVEL     'S'           WFIGL                          bolla figlia
061200030721     C                   EXSR      SBLOCC
061300030721     C                   END
061400030721     C*  Se sbloccata metto in consegna figlia
061500030721     C     ARBFBC        IFEQ      ' '
061600030721     C                   MOVEL     'S'           WBOLOK
061700030721     C                   END
061800030721     C*
061900030721     C                   END
062000030721     C                   END
062100030721     C                   END
062200030721     C                   END
062300030721     C*  Se non trovata bolla loop su successiva
062400030721     C     WBOLOK        IFEQ      'N'
062500030721     C     KLBL          READPE    FNLBL02L                               34
062600030721     C                   END
062700030721     C*
062800030721     C                   ENDdo
062900030721     C*
063000030721     C                   ENDSR
063100030718     C*-----------------------------------------------------*
063200030718     C* aggiorna FNARB e metto in FNARBY anomalia           *
063300030718     C*-----------------------------------------------------*
063400030718     C     sragg         BEGSR
063500070530     c* aggioro FNARB: se sono nel pgm "da spunte"
063600081205     c* se il flg di colli doppi è valorizzato a 1 vuol dire che ho già
063700081205     c* aggiornato la spedizione e sono sul dettaglio dei sui colli
063800030722     c                   if        not *in36
063900081205     c                   if        arbngd = 0 and flgdoppi = 0 and
064000070530     c                             (kcdaz='LR59' or kcdaz='LR60')
064100030722     C                   EXCEPT    AGGARB
064200030722     c                   else
064300030722     c                   unlock    fnarb01l
064400030722     c                   end
064500030722     c                   end
064600030722     c*
064700030718     C     KARB          CHAIN     FNARBY1L                           33
064800030718     C                   MOVEL     CDA           CDF
064900030718     C   33              WRITE     FNARBY00
065000030718     C  N33              UPDATE    FNARBY00
065100030722     c*
065200030718     C                   ENDSR
065300950526     C*-----------------------------------------------------*
065400101123     C* memorizzo l'errore nel file di work                 *
065500950526     C*-----------------------------------------------------*
065600101123     C     srsqa         BEGSR
065700101123     c                   clear                   dsqa
065800101123     c     ktbe          chain     tntbe01l
065900101123     c                   if        %found(tntbe01l)
066000101124     c                   movel     tbeuni        dsqa
066100101123     c                   if        D§SQAELA = 'S'
066200101124     c* TESTATA
066300101123     c                   clear                   wfnlrq1ds
066400101125     c                   eval      wlrq1ndc = d59ndt
066500101125     c                   eval      wlrq1fgs = d59fgs
066600101125     c                   eval      wlrq1pdr = d59pdr
066700101123     c* se ho i rif. della spedizione li memorizzo altrimenti 0
066800101125     c                   movea     desbol        der
066900101125     c                   do        26            xx                2 0
067000101125     C                   if        der(xx) = ' '
067100101125     c                   eval      der(xx) ='0'
067200101125     c                   end
067300101125     c                   enddo
067400101126     c                   movea     der           descom           26
067500101125     c*
067600101126     c                   eval      comaas = %subst(descom: 1: 4)
067700101124     c                   testn                   comaas            4  99
067800101124     c   99              movel     comaas        wlrq1aas
067900101126     c                   eval      comlnp = %subst(descom: 6: 3)
068000101124     c                   testn                   comlnp            3  99
068100101124     c   99              movel     comlnp        wlrq1lnp
068200101126     c                   eval      comnrs = %subst(descom: 10: 2)
068300101124     c                   testn                   comnrs            2  99
068400101124     c   99              movel     comnrs        wlrq1nrs
068500101126     c                   eval      comnsp = %subst(descom: 13: 7)
068600101124     c                   testn                   comnsp            7  99
068700101124     c   99              movel     comnsp        wlrq1nsp
068800101124     c*
068900101124     c     kwf10         chain     wfnlrq11l
069000101124     c                   if        not %found(wfnlrq11l)
069100101124     c                   movel     tbeke2        wlrq1cer
069200101124     c                   eval      wlrq1sce = D§SQASCE
069300101123     c                   eval      wlrq1err = desbrv
069400101124     c                   write     wfnlrq10
069500101124     c                   else
069600101124     c                   eval      wlrq1err = 'Errore dettaglio colli'
069700101124     c                   clear                   wlrq1cer
069800101124     c                   clear                   wlrq1sce
069900101124     c                   update    wfnlrq10
070000101124     c                   end
070100101124     c* dettaglio
070200101124     c                   clear                   wfnlrq2ds
070300101124     c* se ho i rif. della spedizione li memorizzo altrimenti 0
070400101124     c                   eval      wlrq2aas = wlrq1aas
070500101124     c                   eval      wlrq2lnp = wlrq1lnp
070600101124     c                   eval      wlrq2nrs = wlrq1nrs
070700101124     c                   eval      wlrq2nsp = wlrq1nsp
070800101124     c                   eval      wlrq2ndc = wlrq1ndc
070900101124     c                   eval      wlrq2fgs = wlrq1fgs
071000101124     c                   eval      wlrq2pdr = wlrq1pdr
071100101124     c                   movel     tbeke2        wlrq2cer
071200101124     c                   eval      wlrq2sce = D§SQASCE
071300101124     c                   eval      wlrq2err = desbrv
071301110104     c                   eval      wlrq2fls = brvlnp
071400101124     c                   eval      wlrq2nsc = brvnsc
071401110126     c                   eval      wlrq2nrc = brvnrs
071402110131     c                   if        tbeke2 = '003'
071501110104     c                   eval      wlrq2flv = brvfgs
071600101126     c                   eval      wlrq2nfv = brvnfv
071601110117     c                   eval      wlrq2dfv = D59DDT
071700101126     c                   eval      wlrq2npg = brvnpg
071800101126     c                   eval      wlrq2hms = brvhms
071900101126     c                   eval      wlrq2nps = brvnps
071901110131     c                   end
072000101124     c                   write     wfnlrq20
072100101123     c                   end
072200101124     c                   end
072300101123     C                   ENDSR
072400101123     C*-----------------------------------------------------*
072500101123     C* Gestione eventuale attribuzione Stop                *
072600101123     C*-----------------------------------------------------*
072700101123     C     CTRSTP        BEGSR
072800951121     C                   MOVEL     *BLANKS       KRAG
072900951211     C                   Z-ADD     0             KKSC              7 0
073000951211     C                   Z-ADD     0             ARBKCS
073100951211     C*  Controllo se devo attribuire lo Stop
073200951211     C                   Z-ADD     1             Y                 2 0
073300951211     C                   MOVEL     *BLANKS       WTBOL1            1
073400951211     C     ARBCBO        LOOKUP    C3A(Y)                                 30
073500951211     C     *IN30         IFEQ      '1'
073600951211     C                   MOVEL     D3A(Y)        DS3A
073700951211     C                   MOVEL     §3ATB1        WTBOL1
073800951211     C     WTBOL1        IFEQ      'A'
073900951211     C                   Z-ADD     ARBKSC        KKSC
074000951211     C                   END
074100951211     C                   END
074200951211     C*
074300951211     C     *IN30         IFEQ      '1'
074400990921     C     WTBOL1        IFEQ      'A'
074500990921     C     §3ATB2        ORNE      *BLANKS
074600990921     C     KARB          CHAIN     FIAR601L                           30
074700990921     C  N30              Z-ADD     AR6KSC        KKSC
074800951211     C                   END
074900951211     C                   END
075000951211     C*  Controllo se cliente no generico e se si attribuisco stop
075100951211     C                   Z-ADD     KKSC          APPOG             4 0
075200951211     C     KKSC          IFNE      0
075300951211     C     APPOG         ANDNE     9999
075400951211     C     APPOG         ANDNE     8888
075500951211     C                   Z-ADD     KKSC          ARBKCS
075600951211     C                   MOVEL     KKSC          KRAG
075700951211     C                   ELSE
075800951211     C*  Se no assegnato codificato lavoro con rag.sociale/indirizzo
075900951121     C                   MOVEL     ARBRSD        KRAG
076000951121     C                   MOVE      ARBIND        KRAG
076100951211     C                   END
076200951211     C*  Attribuisco lo Stop
076300951121     C     KRAG          CHAIN     FNARBZ0F                           30
076400951121     C*  Controllo se cliente esiste già
076500950526     C     *IN30         IFEQ      '1'
076600950526     C                   ADD       1             D59STO
076700950526     C                   Z-ADD     D59STO        ARBSTP
076800951121     C                   MOVEL     KRAG          ARBRAG
076900950526     C                   WRITE     FNARBZ00
077000950526     C                   END
077100950526     C*
077200950526     C                   ENDSR
077300950525     C*--------------------------------------------------------------*
077400950525     C*  Sblocco spedizione se possibile
077500950525     C*--------------------------------------------------------------*
077600950525     C     SBLOCC        BEGSR
077700070530     c* sblocco FNARB: se pgm "da spunte"
077800070530     c                   if        arbngd = 0 and
077900070530     c                             (kcdaz='LR59' or kcdaz='LR60')
078000031017     c* devo rilasciare FNARB xchè l'aggiornamento dei campi
078100031017     c* ARBCMC e ARBFBC avviene all'interno del pgm fnlr39r
078200031017     c                   unlock    fnarb01l
078300031017     c                   clear                   dslr02
078400031017     C                   Z-ADD     arbaas        DR2AAS
078500031017     C                   Z-ADD     arbLNP        DR2LNP
078600031017     C                   Z-ADD     arbNRS        DR2NRS
078700031017     C                   Z-ADD     arbNSP        DR2NSP
078800040831     C                   Z-ADD     d59fgs        DR2FGS
078900031017     C                   MOVEL     '1'           DR2RIC
079000031017     C                   MOVEL     '1'           DR2comit
079100031017     C                   MOVEL     'N'           DR2vid
079200031017     C                   MOVEL     'N'           DR2pos
079300031017     C                   MOVEL     DSLR02        KPJBU
079400031017     C                   CALL      'FNLR39R'
079500031017     C                   PARM                    KPJBA
079600031017     c                   movel     kpjbu         dslr02
079700031017     c     karb          chain     fnarb01l
079800031016     c                   end
079900950529     C*
080000030704     C                   ENDSR
080100950525     C*--------------------------------------------------------------*
080200950525     C*  Gestisco turno di chiusura
080300950525     C*--------------------------------------------------------------*
080400950525     C     TURNO         BEGSR
080500950525     C*
080600950525     C     ARBGC1        IFNE      *BLANKS
080700950525     C                   MOVEL     ARBGC1        WGIOR             1
080800950525     C     WGIOR         IFEQ      WGG
080900950525     C                   ADD       1             X
081000950525     C                   MOVEL     '11'          CDA(X)
081100950531     C                   GOTO      FINTUR
081200950525     C                   END
081300950531     C                   END
081400950531     C*
081500950525     C     ARBGC2        IFNE      *BLANKS
081600950531     C                   MOVEL     ARBGC2        WGIOR             1
081700950525     C     WGIOR         IFEQ      WGG
081800950525     C                   ADD       1             X
081900950525     C                   MOVEL     '11'          CDA(X)
082000950525     C                   END
082100950525     C                   END
082200950525     C*
082300950531     C     FINTUR        ENDSR
082400030721     C*--------------------------------------------------------------*
082500030721     C*  disalloco la distinta e stampo totali
082600030721     C*--------------------------------------------------------------*
082700030721     C     sblodis       BEGSR
082800030721     C     FVVFCF        IFeq      '§'
082900030721     C                   MOVEL     ' '           FVVFCF
083000030908     C                   EXCEPT    AGGFL1
083100030908     c                   commit
083200030909     c                   end
083300030825     C* STAMPA DISTINTA
083400101201     c                   if        d59ndt = fvvnfv  and
083500101201     c                             (d59sqa = ' ' or
083600101201     c                             d59sqa = 'T')
083700030909     c*
083800030909     C                   CLEAR                   FNLR55
083900030909     c* stampa scritta DISTINTA CON ANOMALIE
084000050426     C                   eval      D55bcd = d59bcd
084100050426     C                   MOVEL     D59ANM        D55ANM
084200030723     c*
084300030909     C                   Z-ADD     d59ddt        D55DAT
084400030909     C                   Z-ADD     d59ndt        D55NRD
084500030909     C                   Z-ADD     d59ndt        D55NRA
084600040930     C                   Z-ADD     d59pag        D55pag
084700040930     C                   Z-ADD     d59lin        D55lin
084800040930     C                   eval      d55nwp = 'N'
084900050426     C                   eval      d55lsr = d59lsr
085000030822     C* Solo anomalie
085100030721     C                   MOVEL     'A'           D55TIP
085200050204     C     D59Stp        ifeq      'O'
085300030822     C                   MOVEL     'E'           D55STP
085400030822     C                   Else
085500030822     C                   MOVEL     'C'           D55STP
085600030822     C                   ENDIF
085700030721     C                   MOVEL     D59FGS        D55FIL
085800041013     C                   Z-ADD     D59NBl        D55NBN
085900050207     C                   MOVEL     QDTFNBAF      D55fba
086000050207     C                   MOVEL     DSTNOT        D55NOT
086100030721     c* stampa solo totali
086200030822     c                   if        d59stp = 'A'
086300030721     C                   MOVEL     'S'           D55tot
086400050204     c* stampa anche la firma dell'aut
086500050204     C                   MOVEL     'S'           D55fir
086600030822     c                   end
086700030721      *--
086800030721     C                   MOVEL(p)  FNLR55        KPJBU
086900030721     C                   CALL      'FNLR55R'
087000030721     C                   PARM                    KPJBA
087100030825     c                   end
087200030721     c                   endsr
087201110208     C*--------------------------------------------------------------*
087202110208     C*  cancella la spunta + vecchia tra la 3 e la 4 se sono richiamato
087203110208     c* dal controllo a video
087204110208     C*--------------------------------------------------------------*
087205110208     C     srls46        BEGSR
087206110208     c                   clear                   canc4             1
087207110209     c                   setoff                                       22
087208110208     c* controllo se ho la spunta 3
087209110209     c                   do        10            xx
087210110209     c                   if        ima(xx) <> 0
087211110208     c     kbrv2         setll     fnbrv02l                               22
087212110209     c                   if        *in22
087213110209     c                   leave
087214110209     c                   end
087215110209     c                   else
087216110209     c                   leave
087217110209     c                   end
087218110209     c                   enddo
087219110208     c* trovo anche la spunta 3
087220110208     c                   if        *in22
087221110208     c                   clear                   fnls46ds
087222110208     c                   clear                   kpjbu
087223110208     c                   eval      ILS46NPGA =  brvnpg
087224110208     c                   eval      ILS46NFVA =  brvnfv
087225110208     c                   eval      ILS46FGSA =  brvfgs
087226110208     c                   eval      ILS46NPGB =  3
087227110209     c                   eval      ILS46NFVB =  ima(xx)
087228110208     c                   eval      ILS46FGSB =  d59fgs
087229110208     c                   eval      ILS46AMB =  'QD'
087230110208     c                   eval      ILS46aupr=  'SE'
087231110208     c                   eval      ILS46FLS =  brvlnp
087232110208     c                   eval      ILS46LNA =  brvlna
087233110208     c                   eval      ILS46NRS =  brvnrs
087234110208     c                   eval      ILS46NSC =  brvnsc
087235110208     c                   eval      ILS46TELA = 'M'
087236110208     c*
087237110208     c                   call      'FNLS46R'
087238110208     c                   parm                    kpjba
087239110208     c                   parm                    fnls46ds
087240110208     c* verifico se cancellata la spunta 3 o 4
087241110208     c                   if        ols46err = ' '
087242110208     c     kbrv2         setll     fnbrv02l                               22
087243110208     c* trovo la spunta 3 quindi ho cancellato la 4
087244110208     c                   if        *in22
087245110208     c                   eval      canc4 = '1'
087246110208     c                   end
087247110208     c                   end
087248110208     c                   end
087249110208     c*
087250110208     C                   ENDSR
087300950911     C*--------------------------------------------------------------*
087400950911     C*  Richiamo pgm x ultima volta
087500950911     C*--------------------------------------------------------------*
087600950911     C     CLOFIL        BEGSR
087700950911     C*
087800950911     C                   MOVEL     'U'           D59TPR
087900950911     C                   MOVEL     'U'           D55TPR
088000030721     C                   MOVEL(p)  FNLR59        KPJBU
088100030708     C                   CALL      'FNLRF7R1'
088200950911     C                   PARM                    KPJBA
088300030723     C                   CALL      'FNLRF7R2'
088400950911     C                   PARM                    KPJBA
088500030723     C                   CALL      'FNLRF7R3'
088600950911     C                   PARM                    KPJBA
088700030723     C                   CALL      'FNLRF7R4'
088800030723     C                   PARM                    KPJBA
088900950911     C                   MOVEL     FNLR55        KPJBU
089000950911     C                   CALL      'FNLR55R'
089100950911     C                   PARM                    KPJBA
089200030911     c* devo commitare di nuovo perchè posso aver scritto degli eventi nic
089300030911     c* chiudendo fnlrf7r2
089400030911     c                   commit
089401110103     C                   MOVEL(p)  FNLR59        KPJBU
089500950911     C*
089600950911     C                   ENDSR
089700030721     C*-----------------------------------------------------*
089800030721     C* stampa testata                                      *
089900030721     C*-----------------------------------------------------*
090000030721     C     srtesta       BEGSR
090100030731     c                   if        d59lin = 0 or d59lin > 54
090200030728     C* AUTOTRASPORTATORE
090300030728     C                   Z-ADD     APDPDR        PRTPDR            7 0
090400030728     C                   MOVEL(P)  APDRSC        PRTDSF           35
090500050422     C                   MOVEL(P)  APDRSC        PRTDSF2
090600030728     C* DISTINTA
090700030728     C                   Z-ADD     FVVNFV        PRTNDT            5 0
090701110209     C                   Z-ADD     FVVNFV        PRTNDT3
090800110209     C*                  eval      PRTNDT2 = d59bcd
090801110209     C                   move      d59bcd        PRTNDT2
090900030728     C                   Z-ADD     FVVFGS        PRTFGS            3 0
091000030728     C                   Z-ADD     FVVDFV        G02INV
091100030728     C                   MOVEL     '3'           G02ERR
091200030728     C                   CALL      'XSRDA8'
091300030728     C                   PARM                    WLBDA8
091400030728     C                   Z-ADD     G02DAT        STCDDC            8 0
091500030728     C* TIPO PRESTAZIONE
091600030721     C                   MOVEL     'S'           D59ANM
091700030724     C                   MOVEL     CMAT          TPPRE
091800030724     C     FVVFPP        IFEQ      'P'
091900030724     C                   MOVEL     CPOM          TPPRE            10
092000030724     C                   END
092100030725     c*
092200030728     C                   ADD       1             D59PAG
092300101201     c                   if        d59sqa = ' ' or
092400101201     c                             d59sqa = 'T'
092500030728     C                   WRITE     TESTA
092600050502     c                   if        d59lsr = 'S' and d59pag = 1
092700050422     C                   write     testal
092800050426     c                   end
092900030729     C                   WRITE     TESTA1
093000101125     c                   end
093100030728     C                   SETOFF                                       80
093200030728     C                   Z-ADD     6             D59LIN
093300030731     c                   end
093400030721     C                   ENDSR
093500950525     C*--------------------------------------------------------------*
093600950525     C*  Operazioni iniziali
093700950525     C*--------------------------------------------------------------*
093800950525     C     *INZSR        BEGSR
093900950525     C*
094000950525     C     *ENTRY        PLIST
094100950525     C                   PARM                    KPJBA
094200030721     C                   MOVEL(P)  KPJBU         FNLR59
094300030718     c*
094400030718     c     *dtaara       define    §azute        azuteds
094500030718     c     *dtaara       define    §datiute      ddatiute
094600030718     C                   in(E)     *dtaara
094700030718     C                   IF        %Error  or  RSUT = *blanks
094800030718     C                   call      'TIBS34R'
094900030718     C                   parm                    Tibs34Ds
095000030718     C                   in        *dtaara
095100030718     c                   ENDIF
095200050204     C* IMPOSTO L'INDICATORE PER STAMPARE IL TITOLO IN BASE AL PARAMETRO
095300050204     C* TIPO STAMPA A=CONTROLLO AUTOTRASPORTATORE " "/O= DISTINTA DI USCITA
095400050204     C* AUTOTRASPORTATORE
095500050204     C                   IF        D59STP = 'A'
095600050204     C                   SETON                                        61
095700050204     C                   SETOFF                                       62
095800050204     C                   ELSE
095900050204     C                   SETON                                        62
096000050204     C                   SETOFF                                       61
096100050204     C                   END
096200101124     c                   eval      tbecod = 'SQA'
096300101124     c                   eval      tbeke1 = nompgm
096400950525     C*------------------------------------------------------*
096500950525     C*  Definisco chiavi di accesso
096600101123     C     Ktbe          KLIST
096700101124     C                   KFLD                    tbecod
096800101124     C                   KFLD                    tbeke1
096900101124     C                   KFLD                    tbeke2
097000101124     C     Kwf10         KLIST
097100101124     C                   KFLD                    WLRQ1AAS
097200101124     C                   KFLD                    WLRQ1LNP
097300101124     C                   KFLD                    WLRQ1NRS
097400101124     C                   KFLD                    WLRQ1NSP
097500101124     C                   KFLD                    WLRQ1FGS
097600101124     C                   KFLD                    WLRQ1NDC
097700101124     C                   KFLD                    WLRQ1PDR
097800101123     C     Keyfvv        KLIST
097900101123     C                   KFLD                    KNPG
098000101123     C                   KFLD                    fvvnfv
098100101123     C                   KFLD                    KFGS
098200030908     C     KFVV1         KLIST
098300030908     C                   KFLD                    KNPG
098400030908     C                   KFLD                    KNFV
098500030908     C                   KFLD                    KFGS
098501110208     C     KFVV2         KLIST
098502110209     C                   KFLD                    KNPG3
098503110208     C                   KFLD                    Kdfv
098504110208     C                   KFLD                    KFGS
098600950525     C     KDST          KLIST
098700950525     C                   KFLD                    KNFV
098800950525     C                   KFLD                    KFGS
098900041013     C     KqdT          KLIST
099000041013     C                   KFLD                    KFGS
099100041013     C                   KFLD                    KNFV2
099200041013     C                   KFLD                    Kdfv
099300950525     C     KBRV          KLIST
099400950525     C                   KFLD                    KNPG
099500000114     C                   KFLD                    KNFV1
099600950525     C                   KFLD                    KFGS
099601110208     C     KBRV2         KLIST
099602110209     C                   KFLD                    KNPG3
099603110209     C                   KFLD                    ima(xx)
099604110209     C                   KFLD                    kfgs
099605110208     C                   KFLD                    brvlna
099606110208     C                   KFLD                    brvlnp
099607110208     C                   KFLD                    brvnrs
099608110208     C                   KFLD                    brvnsc
099700950525     C     KART          KLIST
099800110121     C                   KFLD                    KFLS
099900110121     C                   KFLD                    KLNA
100000110121     C                   KFLD                    KNRSa
100100110121     C                   KFLD                    KNSC
100200950525     C     KARB          KLIST
100300950525     C                   KFLD                    KAAS
100400950525     C                   KFLD                    KLNP
100500950525     C                   KFLD                    KNRS
100600950525     C                   KFLD                    KNSP
100700950525     C     KLBL          KLIST
100800950525     C                   KFLD                    KAAP
100900950525     C                   KFLD                    KLPP
101000950525     C                   KFLD                    KNRP
101100950529     C                   KFLD                    KNSP1
101200950525     C     KCLN          KLIST
101300950525     C                   KFLD                    KTFP
101400950525     C                   KFLD                    KTFA
101500950525     C                   KFLD                    KANN
101600950525     C                   KFLD                    KMES
101700950526     C     KTAB          KLIST
101800950526     C                   KFLD                    KKUT
101900950526     C                   KFLD                    KCOD
102000021204     C     Kapd          KLIST
102100021204     C                   KFLD                    apdtip
102200021204     C                   KFLD                    dstpdr
102300021204     c                   movel     'A'           apdtip
102400040713     C     Kapd2         KLIST
102500040713     C                   KFLD                    apdtip
102600040713     C                   KFLD                    arbpdc
102700950525     C*------------------------------------------------------*
102800950525     C*  Definisco variabili
102900030728     C     *LIKE         DEFINE    FVVNPG        KNPG
103000950525     C     *LIKE         DEFINE    FVVNFV        KNFV
103100000114     C     *LIKE         DEFINE    BRVNFV        KNFV1
103200041013     C     *LIKE         DEFINE    qdtNFV        KNFV2
103300950525     C     *LIKE         DEFINE    FVVFGS        KFGS
103400950525     C     *LIKE         DEFINE    FVVDFV        KDFV
103500950525     C     *LIKE         DEFINE    ARTFLS        KFLS
103600950525     C     *LIKE         DEFINE    ARTLNA        KLNA
103700950525     C     *LIKE         DEFINE    ARTNRS        KNRS
103701110121     C     *LIKE         DEFINE    ARTNRS        KNRSa
103800950525     C     *LIKE         DEFINE    ARTNSC        KNSC
103900950525     C     *LIKE         DEFINE    ARTAAS        KAAS
104000950525     C     *LIKE         DEFINE    ARTLNP        KLNP
104100950525     C     *LIKE         DEFINE    ARTNSP        KNSP
104200950529     C     *LIKE         DEFINE    ARTNSP        KNSP1
104300950525     C     *LIKE         DEFINE    LBLAAP        KAAP
104400950525     C     *LIKE         DEFINE    LBLLPP        KLPP
104500950525     C     *LIKE         DEFINE    LBLNRP        KNRP
104600950525     C     *LIKE         DEFINE    CLNTFP        KTFP
104700950525     C     *LIKE         DEFINE    CLNTFA        KTFA
104800950525     C     *LIKE         DEFINE    CLNANN        KANN
104900950525     C     *LIKE         DEFINE    CLNMES        KMES
105000950526     C     *LIKE         DEFINE    TBLKUT        KKUT
105100950526     C     *LIKE         DEFINE    TBLCOD        KCOD
105200950609     C     *LIKE         DEFINE    TBLKEY        KKEY
105300951121     C     *LIKE         DEFINE    ARBRAG        KRAG
105400950525     C     *LIKE         DEFINE    ARBFBC        OLDFBC
105500950529     C     *LIKE         DEFINE    ARBAAS        WAAS
105600950529     C     *LIKE         DEFINE    ARBLNP        WLNP
105700950529     C     *LIKE         DEFINE    ARBNRS        WNRS
105800950529     C     *LIKE         DEFINE    ARBNSP        WNSP
105900950525     C*------------------------------------------------------*
106000950525     C*  Controllo sul calendario il tipo di giorno che è la
106100950525     C*  data di consegna
106200950525     C                   Z-ADD     D59DDT        WMMGG             4 0
106300950525     C                   Z-ADD     D59DDT        WG                2 0
106400950525     C                   MOVEL     D59DDT        KANN
106500950525     C                   Z-ADD     0             KTFP
106600950525     C                   Z-ADD     0             KTFA
106700950525     C                   MOVEL     WMMGG         KMES
106800950525     C                   OPEN      AZCLN01L
106900950525     C     KCLN          CHAIN     AZCLN01L                           31
107000950525     C                   MOVEA     CLNMAT        MES
107100950525     C                   MOVE      MES(WG)       WGG               1
107200950525     C                   CLOSE     AZCLN01L
107300950525     C*------------------------------------------------------*
107400950525     C*  Inizializzo variabili
107500030728     C                   MOVEL     *ALL'-'       CMP132          198
107600030728     C                   MOVEL     *ALL'-'       CMP198          198
107700950525     C                   TIME                    TIMES             6 0
107800960927     C                   TIME                    WHDAT            12 0
107900960927     C                   MOVE      WHDAT         WOGGI             6 0
108000951211     C*  Carico dati tabella 3A
108100951211     C                   OPEN      TABEL00F
108200951211     C                   Z-ADD     1             KKUT
108300951211     C                   MOVEL     '3A'          KCOD
108400951211     C                   Z-ADD     0             X
108500951211     C     KTAB          CHAIN     TABEL00F                           31
108600951211     C     *IN31         DOWEQ     '0'
108700951211     C     X             ANDLT     100
108800951211     C                   ADD       1             X
108900951211     C                   MOVEL     TBLKEY        C3A(X)
109000951211     C                   MOVEL     TBLUNI        D3A(X)
109100951211     C     KTAB          READE     TABEL00F                               31
109200951211     C                   END
109300950609     C*  CARICO£6
109400020506     c                   clear                   trul06ds
109500020506     c                   eval      d06cod = '£6'
109600020506     c                   movel     d59fgs        d06key
109700040831     c                   move      'S'           d06key
109800040831     c                   eval      d06drf = d59ddt
109900030721     c                   movel(P)  trul06ds      kpjbu
110000020506     c                   call      'TRUL06R'
110100020506     c                   parm                    kpjba
110200030721     c                   movel(P)  kpjbu         trul06ds
110300020506     c                   movea     lin           £6
110400950526     C                   CLOSE     TABEL00F
111800950531     C*
111900030728     C                   OPEN      FNLRF7P
112000030728     C                   Z-ADD     0             D59PAG
112100030728     C                   Z-ADD     0             D59LIN
112101110208     c* cerco il foglio ima
112102110209     C                   Z-ADD     3             KNPG3             1 0
112104110208     C                   Z-ADD     D59FGS        KFGS
112105110208     C                   Z-ADD     D59ddt        Kdfv
112107110209     c                   clear                   xi                2 0
112108110209     c                   clear                   ima
112109110208     C     KFVV2         setll     FNFVV02L
112110110208     c                   do        *hival
112111110208     C     KFVV2         reade     FNFVV02L
112112110208     c                   if        %eof(fnfvv02l)
112113110208     c                   leave
112114110208     c                   end
112115110208     c                   if        fvvspg = 'A'
112116110209     c                   add       1             xi
112117110209     c                   eval      ima(xi) = fvvnfv
112119110208     c                   end
112120110208     c                   enddo
112200950525     C*
112300950525     C                   ENDSR
112400950525     OFNARB000  E            AGGARB
112500031016     O*                      ARBcmc
112600031016     O*                      ARBfbc
112700030725     O                       ARBACC
112800030708     O                       ARBAMA
112900950613     OFNFVV001  E            AGGFL1
113000950613     O                       FVVFCF
