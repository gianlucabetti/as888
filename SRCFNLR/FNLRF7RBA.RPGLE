000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200030710      *--------------------------------------------------------------*
000300030908     FFNFVV02L  iF   E           K DISK
000400030708     FFNFVV01L  UF   E           K DISK    COMMIT
000500950524     F                                     RENAME(FNFVV000:FNFVV001)
000600070129     FFNBRV01L  IF   E           K DISK
000700110208     FFNBRV02L  IF   E           K DISK
000800110208     F                                     RENAME(FNbrv000:FNbrv002)
000900030721     FFNARB01L  uF   E           K DISK    commit
001000140512     FFIAR601L  IF   E           K DISK    usropn
001100950524     FFNART27L  IF   E           K DISK
001200950524     FFNLBL02L  IF   E           K DISK
001300950525     FAZCLN01L  IF   E           K DISK    USROPN
001400110518     FFiDST01L  IF   E           K DISK
001500041013     FFiqdt01L  IF   E           K DISK
001600021204     Ffiapd01L  IF   E           K DISK
001700950525     FFNARBY1L  UF A E           K DISK
001800140507     FFNARBZ0F  iF A E           K DISK    usropn
001900950526     FTABEL00F  IF   E           K DISK    USROPN
002000101124     FTntbe01l  IF   E           K DISK
002100101124     Fwfnlrq11l uf a E           k DISK
002200101118     Fwfnlrq20f o    E             DISK
002300030728     FFNLRF7P   O    E             PRINTER OFLIND(*IN80)
002400950612     F                                     USROPN
002500950524      *--------------------------------------------------------------*
002600950524      * Schiere
002700950524      *--------------------------------------------------------------*
002800950524     D CDA             S              2    DIM(10)
002900950525     D CDF             S              2    DIM(10)
003000950525     D MES             S              1    DIM(31)
003100951211     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
003200951211     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
003300020506     D £6              S              3  0 DIM(30)                              LINEE ARRIVI
003400101125     D der             S              1    DIM(26)                              FILIALI POSTE
003500110209     c* fogli ima del gg
003600110209     D ima             S              6  0 DIM(10)                              FILIALI POSTE
003700950524      *--------------------------------------------------------------*
003800950524      *   DS
003900950524      *--------------------------------------------------------------*
004000950525     D WLBDA8          DS                  INZ
004100950525     D  G02DAT                 1      8  0
004200950525     D  G02INV                 9     16  0
004300950525     D  G02ERR                17     17
004400950525     D  G02TGI                18     22  0
004500140611     D fnls46ds1     e ds                  inz
004600040827     D OG143         E DS
004700950524     D KPJBA         E DS
004800101123     D dsqa          e ds
004900030721     D ddatiute      e ds
005000101123     D wfnlrq1ds     e ds                  extname(wfnlrq10F)
005100101123     D wfnlrq2ds     e ds                  extname(wfnlrq20F)
005200040713     D dsapd         e ds                  extname(fiapd00F)
005300040713     D dsapds        e ds                  extname(fiapd00F) prefix(s)
005400040713     D azuteds       e ds                  extname(AZUTE00F)
005500030718     D tibs34ds      E DS                  inz
005600081014     D fnlrv7ds      e ds                  inz
005700031017     c* ds parametri pgm fnlr39r blocco/sblocco
005800031017     d dslr02        e ds                  inz
005900031017     d trul06ds      e ds
006000020506     d  lin                    1     90  0 dim(30)
006100031017     c* ds passaggio parametri del pgm
006200950524     D FNLR59        E DS                  EXTNAME(FNLR59DS)
006300031017     c* ds passaggio parametri pgm stampa distinta
006400950524     D FNLR55        E DS                  EXTNAME(FNLR55DS)
006500951211     D DS3A          E DS
006600040713     D arbndcx         S              7    INZ
006700040713     D arbaasx         S              4    INZ
006800040713     D arbggx          S              2    INZ
006900040713     D arbmgsx         S              2    INZ
007000040713     D arbnrsx         S              2    INZ
007100040713     D arbnspx         S              7    INZ
007200040713     D arblnpx         S              3    INZ
007300040713     D DESBRV          S             90    INZ
007400030908     D savndd          S                   INZ like(d59ndd)
007500030908     D savnda          S                   INZ like(d59nda)
007600030728     D CMAT            C                   CONST('MATTINO   ')
007700030724     D CPOM            C                   CONST('POMERIGGIO')
007800040930     D                SDS
007900040930     D  NOMPGM                 1     10
008000950524      *--------------------------------------------------------------*
008100950524      * Ridenomino i campi dei codici anomalie in schiere
008200950524      *--------------------------------------------------------------*
008300950524     IFNARBY00
008400950525     I              ARBA01                      CDF(1)
008500950525     I              ARBA02                      CDF(2)
008600950525     I              ARBA03                      CDF(3)
008700950525     I              ARBA04                      CDF(4)
008800950525     I              ARBA05                      CDF(5)
008900950525     I              ARBA06                      CDF(6)
009000950525     I              ARBA07                      CDF(7)
009100950525     I              ARBA08                      CDF(8)
009200950525     I              ARBA09                      CDF(9)
009300950525     I              ARBA10                      CDF(10)
009400950524     C*--------------------------------------------------------------*
009500950524     C*  Ciclo principale
009600950524     C*--------------------------------------------------------------*
009700950613     C*  Imposto la chiave per il posizionamento
009800950524     C                   Z-ADD     4             KNPG
009900030722     C                   Z-ADD     D59nda        Knfv
010000030722     C                   Z-ADD     D59FGS        KFGS
010100031016      *  mi posiziono sulla distinta
010200030908     C     KFVV1         CHAIN(n)  FNFVV01L                           3132
010300030729     C                   IF        NOT *IN31  AND NOT *IN32
010400030909     C                             AND FVVDFV = D59DDT  and fvvfcf <> 'S'
010500030909     c                             and fvvatb <> '*'
010600030725     C                   EXSR      GESDIS
010700030725     C                   END
010800950911     C*  Richiamo pgm. per l'ultima volta x farli chiudere in LR
010900950911     C                   EXSR      CLOFIL
011000950525     C*
011100950524     C                   SETON                                        LR
011200030708     C*--------------------------------------------------------------*
011300030708     C*  Gestione singola distinta
011400030708     C*--------------------------------------------------------------*
011500030708     C     GESDIS        BEGSR
011600030721     c*
011700030718     c                   do
011800030718     C                   MOVEL     'N'           D59ANM
011900030718     C                   Z-ADD     FVVNFV        KNFV
012000041013     C                   Z-ADD     FVVdfv        Kdfv
012100030718     C                   Z-ADD     FVVNFV        KNFV1
012200041013     C                   Z-ADD     FVVNFV        KNFV2
012300030708     c* aggancio la distinta
012400110518     C     KDST          CHAIN     FiDST01L
012500110518     c                   if        not %found(fidst01l)
012600030708     c                   leave
012700030805     c                   else
012800041013     c* utilizzato il campo del numero delle bolle x bancali visto che non è
012900041013     c* + utilizzato in quanto le bolle vengono sempre ricalcolate
013000041013     C     Kqdt          CHAIN     Fiqdt01L
013100050207     c                   if        %found(fiqdt01l) and qdtatb = ' '
013200041013     c                   z-add     QDTNBNAF      d59nbl
013300041013     c                   else
013400110518     c*                  z-add     dstnbn        d59nbl
013500050207     c                   clear                   QDTFNBAF
013600050207     c                   clear                   QDTNBNAF
013700030708     c                   end
013800041013     c                   end
013900030708     c* aggancio padroncino
014000030708     C     kapd          CHAIN     fiapd01L
014100030708     c                   if        not %found(fiapd01l)
014200030708     c                   leave
014300030708     c                   end
014400030909     C*  Richiamo pgm x allocare la distinta ed azzerare quelle posta
014500030909     c                   z-add     d59ndd        savndd
014600030909     c                   z-add     d59nda        savnda
014700030909     c                   z-add     fvvnfv        d59ndd
014800030909     c                   z-add     fvvnfv        d59nda
014900030909     C                   MOVEL(p)  FNLR59        KPJBU
015000030909     C                   CALL      'FNLRF7R1'
015100030909     C                   PARM                    KPJBA
015200030909     C                   MOVEL(p)  KPJBU         FNLR59
015300030909     c                   z-add     savndd        d59ndd
015400030909     c                   z-add     savnda        d59nda
015500030909     c* alloco la distinta
015600030909     C     Keyfvv        CHAIN     FNFVV01L                           3132
015700030909     C                   IF        NOT *IN31  AND NOT *IN32
015800030909     C                             AND FVVDFV = D59DDT and fvvfcf = '§'
015900950613     C                   Z-ADD     FVVNFV        D59NDT
016000030723     C                   MOVE      APDPDR        D59PDR
016100030723     C                   MOVE      FVVFPP        D59FPP
016200030723     C                   MOVEL     APDRSC        D59DSF
016300030721     c* STAMPO ANOMALIE PARTENDO DALLE BOLLE
016400030723     C                   MOVEL(p)  FNLR59        KPJBU
016500030718     C                   CALL      'FNLRF7R4'
016600030718     C                   PARM                    KPJBA
016700030723     c                   movel(p)  kpjbu         FNLR59
016800030722     C*  Richiamo pgm x calcolo stop spedizioni manuali
016900030721     C                   MOVEL(p)  FNLR59        KPJBU
017000030721     C                   CALL      'FNLRF7R3'
017100030721     C                   PARM                    KPJBA
017200030721     C                   MOVEL(p)  KPJBU         FNLR59
017300030718     C* scrivo errori in fnarby leggendo le spunte
017400950524     C                   EXSR      DETBRV
017500030801     c* se ho stampato anomalie da bolla salto 2 righe in stampa
017600101201     c                   if        d59lin <> 0  and
017700101201     c                             (d59sqa = ' ' or
017800101201     c                             d59sqa = 'T')
017900030801     c                   write     salto
018000030801     c                   add       2             lin
018100030801     c                   end
018200030718     C* stampo anomalie da spunte e metto eventuali spedizioni
018300030718     c* poste in distinta
018400030721     C                   MOVEL(p)  FNLR59        KPJBU
018500030703     C                   CALL      'FNLRF7R2'
018600950524     C                   PARM                    KPJBA
018700030721     C                   MOVEL(p)  KPJBU         FNLR59
018800030909     C* sblocca foglio e stampa
018900030718     c                   exsr      sblodis
019000030908     c                   end
019100030909     C                   ENDdo                                                  Trovate spunte
019200950524     C*
019300030718     C                   ENDSR
019400950524     C*--------------------------------------------------------------*
019500950524     C*  Dettaglio singola spunta
019600950524     C*--------------------------------------------------------------*
019700950524     C     DETBRV        BEGSR
019800030718     C*
019900070129     C     KBRV          setll     FNBRV01L
020000030718     C                   DO        *hival
020100070129     C     KBRV          READE     FNBRV01L
020200070129     c                   if        %eof(fnbrv01l)
020300030718     c                   leave
020400030718     c                   end
020500110208     c* cancella la spunta 3 o 4 + vecchia
020600110209     c                   if        kcdaz = 'LRQ1' and xi <> 0
020700110208     c                   exsr      srls46
020800110208     c* cancellata spunta 4
020900150903     c*                  if        canc4 ='1'
021000150903     c*                  iter
021100150903     c*                  end
021200110208     c                   end
021300030718     C*
021400030718     C                   Z-ADD     0             X                 3 0
021500030718     C                   CLEAR                   FNARBY00
021600030718     C                   MOVEA     *BLANKS       CDA
021700030718     C*
021800950524     C*  E R R O R I      B L O C C A N T I  ! ! ! ! !
021900030718     C*
022000950609     C*  CONTROLLO SE SPARATO COLLO DI ALTRA FILIALE
022100950609     C     BRVLNA        LOOKUP    £6                                     39
022200950609     C     *IN39         IFEQ      '0'
022300030804     c                   exsr      srtesta
022400030728     C                   EVAL      DESBRV = 'SPUNTATO COLLO DI ALTRO P.O.'
022500040713     C                   EVAL      DESBol = '++++ +++ ++ +++++++  ++/++'
022600110414     c                   clear                   destsp
022700101201     c* stampa
022800101201     c                   if        d59sqa = ' ' or
022900101201     c                             d59sqa = 'T'
023000030728     C                   WRITE     ERR1
023100101201     c                   end
023200101201     c* video
023300101201     c                   if        d59sqa = 'S' or
023400101201     c                             d59sqa = 'T'
023500101123     c                   eval      tbeke2 = '001'
023600101118     c                   exsr      srsqa
023700101118     c                   end
023800030728     C                   ADD       1             D59LIN
023900030718     C                   iter
024000030718     c                   end
024100950524     C*  Mi posiziono dalle spunte sul dettaglio segnacolli
024200110121     C                   Z-ADD     BRVLNP        KFLS
024300110121     C                   Z-ADD     BRVLNA        KLNA
024400110121     C                   Z-ADD     BRVNRS        KNRSa
024500110121     C                   Z-ADD     BRVNSC        KNSC
024600030718     C     KART          CHAIN     FNART27L
024700030718     C                   IF        not %found(fnart27l)
024800030804     c                   exsr      srtesta
024900030728     C                   EVAL      DESBRV = 'MANCA RECORD BOLLA'
025000040713     C                   EVAL      DESBol = '++++ +++ ++ +++++++  ++/++'
025100110414     c                   clear                   destsp
025200101124     c* stampa
025300101201     c                   if        d59sqa = ' ' or
025400101201     c                             d59sqa = 'T'
025500101124     C                   WRITE     ERR1
025600101201     c                   end
025700101124     c* video
025800101201     c                   if        d59sqa = 'S' or
025900101201     c                             d59sqa = 'T'
026000101123     c                   eval      tbeke2 = '002'
026100101118     c                   exsr      srsqa
026200101118     c                   end
026300030728     C                   ADD       1             D59LIN
026400030718     C                   iter
026500030718     c                   end
026600030718     C* Se i dati della spedizione sono uguali salto controlli xchè già
026700040714     c* eseguiti
026800030718     C     WNSP          IFeq      ARTNSP
026900030718     C     WNRS          andeq     ARTNRS
027000030718     C     WLNP          andeq     ARTLNP
027100030718     C     WAAS          andeq     ARTAAS
027200040714     c                   exsr      srerr03
027300030718     c                   iter
027400081107     c                   else
027500081107     c                   clear                   flgdoppi          1 0
027600030718     c                   end
027700040714     C*  a cambio spedizione
027800040714     C                   Z-ADD     ARTAAS        WAAS
027900950529     C                   Z-ADD     ARTLNP        WLNP
028000950529     C                   Z-ADD     ARTNRS        WNRS
028100950529     C                   Z-ADD     ARTNSP        WNSP
028200040714     C                   Z-ADD     ARTAAS        KAAS
028300040714     C                   Z-ADD     ARTLNP        KLNP
028400040714     C                   Z-ADD     ARTNRS        KNRS
028500040714     C                   Z-ADD     ARTNSP        KNSP
028600040714     C     KARB          CHAIN     FNARB01L                           36
028700950530     C                   MOVEL     ARBFBC        OLDFBC
028800950524     C*  Manca bolla
028900950525     C     *IN36         IFEQ      '1'
029000950525     C                   Z-ADD     ARTAAS        ARBAAS
029100950525     C                   Z-ADD     ARTLNP        ARBLNP
029200950525     C                   Z-ADD     ARTNRS        ARBNRS
029300950525     C                   Z-ADD     ARTNSP        ARBNSP
029400950524     C                   ADD       1             X
029500950524     C                   MOVEL     '01'          CDA(X)
029600030718     C                   exsr      sragg
029700030718     c                   iter
029800950524     C                   END
029900081014     c* per prima memorizzo segnazione di probabile etichetta doppia
030000081107     c                   exsr      srdoppi
030100081014     c* più di 3 ricorrenze trovate di cui almeno una anche in partenza
030200090615     c                   if        OLRV7NRIC >= 3 and OLRV7PART ='S'
030300081014     C                   ADD       1             X
030400081014     C                   MOVEL     '17'          CDA(X)
030500081014     C                   exsr      sragg
030600081107     c                   eval      flgdoppi = 1
030700081014     C                   END
030800040827     c* dal 15/8/04 le sped.poste vengono immesse manualmente eccetto SDA
030900040827     c                   movel     arbaas        arbdsp            8 0
031000040827     c                   move      arbmgs        arbdsp
031100030721     C*  Bolla già consegnata
031200030721     C     ARBDCM        IFNE      0
031300030721     C                   ADD       1             X
031400030721     C                   MOVEL     '02'          CDA(X)
031500030721     C     X             IFEQ      10
031600030721     C                   exsr      sragg
031700030721     c                   iter
031800030721     C                   END
031900030721     C                   END
032000950524     C*  Controllo se spedizione mamma
032100950524     C                   Z-ADD     ARTAAS        KAAP
032200950524     C                   Z-ADD     ARTLNP        KLPP
032300950524     C                   Z-ADD     ARTNRS        KNRP
032400950529     C                   Z-ADD     ARTNSP        KNSP1
032500950524     C                   MOVEL     'N'           WTRBOL            1
032600950529     C                   MOVEL     'N'           WBOLOK            1
032700950524     C     KLBL          SETGT     FNLBL02L
032800950524     C     KLBL          READPE    FNLBL02L                               34
032900950524     C*  Se lo è cerco l'ultima bolla figlia sbloccata da consegnare
033000950524     C     *IN34         IFEQ      '0'
033100950524     C                   EXSR      CHKFGL
033200950524     C                   END
033300950524     C*  Se esistono figlie ma sono tutte bloccate emetto msg. errore
033400950524     C     WTRBOL        IFEQ      'S'
033500950529     C     WBOLOK        ANDEQ     'N'
033600950615     C                   MOVEL     *BLANKS       OLDFBC
033700950524     C                   ADD       1             X
033800950615     C*  SE NON HO TROVATO FIGLIE MI RIPOSIZIONO SU MAMMA
033900950615     C     *IN36         IFEQ      '1'
034000950615     C                   MOVEL     '14'          CDA(X)
034100950615     C                   Z-ADD     ARTAAS        KAAS
034200950615     C                   Z-ADD     ARTLNP        KLNP
034300950615     C                   Z-ADD     ARTNRS        KNRS
034400950615     C                   Z-ADD     ARTNSP        KNSP
034500950615     C     KARB          CHAIN     FNARB01L                           36
034600040827     c* dal 15/8/04 le sped.poste vengono immesse manualmente eccetto SDA
034700040827     c                   movel     arbaas        arbdsp            8 0
034800040827     c                   move      arbmgs        arbdsp
034900950615     C                   ELSE
035000030923     C     ARBFBC        IFNE      *BLANKS
035100950615     C                   MOVEL     '04'          CDA(X)
035200950615     C                   END
035300030923     C                   END
035400950524     C     X             IFEQ      10
035500030718     C                   exsr      sragg
035600030718     c                   iter
035700950524     C                   END
035800950524     C                   END
035900030912     C*  Se bolla non legata ...
036000950524     C     WTRBOL        IFEQ      'N'
036100070530     C*  Bolla non in distinta errore solo se sono nel pgm di "quadratura"
036200070530     C                   IF        arbndc = 0  and
036300101216     C*                            (kcdaz = 'LRF6' or kcdaz = 'LRF7'
036400101216     C*                            or kcdaz = 'LRF0' or kcdaz = 'LRF4')
036500101216     c                             kcdaz<>'LR59' and kcdaz<>'LR60'
036600030912     C                   ADD       1             X
036700040415     C                   MOVEL     '15'          CDA(X)
036800030912     C     X             IFEQ      10
036900030912     C                   exsr      sragg
037000030912     c                   iter
037100030912     C                   END
037200030912     C                   END
037300030912     C*  Se bolla non legata controllo se bloccata
037400030912     C     ARBFBC        ifne      *BLANKS
037500950525     C                   MOVEL     'N'           WFIGL             1            bolla figlia
037600950524     C                   EXSR      SBLOCC
037700950524     C     ARBFBC        IFNE      *BLANKS
037800950530     C                   MOVEL     *BLANKS       OLDFBC
037900950530     C                   ADD       1             X
038000950524     C                   MOVEL     '04'          CDA(X)
038100950524     C     X             IFEQ      10
038200030718     C                   exsr      sragg
038300030718     c                   iter
038400950524     C                   END
038500030912     C                   END
038600950524     C                   END
038700950524     C                   END
038800040714     C*  Bolla in consegna su altra distinta (non memorizzo + l'errore 03
038900040714     c*  ma stampo subito)
039000040714     C     ARBNDC        IFNE      0
039100040714     C     ARBNDC        ANDNE     FVVNFV
039200040714     C     ARBDCM        ANDEQ     0
039300040714     c                   exsr      srerr03
039400040714     c                   unlock    fnarb01l
039500040714     c                   iter
039600040714     C                   END
039700040713     C*                  END
039800950524     C*  Controllo se porto assegnato non tassato
039900950524     C     ARBACA        IFNE      'S'
040000950524     C                   ADD       1             X
040100950524     C                   MOVEL     '05'          CDA(X)
040200950524     C     X             IFEQ      10
040300030718     C                   exsr      sragg
040400030718     c                   iter
040500950524     C                   END
040600950524     C                   END
040700030718     C*
040800031016     C*  Se non ci sono errori bloccanti procedo al calcolo degli stop.
040900070530     C*  solo se pgm "da spunte"
041000070530     c                   if        arbngd = 0 and
041100070530     c                             (kcdaz='LR59' or kcdaz='LR60')
041200950526     C                   EXSR      CTRSTP
041300030723     c                   end
041400030718     C*
041500950524     C*  E R R O R I    N O N    B L O C C A N T I  ! ! ! ! !
041600030718     C*
041700950525     C     OLDFBC        IFNE      *BLANK
041800950524     C     ARBFBC        ANDEQ     *BLANK
041900950524     C                   ADD       1             X
042000950524     C                   MOVEL     '06'          CDA(X)
042100950524     C     X             IFEQ      10
042200030718     C                   exsr      sragg
042300030718     c                   iter
042400950524     C                   END
042500950524     C                   END
042600950524     C*  Spedizione da abilitare all'incasso C/ass.
042700950524     C     ARBACC        IFNE      'S'
042800950524     C                   MOVEL     'S'           ARBACC
042900950524     C                   ADD       1             X
043000950524     C                   MOVEL     '07'          CDA(X)
043100950524     C     X             IFEQ      10
043200030718     C                   exsr      sragg
043300030718     c                   iter
043400950524     C                   END
043500950524     C                   END
043600031110     C* Spedizione da abilitare alla consegna merce
043700070530     C* aggiorno ARBAMA solo se sto lanciando "da spunte"
043800950524     C     ARBAMA        IFEQ      0
043900950524     C                   Z-ADD     3             ARBAMA
044000070530     c                   if        arbngd = 0 and
044100070530     c                             (kcdaz='LR59' or kcdaz='LR60')
044200070530     c                   else
044300031110     C                   ADD       1             X
044400031110     C                   MOVEL     '08'          CDA(X)
044500031110     C     X             IFEQ      10
044600031110     C                   exsr      sragg
044700031110     c                   iter
044800031110     C                   END
044900031110     C                   END
045000950524     C                   END
045100950524     C*  Consegnata sped. fermo deposito con padroncino no 9999
045200950524     C     ARBFFD        IFEQ      'S'
045300051108     C     ARBFFD        oreq      'M'
045400950602     C     APDFCM        IFNE      'S'
045500950524     C                   ADD       1             X
045600950524     C                   MOVEL     '09'          CDA(X)
045700950524     C     X             IFEQ      10
045800030718     C                   exsr      sragg
045900030718     c                   iter
046000950524     C                   END
046100950524     C                   END
046200950524     C                   END
046300950524     C*  Controllo se spedizione consegnata in anticipo
046400950524     C     ARBDCR        IFGE      D59DDT
046500950524     C     ARBTCR        ANDEQ     'D'
046600950524     C     ARBDCR        ORGT      D59DDT
046700950524     C     ARBTCR        ANDEQ     ' '
046800950524     C                   ADD       1             X
046900950524     C                   MOVEL     '10'          CDA(X)
047000950524     C     X             IFEQ      10
047100030718     C                   exsr      sragg
047200030718     c                   iter
047300950524     C                   END
047400950524     C                   END
047500950524     C*  Calcolo turno chiusura
047600950524     C                   EXSR      TURNO
047700030725     C*  aggiorno fnarb SE POSTE e fnarby
047800030718     C                   EXSR      sragg
047900950529     C*
048000030718     C                   ENDdo
048100030718     C*
048200950529     C                   ENDSR
048300030721     C*--------------------------------------------------------------*
048400081014     C*  bolla legata ad altra distinta
048500030721     C*--------------------------------------------------------------*
048600040714     C     srerr03       BEGSR
048700081107     c*
048800081107     c                   exsr      srdoppi
048900040714     c*
049000040714     C     ARBNDC        IFNE      0
049100040714     C     ARBNDC        ANDNE     FVVNFV
049200040714     C     ARBDCM        ANDEQ     0
049300040714     C     *in36         ANDEQ     *off
049400040714     c                   exsr      srtesta
049500040714     C                   EVAL      DESBRV = 'COLLO SPUNTATO DI DISTINTA NR.'
049600040714     C                   EVAL      arbndcx=%editc(arbndc:'Z')
049700040714     C                   EVAL      %subst(desbrv: 31: 7)=arbndcx
049800040714     C                   EVAL      %subst(desbrv: 39: 6)='PADR. '
049900040714     c* salvo i dati dell'autista della distinta che sto quadrando
050000040714     c* per non sporcarli
050100040714     c                   eval      dsapds = dsapd
050200040714     c     kapd2         chain     fiapd01l
050300040714     C                   EVAL      %subst(desbrv: 45: 30)=apdrsc
050400040714     c                   eval      dsapd = dsapds
050500040714     c* riferimenti bolla prendo quelli del file FNART xchè i riferimenti
050600040714     c* spedizione del file FNARB la seconda volta sono puliti x effetto
050700040714     c* della clear di fnarby0f
050800040714     C                   movel     artaas        arbaasx
050900040714     C                   EVAL      %subst(desbol: 1: 4)=arbaasx
051000040714     C                   EVAL      arblnpx=%editc(artlnp:'Z')
051100040714     C                   EVAL      %subst(desbol: 6: 3)=arblnpx
051200040714     C                   movel     artnrs        arbnrsx
051300040714     C                   EVAL      %subst(desbol: 10: 2)=arbnrsx
051400040714     C                   EVAL      arbnspx=%editc(artnsp:'Z')
051500040714     C                   EVAL      %subst(desbol: 13: 7)=arbnspx
051600040714     C                   EVAL      arbggx=%subst(%editc(arbmgs:'Z'): 3: 2)
051700040714     C                   EVAL      %subst(desbol: 22: 2)= arbggx
051800040714     C                   EVAL      %subst(desbol: 24: 1)='/'
051900040714     C                   movel     arbmgs        arbmgsx
052000040714     C                   EVAL      %subst(desbol: 25: 2)=arbmgsx
052100110414     c                   eval      destsp = arbtsp
052200101124     c* stampa
052300101201     c                   if        d59sqa = ' ' or
052400101201     c                             d59sqa = 'T'
052500101124     C                   WRITE     ERR1
052600101201     c                   end
052700101124     c* video
052800101201     c                   if        d59sqa = 'S' or
052900101201     c                             d59sqa = 'T'
053000101123     c                   eval      tbeke2 = '003'
053100101118     c                   exsr      srsqa
053200101118     c                   end
053300040714     C                   ADD       1             D59LIN
053400081107     c* più di 3 ricorrenze trovate di cui almeno una anche in partenza
053500090615     c                   if        OLRV7NRIC >= 3 and OLRV7PART ='S'
053600081014     c                   exsr      srtesta
053700081014     C                   EVAL      DESBRV = 'COLLO DOPPIO'
053800101124     c* stampa
053900101201     c                   if        d59sqa = ' ' or
054000101201     c                             d59sqa = 'T'
054100081014     C                   WRITE     ERR1
054200101201     c                   end
054300101124     c* video
054400101201     c                   if        d59sqa = 'S' or
054500101201     c                             d59sqa = 'T'
054600101123     c                   eval      tbeke2 = '004'
054700101118     c                   exsr      srsqa
054800101118     c                   end
054900081014     C                   ADD       1             D59LIN
055000040714     C                   END
055100081107     C                   END
055200081107     c* se stessa distinta e stessa spedizione memorizzo l'anomalia
055300081107     c* di collo doppio se non già segnalata nei colli precedenti
055400081107     c                   if        not *in36 and flgdoppi = 0
055500081107     c                             and arbdcm = 0 and arbndc = fvvnfv
055600090615     c                             and OLRV7NRIC >= 3 and OLRV7PART ='S'
055700081107     C                   ADD       1             X
055800081107     C                   MOVEL     '17'          CDA(X)
055900100105     c* rileggo FNARB: se sono nel pgm "da spunte" perchè posso averlo
056000100105     c* rilasciato
056100100105     c                   if        arbngd = 0 and
056200100105     c                             (kcdaz='LR59' or kcdaz='LR60')
056300100105     C                   Z-ADD     ARtAAS        KAAS
056400100105     C                   Z-ADD     ARtLNP        KLNP
056500100105     C                   Z-ADD     ARtNRS        KNRS
056600100105     C                   Z-ADD     ARtNSP        KNSP
056700100105     C     KARB          CHAIN     FNARB01L                           36
056800100105     c                   end
056900081107     C                   exsr      sragg
057000081107     c                   eval      flgdoppi = 1
057100081107     c                   end
057200040714     c*
057300040714     C                   ENDSR
057400081014     C*--------------------------------------------------------------*
057500081014     C*  Cerco figlia
057600081014     C*--------------------------------------------------------------*
057700081107     c     srdoppi       BEGSR
057800081107     c*
057900081107     c* verifico eventuali etichette doppie e stampo subito
058000081107     c                   clear                   fnlrv7ds
058100081107     c                   eval      ILRV7FLS=artfls
058200081107     c                   eval      ILRV7LNA=artlna
058300081107     c                   eval      ILRV7NRS=artnrs
058400081107     c                   eval      ILRV7NSC=artnsc
058500081107     c                   movel(p)  fnlrv7ds      kpjbu
058600081107     c                   call      'FNLRV7R'
058700081107     c                   parm                    kpjba
058800081107     c                   movel     kpjbu         fnlrv7ds
058900081107     c*
059000081107     C                   ENDSR
059100081107     C*--------------------------------------------------------------*
059200081107     C*  Cerco figlia
059300081107     C*--------------------------------------------------------------*
059400081107     C     CHKFGL        BEGSR
059500030721     C*  Se bolla con C/assegno controllo se da abilitare
059600030721     C     WBOLOK        DOWEQ     'N'
059700030721     C     *IN34         ANDEQ     '0'
059800030721     C     *IN36         ANDEQ     '0'
059900030721     C     LBLLAP        IFEQ      LBLLAN
060000030721     C                   MOVEL     'S'           WTRBOL
060100030721     C                   Z-ADD     LBLAAN        KAAS
060200030721     C                   Z-ADD     LBLLPN        KLNP
060300030721     C                   Z-ADD     LBLNRN        KNRS
060400030721     C                   Z-ADD     LBLNSN        KNSP
060500030721     C     KARB          CHAIN     FNARB01L                           36
060600030721     C     *IN36         IFEQ      '0'
060700040827     c* dal 15/8/04 le sped.poste vengono immesse manualmente eccetto SDA
060800040827     c                   movel     arbaas        arbdsp            8 0
060900040827     c                   move      arbmgs        arbdsp
061000070530     C*  Bolla non in distinta errore solo se sono nel pgm di "quadratura"
061100070530     C                   IF        arbndc = 0 and
061200031103     C                             (kcdaz = 'LRF6' or kcdaz = 'LRF7'
061300040426     C                              or kcdaz = 'LRF0' or kcdaz = 'LRF4')
061400030721     C                   ADD       1             X
061500030721     C                   MOVEL     '15'          CDA(X)
061600030721     C     X             IFEQ      10
061700030721     C                   exsr      sragg
061800030721     C                   END
061900030721     C                   else
062000030721     C     ARBNDC        ifne      9999999
062100030721     C                   MOVEL     ARBFBC        OLDFBC
062200030721     C*  Se bolla bloccata controllo se sbloccabile
062300030721     C     ARBFBC        IFNE      *BLANKS
062400030721     C                   MOVEL     'S'           WFIGL                          bolla figlia
062500030721     C                   EXSR      SBLOCC
062600030721     C                   END
062700030721     C*  Se sbloccata metto in consegna figlia
062800030721     C     ARBFBC        IFEQ      ' '
062900030721     C                   MOVEL     'S'           WBOLOK
063000030721     C                   END
063100030721     C*
063200030721     C                   END
063300030721     C                   END
063400030721     C                   END
063500030721     C                   END
063600030721     C*  Se non trovata bolla loop su successiva
063700030721     C     WBOLOK        IFEQ      'N'
063800030721     C     KLBL          READPE    FNLBL02L                               34
063900030721     C                   END
064000030721     C*
064100030721     C                   ENDdo
064200030721     C*
064300030721     C                   ENDSR
064400030718     C*-----------------------------------------------------*
064500030718     C* aggiorna FNARB e metto in FNARBY anomalia           *
064600030718     C*-----------------------------------------------------*
064700030718     C     sragg         BEGSR
064800070530     c* aggioro FNARB: se sono nel pgm "da spunte"
064900081205     c* se il flg di colli doppi è valorizzato a 1 vuol dire che ho già
065000081205     c* aggiornato la spedizione e sono sul dettaglio dei sui colli
065100030722     c                   if        not *in36
065200081205     c                   if        arbngd = 0 and flgdoppi = 0 and
065300070530     c                             (kcdaz='LR59' or kcdaz='LR60')
065400030722     C                   EXCEPT    AGGARB
065500030722     c                   else
065600030722     c                   unlock    fnarb01l
065700030722     c                   end
065800030722     c                   end
065900030722     c*
066000030718     C     KARB          CHAIN     FNARBY1L                           33
066100030718     C                   MOVEL     CDA           CDF
066200030718     C   33              WRITE     FNARBY00
066300030718     C  N33              UPDATE    FNARBY00
066400030722     c*
066500030718     C                   ENDSR
066600950526     C*-----------------------------------------------------*
066700101123     C* memorizzo l'errore nel file di work                 *
066800950526     C*-----------------------------------------------------*
066900101123     C     srsqa         BEGSR
067000101123     c                   clear                   dsqa
067100101123     c     ktbe          chain     tntbe01l
067200101123     c                   if        %found(tntbe01l)
067300101124     c                   movel     tbeuni        dsqa
067400101123     c                   if        D§SQAELA = 'S'
067500101124     c* TESTATA
067600101123     c                   clear                   wfnlrq1ds
067700101125     c                   eval      wlrq1ndc = d59ndt
067800101125     c                   eval      wlrq1fgs = d59fgs
067900101125     c                   eval      wlrq1pdr = d59pdr
068000101123     c* se ho i rif. della spedizione li memorizzo altrimenti 0
068100101125     c                   movea     desbol        der
068200101125     c                   do        26            xx                2 0
068300101125     C                   if        der(xx) = ' '
068400101125     c                   eval      der(xx) ='0'
068500101125     c                   end
068600101125     c                   enddo
068700101126     c                   movea     der           descom           26
068800101125     c*
068900101126     c                   eval      comaas = %subst(descom: 1: 4)
069000101124     c                   testn                   comaas            4  99
069100101124     c   99              movel     comaas        wlrq1aas
069200101126     c                   eval      comlnp = %subst(descom: 6: 3)
069300101124     c                   testn                   comlnp            3  99
069400101124     c   99              movel     comlnp        wlrq1lnp
069500101126     c                   eval      comnrs = %subst(descom: 10: 2)
069600101124     c                   testn                   comnrs            2  99
069700101124     c   99              movel     comnrs        wlrq1nrs
069800101126     c                   eval      comnsp = %subst(descom: 13: 7)
069900101124     c                   testn                   comnsp            7  99
070000101124     c   99              movel     comnsp        wlrq1nsp
070100101124     c*
070200101124     c     kwf10         chain     wfnlrq11l
070300101124     c                   if        not %found(wfnlrq11l)
070400101124     c                   movel     tbeke2        wlrq1cer
070500101124     c                   eval      wlrq1sce = D§SQASCE
070600101123     c                   eval      wlrq1err = desbrv
070700101124     c                   write     wfnlrq10
070800101124     c                   else
070900101124     c                   eval      wlrq1err = 'Errore dettaglio colli'
071000101124     c                   clear                   wlrq1cer
071100101124     c                   clear                   wlrq1sce
071200101124     c                   update    wfnlrq10
071300101124     c                   end
071400101124     c* dettaglio
071500101124     c                   clear                   wfnlrq2ds
071600101124     c* se ho i rif. della spedizione li memorizzo altrimenti 0
071700101124     c                   eval      wlrq2aas = wlrq1aas
071800101124     c                   eval      wlrq2lnp = wlrq1lnp
071900101124     c                   eval      wlrq2nrs = wlrq1nrs
072000101124     c                   eval      wlrq2nsp = wlrq1nsp
072100101124     c                   eval      wlrq2ndc = wlrq1ndc
072200101124     c                   eval      wlrq2fgs = wlrq1fgs
072300101124     c                   eval      wlrq2pdr = wlrq1pdr
072400101124     c                   movel     tbeke2        wlrq2cer
072500101124     c                   eval      wlrq2sce = D§SQASCE
072600101124     c                   eval      wlrq2err = desbrv
072700110104     c                   eval      wlrq2fls = brvlnp
072800101124     c                   eval      wlrq2nsc = brvnsc
072900110126     c                   eval      wlrq2nrc = brvnrs
073000110131     c                   if        tbeke2 = '003'
073100110104     c                   eval      wlrq2flv = brvfgs
073200101126     c                   eval      wlrq2nfv = brvnfv
073300110117     c                   eval      wlrq2dfv = D59DDT
073400101126     c                   eval      wlrq2npg = brvnpg
073500101126     c                   eval      wlrq2hms = brvhms
073600101126     c                   eval      wlrq2nps = brvnps
073700110131     c                   end
073800101124     c                   write     wfnlrq20
073900101123     c                   end
074000101124     c                   end
074100101123     C                   ENDSR
074200101123     C*-----------------------------------------------------*
074300101123     C* Gestione eventuale attribuzione Stop                *
074400101123     C*-----------------------------------------------------*
074500101123     C     CTRSTP        BEGSR
074600951121     C                   MOVEL     *BLANKS       KRAG
074700951211     C                   Z-ADD     0             KKSC              7 0
074800951211     C                   Z-ADD     0             ARBKCS
074900951211     C*  Controllo se devo attribuire lo Stop
075000951211     C                   Z-ADD     1             Y                 2 0
075100951211     C                   MOVEL     *BLANKS       WTBOL1            1
075200951211     C     ARBCBO        LOOKUP    C3A(Y)                                 30
075300951211     C     *IN30         IFEQ      '1'
075400951211     C                   MOVEL     D3A(Y)        DS3A
075500951211     C                   MOVEL     §3ATB1        WTBOL1
075600951211     C     WTBOL1        IFEQ      'A'
075700951211     C                   Z-ADD     ARBKSC        KKSC
075800951211     C                   END
075900951211     C                   END
076000951211     C*
076100951211     C     *IN30         IFEQ      '1'
076200990921     C     WTBOL1        IFEQ      'A'
076300990921     C     §3ATB2        ORNE      *BLANKS
076400990921     C     KARB          CHAIN     FIAR601L                           30
076500990921     C  N30              Z-ADD     AR6KSC        KKSC
076600951211     C                   END
076700951211     C                   END
076800140512      *
076900951211     C*  Controllo se cliente no generico e se si attribuisco stop
077000951211     C                   Z-ADD     KKSC          APPOG             4 0
077100951211     C     KKSC          IFNE      0
077200951211     C     APPOG         ANDNE     9999
077300951211     C     APPOG         ANDNE     8888
077400951211     C                   Z-ADD     KKSC          ARBKCS
077500951211     C                   MOVEL     KKSC          KRAG
077600951211     C                   ELSE
077700951211     C*  Se no assegnato codificato lavoro con rag.sociale/indirizzo
077800951121     C                   MOVEL     ARBRSD        KRAG
077900951121     C                   MOVE      ARBIND        KRAG
078000951211     C                   END
078100140507      *
078200951211     C*  Attribuisco lo Stop
078300951121     C     KRAG          CHAIN     FNARBZ0F                           30
078400951121     C*  Controllo se cliente esiste già
078500950526     C     *IN30         IFEQ      '1'
078600950526     C                   ADD       1             D59STO
078700950526     C                   Z-ADD     D59STO        ARBSTP
078800951121     C                   MOVEL     KRAG          ARBRAG
078900950526     C                   WRITE     FNARBZ00
079000950526     C                   END
079100950526     C*
079200950526     C                   ENDSR
079300950525     C*--------------------------------------------------------------*
079400950525     C*  Sblocco spedizione se possibile
079500950525     C*--------------------------------------------------------------*
079600950525     C     SBLOCC        BEGSR
079700070530     c* sblocco FNARB: se pgm "da spunte"
079800070530     c                   if        arbngd = 0 and
079900070530     c                             (kcdaz='LR59' or kcdaz='LR60')
080000031017     c* devo rilasciare FNARB xchè l'aggiornamento dei campi
080100031017     c* ARBCMC e ARBFBC avviene all'interno del pgm fnlr39r
080200031017     c                   unlock    fnarb01l
080300031017     c                   clear                   dslr02
080400031017     C                   Z-ADD     arbaas        DR2AAS
080500031017     C                   Z-ADD     arbLNP        DR2LNP
080600031017     C                   Z-ADD     arbNRS        DR2NRS
080700031017     C                   Z-ADD     arbNSP        DR2NSP
080800040831     C                   Z-ADD     d59fgs        DR2FGS
080900031017     C                   MOVEL     '1'           DR2RIC
081000031017     C                   MOVEL     '1'           DR2comit
081100031017     C                   MOVEL     'N'           DR2vid
081200031017     C                   MOVEL     'N'           DR2pos
081300031017     C                   MOVEL     DSLR02        KPJBU
081400031017     C                   CALL      'FNLR39R'
081500031017     C                   PARM                    KPJBA
081600031017     c                   movel     kpjbu         dslr02
081700031017     c     karb          chain     fnarb01l
081800031016     c                   end
081900950529     C*
082000030704     C                   ENDSR
082100950525     C*--------------------------------------------------------------*
082200950525     C*  Gestisco turno di chiusura
082300950525     C*--------------------------------------------------------------*
082400950525     C     TURNO         BEGSR
082500950525     C*
082600950525     C     ARBGC1        IFNE      *BLANKS
082700950525     C                   MOVEL     ARBGC1        WGIOR             1
082800950525     C     WGIOR         IFEQ      WGG
082900950525     C                   ADD       1             X
083000950525     C                   MOVEL     '11'          CDA(X)
083100950531     C                   GOTO      FINTUR
083200950525     C                   END
083300950531     C                   END
083400950531     C*
083500950525     C     ARBGC2        IFNE      *BLANKS
083600950531     C                   MOVEL     ARBGC2        WGIOR             1
083700950525     C     WGIOR         IFEQ      WGG
083800950525     C                   ADD       1             X
083900950525     C                   MOVEL     '11'          CDA(X)
084000950525     C                   END
084100950525     C                   END
084200950525     C*
084300950531     C     FINTUR        ENDSR
084400030721     C*--------------------------------------------------------------*
084500030721     C*  disalloco la distinta e stampo totali
084600030721     C*--------------------------------------------------------------*
084700030721     C     sblodis       BEGSR
084800030721     C     FVVFCF        IFeq      '§'
084900030721     C                   MOVEL     ' '           FVVFCF
085000030908     C                   EXCEPT    AGGFL1
085100030908     c                   commit
085200030909     c                   end
085300030825     C* STAMPA DISTINTA
085400101201     c                   if        d59ndt = fvvnfv  and
085500101201     c                             (d59sqa = ' ' or
085600101201     c                             d59sqa = 'T')
085700030909     c*
085800030909     C                   CLEAR                   FNLR55
085900030909     c* stampa scritta DISTINTA CON ANOMALIE
086000050426     C                   eval      D55bcd = d59bcd
086100050426     C                   MOVEL     D59ANM        D55ANM
086200030723     c*
086300030909     C                   Z-ADD     d59ddt        D55DAT
086400030909     C                   Z-ADD     d59ndt        D55NRD
086500030909     C                   Z-ADD     d59ndt        D55NRA
086600040930     C                   Z-ADD     d59pag        D55pag
086700040930     C                   Z-ADD     d59lin        D55lin
086800040930     C                   eval      d55nwp = 'N'
086900050426     C                   eval      d55lsr = d59lsr
087000030822     C* Solo anomalie
087100030721     C                   MOVEL     'A'           D55TIP
087200050204     C     D59Stp        ifeq      'O'
087300030822     C                   MOVEL     'E'           D55STP
087400030822     C                   Else
087500030822     C                   MOVEL     'C'           D55STP
087600030822     C                   ENDIF
087700030721     C                   MOVEL     D59FGS        D55FIL
087800041013     C                   Z-ADD     D59NBl        D55NBN
087900050207     C                   MOVEL     QDTFNBAF      D55fba
088000050207     C                   MOVEL     DSTNOT        D55NOT
088100030721     c* stampa solo totali
088200030822     c                   if        d59stp = 'A'
088300030721     C                   MOVEL     'S'           D55tot
088400050204     c* stampa anche la firma dell'aut
088500050204     C                   MOVEL     'S'           D55fir
088600030822     c                   end
088700030721      *--
088800030721     C                   MOVEL(p)  FNLR55        KPJBU
088900030721     C                   CALL      'FNLR55R'
089000030721     C                   PARM                    KPJBA
089100030825     c                   end
089200030721     c                   endsr
089300110208     C*--------------------------------------------------------------*
089400110208     C*  cancella la spunta + vecchia tra la 3 e la 4 se sono richiamato
089500110208     c* dal controllo a video
089600110208     C*--------------------------------------------------------------*
089700110208     C     srls46        BEGSR
089800110208     c                   clear                   canc4             1
089900110209     c                   setoff                                       22
090000110208     c* controllo se ho la spunta 3
090100110209     c                   do        10            xx
090200110209     c                   if        ima(xx) <> 0
090300110208     c     kbrv2         setll     fnbrv02l                               22
090400110209     c                   if        *in22
090500110209     c                   leave
090600110209     c                   end
090700110209     c                   else
090800110209     c                   leave
090900110209     c                   end
091000110209     c                   enddo
091100110208     c* trovo anche la spunta 3
091200110208     c                   if        *in22
091300140611     c                   clear                   fnls46ds1
091400110208     c                   clear                   kpjbu
091500110208     c                   eval      ILS46NPGA =  brvnpg
091600110208     c                   eval      ILS46NFVA =  brvnfv
091700110208     c                   eval      ILS46FGSA =  brvfgs
091800110208     c                   eval      ILS46NPGB =  3
091900110209     c                   eval      ILS46NFVB =  ima(xx)
092000110208     c                   eval      ILS46FGSB =  d59fgs
092100140611     c                   eval      ILS46dfvB =  d59ddt
092200140611     c                   eval      ILS46dfvA =  d59ddt
092300110208     c                   eval      ILS46AMB =  'QD'
092400110208     c                   eval      ILS46aupr=  'SE'
092500110208     c                   eval      ILS46FLS =  brvlnp
092600110208     c                   eval      ILS46LNA =  brvlna
092700110208     c                   eval      ILS46NRS =  brvnrs
092800110208     c                   eval      ILS46NSC =  brvnsc
092900110208     c                   eval      ILS46TELA = 'M'
093000110208     c*
093100150903     c*                  call      'FNLS46R1'
093200150903     c*                  parm                    kpjba
093300150903     c*                  parm                    fnls46ds1
093400110208     c* verifico se cancellata la spunta 3 o 4
093500110208     c                   if        ols46err = ' '
093600110208     c     kbrv2         setll     fnbrv02l                               22
093700110208     c* trovo la spunta 3 quindi ho cancellato la 4
093800110208     c                   if        *in22
093900110208     c                   eval      canc4 = '1'
094000110208     c                   end
094100110208     c                   end
094200110208     c                   end
094300110208     c*
094400110208     C                   ENDSR
094500950911     C*--------------------------------------------------------------*
094600950911     C*  Richiamo pgm x ultima volta
094700950911     C*--------------------------------------------------------------*
094800950911     C     CLOFIL        BEGSR
094900140430     c
095000140430     c                   clear                   fnlrv7ds
095100140430     C                   MOVEL     'C'           ILRV7TLA
095200140430     c                   movel(p)  fnlrv7ds      kpjbu
095300140430     c                   call      'FNLRV7R'
095400140430     c                   parm                    kpjba
095500140611     c*
095600140611     c                   clear                   fnls46ds1
095700140611     c                   movel     'C'           ils46tla
095800140611     c                   call      'FNLS46R1'
095900140611     c                   parm                    kpjba
096000140611     c                   parm                    fnls46ds1
096100950911     C*
096200950911     C                   MOVEL     'U'           D59TPR
096300950911     C                   MOVEL     'U'           D55TPR
096400030721     C                   MOVEL(p)  FNLR59        KPJBU
096500030708     C                   CALL      'FNLRF7R1'
096600950911     C                   PARM                    KPJBA
096700030723     C                   CALL      'FNLRF7R2'
096800950911     C                   PARM                    KPJBA
096900030723     C                   CALL      'FNLRF7R3'
097000950911     C                   PARM                    KPJBA
097100030723     C                   CALL      'FNLRF7R4'
097200030723     C                   PARM                    KPJBA
097300950911     C                   MOVEL     FNLR55        KPJBU
097400950911     C                   CALL      'FNLR55R'
097500950911     C                   PARM                    KPJBA
097600140430     c
097700030911     c* devo commitare di nuovo perchè posso aver scritto degli eventi nic
097800030911     c* chiudendo fnlrf7r2
097900030911     c                   commit
098000110103     C                   MOVEL(p)  FNLR59        KPJBU
098100950911     C*
098200950911     C                   ENDSR
098300030721     C*-----------------------------------------------------*
098400030721     C* stampa testata                                      *
098500030721     C*-----------------------------------------------------*
098600030721     C     srtesta       BEGSR
098700030731     c                   if        d59lin = 0 or d59lin > 54
098800030728     C* AUTOTRASPORTATORE
098900030728     C                   Z-ADD     APDPDR        PRTPDR            7 0
099000030728     C                   MOVEL(P)  APDRSC        PRTDSF           35
099100050422     C                   MOVEL(P)  APDRSC        PRTDSF2
099200030728     C* DISTINTA
099300030728     C                   Z-ADD     FVVNFV        PRTNDT            5 0
099400110209     C                   Z-ADD     FVVNFV        PRTNDT3
099500110209     C*                  eval      PRTNDT2 = d59bcd
099600110209     C                   move      d59bcd        PRTNDT2
099700030728     C                   Z-ADD     FVVFGS        PRTFGS            3 0
099800030728     C                   Z-ADD     FVVDFV        G02INV
099900030728     C                   MOVEL     '3'           G02ERR
100000030728     C                   CALL      'XSRDA8'
100100030728     C                   PARM                    WLBDA8
100200030728     C                   Z-ADD     G02DAT        STCDDC            8 0
100300030728     C* TIPO PRESTAZIONE
100400030721     C                   MOVEL     'S'           D59ANM
100500030724     C                   MOVEL     CMAT          TPPRE
100600030724     C     FVVFPP        IFEQ      'P'
100700030724     C                   MOVEL     CPOM          TPPRE            10
100800030724     C                   END
100900030725     c*
101000030728     C                   ADD       1             D59PAG
101100101201     c                   if        d59sqa = ' ' or
101200101201     c                             d59sqa = 'T'
101300030728     C                   WRITE     TESTA
101400050502     c                   if        d59lsr = 'S' and d59pag = 1
101500050422     C                   write     testal
101600050426     c                   end
101700030729     C                   WRITE     TESTA1
101800101125     c                   end
101900030728     C                   SETOFF                                       80
102000030728     C                   Z-ADD     6             D59LIN
102100030731     c                   end
102200030721     C                   ENDSR
102300950525     C*--------------------------------------------------------------*
102400950525     C*  Operazioni iniziali
102500950525     C*--------------------------------------------------------------*
102600950525     C     *INZSR        BEGSR
102700950525     C*
102800950525     C     *ENTRY        PLIST
102900950525     C                   PARM                    KPJBA
103000030721     C                   MOVEL(P)  KPJBU         FNLR59
103100030718     c*
103200030718     c     *dtaara       define    §azute        azuteds
103300030718     c     *dtaara       define    §datiute      ddatiute
103400030718     C                   in(E)     *dtaara
103500030718     C                   IF        %Error  or  RSUT = *blanks
103600030718     C                   call      'TIBS34R'
103700030718     C                   parm                    Tibs34Ds
103800030718     C                   in        *dtaara
103900030718     c                   ENDIF
104000050204     C* IMPOSTO L'INDICATORE PER STAMPARE IL TITOLO IN BASE AL PARAMETRO
104100050204     C* TIPO STAMPA A=CONTROLLO AUTOTRASPORTATORE " "/O= DISTINTA DI USCITA
104200050204     C* AUTOTRASPORTATORE
104300050204     C                   IF        D59STP = 'A'
104400050204     C                   SETON                                        61
104500050204     C                   SETOFF                                       62
104600050204     C                   ELSE
104700050204     C                   SETON                                        62
104800050204     C                   SETOFF                                       61
104900050204     C                   END
105000101124     c                   eval      tbecod = 'SQA'
105100101124     c                   eval      tbeke1 = nompgm
105200950525     C*------------------------------------------------------*
105300950525     C*  Definisco chiavi di accesso
105400101123     C     Ktbe          KLIST
105500101124     C                   KFLD                    tbecod
105600101124     C                   KFLD                    tbeke1
105700101124     C                   KFLD                    tbeke2
105800101124     C     Kwf10         KLIST
105900101124     C                   KFLD                    WLRQ1AAS
106000101124     C                   KFLD                    WLRQ1LNP
106100101124     C                   KFLD                    WLRQ1NRS
106200101124     C                   KFLD                    WLRQ1NSP
106300101124     C                   KFLD                    WLRQ1FGS
106400101124     C                   KFLD                    WLRQ1NDC
106500101124     C                   KFLD                    WLRQ1PDR
106600101123     C     Keyfvv        KLIST
106700101123     C                   KFLD                    KNPG
106800101123     C                   KFLD                    fvvnfv
106900101123     C                   KFLD                    KFGS
107000030908     C     KFVV1         KLIST
107100030908     C                   KFLD                    KNPG
107200030908     C                   KFLD                    KNFV
107300030908     C                   KFLD                    KFGS
107400110208     C     KFVV2         KLIST
107500110209     C                   KFLD                    KNPG3
107600110208     C                   KFLD                    Kdfv
107700110208     C                   KFLD                    KFGS
107800950525     C     KDST          KLIST
107900110518     C                   KFLD                    KNPG
108000110518     C                   KFLD                    KNFV1
108100950525     C                   KFLD                    KFGS
108200041013     C     KqdT          KLIST
108300041013     C                   KFLD                    KFGS
108400041013     C                   KFLD                    KNFV2
108500041013     C                   KFLD                    Kdfv
108600950525     C     KBRV          KLIST
108700950525     C                   KFLD                    KNPG
108800000114     C                   KFLD                    KNFV1
108900950525     C                   KFLD                    KFGS
109000110208     C     KBRV2         KLIST
109100110209     C                   KFLD                    KNPG3
109200110209     C                   KFLD                    ima(xx)
109300110209     C                   KFLD                    kfgs
109400110208     C                   KFLD                    brvlna
109500110208     C                   KFLD                    brvlnp
109600110208     C                   KFLD                    brvnrs
109700110208     C                   KFLD                    brvnsc
109800950525     C     KART          KLIST
109900110121     C                   KFLD                    KFLS
110000110121     C                   KFLD                    KLNA
110100110121     C                   KFLD                    KNRSa
110200110121     C                   KFLD                    KNSC
110300950525     C     KARB          KLIST
110400950525     C                   KFLD                    KAAS
110500950525     C                   KFLD                    KLNP
110600950525     C                   KFLD                    KNRS
110700950525     C                   KFLD                    KNSP
110800950525     C     KLBL          KLIST
110900950525     C                   KFLD                    KAAP
111000950525     C                   KFLD                    KLPP
111100950525     C                   KFLD                    KNRP
111200950529     C                   KFLD                    KNSP1
111300950525     C     KCLN          KLIST
111400950525     C                   KFLD                    KTFP
111500950525     C                   KFLD                    KTFA
111600950525     C                   KFLD                    KANN
111700950525     C                   KFLD                    KMES
111800950526     C     KTAB          KLIST
111900950526     C                   KFLD                    KKUT
112000950526     C                   KFLD                    KCOD
112100021204     C     Kapd          KLIST
112200021204     C                   KFLD                    apdtip
112300021204     C                   KFLD                    dstpdr
112400021204     c                   movel     'A'           apdtip
112500040713     C     Kapd2         KLIST
112600040713     C                   KFLD                    apdtip
112700040713     C                   KFLD                    arbpdc
112800950525     C*------------------------------------------------------*
112900950525     C*  Definisco variabili
113000030728     C     *LIKE         DEFINE    FVVNPG        KNPG
113100950525     C     *LIKE         DEFINE    FVVNFV        KNFV
113200000114     C     *LIKE         DEFINE    BRVNFV        KNFV1
113300041013     C     *LIKE         DEFINE    qdtNFV        KNFV2
113400950525     C     *LIKE         DEFINE    FVVFGS        KFGS
113500950525     C     *LIKE         DEFINE    FVVDFV        KDFV
113600950525     C     *LIKE         DEFINE    ARTFLS        KFLS
113700950525     C     *LIKE         DEFINE    ARTLNA        KLNA
113800950525     C     *LIKE         DEFINE    ARTNRS        KNRS
113900110121     C     *LIKE         DEFINE    ARTNRS        KNRSa
114000950525     C     *LIKE         DEFINE    ARTNSC        KNSC
114100950525     C     *LIKE         DEFINE    ARTAAS        KAAS
114200950525     C     *LIKE         DEFINE    ARTLNP        KLNP
114300950525     C     *LIKE         DEFINE    ARTNSP        KNSP
114400950529     C     *LIKE         DEFINE    ARTNSP        KNSP1
114500950525     C     *LIKE         DEFINE    LBLAAP        KAAP
114600950525     C     *LIKE         DEFINE    LBLLPP        KLPP
114700950525     C     *LIKE         DEFINE    LBLNRP        KNRP
114800950525     C     *LIKE         DEFINE    CLNTFP        KTFP
114900950525     C     *LIKE         DEFINE    CLNTFA        KTFA
115000950525     C     *LIKE         DEFINE    CLNANN        KANN
115100950525     C     *LIKE         DEFINE    CLNMES        KMES
115200950526     C     *LIKE         DEFINE    TBLKUT        KKUT
115300950526     C     *LIKE         DEFINE    TBLCOD        KCOD
115400950609     C     *LIKE         DEFINE    TBLKEY        KKEY
115500951121     C     *LIKE         DEFINE    ARBRAG        KRAG
115600950525     C     *LIKE         DEFINE    ARBFBC        OLDFBC
115700950529     C     *LIKE         DEFINE    ARBAAS        WAAS
115800950529     C     *LIKE         DEFINE    ARBLNP        WLNP
115900950529     C     *LIKE         DEFINE    ARBNRS        WNRS
116000950529     C     *LIKE         DEFINE    ARBNSP        WNSP
116100950525     C*------------------------------------------------------*
116200950525     C*  Controllo sul calendario il tipo di giorno che è la
116300950525     C*  data di consegna
116400950525     C                   Z-ADD     D59DDT        WMMGG             4 0
116500950525     C                   Z-ADD     D59DDT        WG                2 0
116600950525     C                   MOVEL     D59DDT        KANN
116700950525     C                   Z-ADD     0             KTFP
116800950525     C                   Z-ADD     0             KTFA
116900950525     C                   MOVEL     WMMGG         KMES
117000950525     C                   OPEN      AZCLN01L
117100950525     C     KCLN          CHAIN     AZCLN01L                           31
117200950525     C                   MOVEA     CLNMAT        MES
117300950525     C                   MOVE      MES(WG)       WGG               1
117400950525     C                   CLOSE     AZCLN01L
117500950525     C*------------------------------------------------------*
117600950525     C*  Inizializzo variabili
117700030728     C                   MOVEL     *ALL'-'       CMP132          198
117800030728     C                   MOVEL     *ALL'-'       CMP198          198
117900950525     C                   TIME                    TIMES             6 0
118000960927     C                   TIME                    WHDAT            12 0
118100960927     C                   MOVE      WHDAT         WOGGI             6 0
118200140512      *
118300140512      * è utilizzata solo per il giro degli STOP che viene condizionato
118400140512      *  dalle azioni che chiamano il programma.
118500140512     c                   if        kcdaz='LR59' or kcdaz='LR60'
118600140512      *
118700140512     c* Apertura condizionata del file degli STOP per ottimizzare le prestazioni.
118800140512     c* Se richiamato da azione LR59 o LR60 utilizza il calcolo degli STOP
118900140512     c*  e quindi Apriamo il file.
119000140512   <-c                   open      fnarbZ0F                             99
119100140512   <-c                   open      fiar601L                             99
119200140512      * Quindi
119300951211     C*  Carico dati tabella 3A
119400951211     C                   OPEN      TABEL00F
119500951211     C                   Z-ADD     1             KKUT
119600951211     C                   MOVEL     '3A'          KCOD
119700951211     C                   Z-ADD     0             X
119800951211     C     KTAB          CHAIN     TABEL00F                           31
119900951211     C     *IN31         DOWEQ     '0'
120000951211     C     X             ANDLT     100
120100951211     C                   ADD       1             X
120200951211     C                   MOVEL     TBLKEY        C3A(X)
120300951211     C                   MOVEL     TBLUNI        D3A(X)
120400951211     C     KTAB          READE     TABEL00F                               31
120500951211     C                   END
120600140512     C                   CLOSE     TABEL00F
120700140512      *
120800140512     c                   end
120900950609     C*  CARICO£6
121000020506     c                   clear                   trul06ds
121100020506     c                   eval      d06cod = '£6'
121200020506     c                   movel     d59fgs        d06key
121300040831     c                   move      'S'           d06key
121400040831     c                   eval      d06drf = d59ddt
121500030721     c                   movel(P)  trul06ds      kpjbu
121600020506     c                   call      'TRUL06R'
121700020506     c                   parm                    kpjba
121800030721     c                   movel(P)  kpjbu         trul06ds
121900020506     c                   movea     lin           £6
122000950531     C*
122100030728     C                   OPEN      FNLRF7P
122200030728     C                   Z-ADD     0             D59PAG
122300030728     C                   Z-ADD     0             D59LIN
122400110208     c* cerco il foglio ima
122500110209     C                   Z-ADD     3             KNPG3             1 0
122600110208     C                   Z-ADD     D59FGS        KFGS
122700110208     C                   Z-ADD     D59ddt        Kdfv
122800110209     c                   clear                   xi                2 0
122900110209     c                   clear                   ima
123000110208     C     KFVV2         setll     FNFVV02L
123100110208     c                   do        *hival
123200110208     C     KFVV2         reade     FNFVV02L
123300110208     c                   if        %eof(fnfvv02l)
123400110208     c                   leave
123500110208     c                   end
123600110208     c                   if        fvvspg = 'A'
123700110209     c                   add       1             xi
123800110209     c                   eval      ima(xi) = fvvnfv
123900110208     c                   end
124000110208     c                   enddo
124100950525     C*
124200950525     C                   ENDSR
124300950525     OFNARB000  E            AGGARB
124400031016     O*                      ARBcmc
124500031016     O*                      ARBfbc
124600030725     O                       ARBACC
124700030708     O                       ARBAMA
124800950613     OFNFVV001  E            AGGFL1
124900950613     O                       FVVFCF
