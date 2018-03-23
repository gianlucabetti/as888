000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS77R *----------------------------------------------------*
000300910207     H*         - RISTAMPA BOLLE                BATCH
000400000000     H*--------------------------------------------------------------*
000500010717     FFNBLP31L  IF   E           K DISK
000600020701     ffnblp76l  if   e           k disk    rename(fnblp000:fnblp076)
000700941228     FFNBLP01L  IF   E           K DISK
000800941228     F                                     RENAME(FNBLP000:FNBLP001)
000900090304     FFIART01L  IF   E           K DISK
001000990201     FTABEL00F  IF   E           K DISK
001100000201     FAZORG01L  IF   E           K DISK
001200020701
001300020701     d xx              s              2  0
001400030616     d wPagA4          s              5  0 inz
001500030616     d wPagA5          s              5  0 inz
001600030616     d wPagB0          s              5  0 inz
001700020701     d comlnp          s                   like(v1clnp)
001800050329     d pflg            s              1    inz
001900020701
002000941228     D PARAM           DS
002100941228     D  V1CLNP                 1      3  0
002200941228     D  V1CLNA                 4      6  0
002300941228     D  DSD                    7     14  0
002400941228     D  WDSA                  15     22  0
002500941228     D*
002600941228     D  V1CLPD                23     25  0
002700941228     D  V1CSED                26     27  0
002800941228     D  V1CNSD                28     34  0
002900000629     D  SECSPE                26     34  0
003000941228     D*
003100941228     D  V1CSEA                35     36  0
003200941228     D  V1CNSA                37     43  0
003300941228     D  V1CSPE                35     43  0
003400941228     D  V1CAAS                44     47  0
003500941228     D*
003600941228     D  VSCSOP                48     48
003700941228     D  VSCLNA                49     51  0
003800941228     D  VSCFLS                52     54  0
003900941228     D  VSCSCD                55     61  0
004000941228     D  VSCSCA                62     68  0
004100941228     D  VSCKSC                69     75  0
004200950116     D*
004300941228     D  VSCCPR                76     82  0
004400950116     D  VSCFPP                83     83
004500950116     D  VSCNRT                84     84
004600970701     D  VSCBAM                85     85
004700090304     d** non più usato poste non ci sono più
004800090304     D**VSCSNP                86     86
004900050329
005000050329     d  vldvcli               87     87
005100050329
005200071029     d* · num. limite di LdV stampabili (prima FISSO 6000)
005300071029     d  wVPOmaxLdV           151    155s 0 inz
005400950420     D* PARFLG = 'E' --> STAMPATE PIU' DI 6000 PAGINE
005500030613     D  PARFLG               156    156
005600030613     d* · pgm.   per stampa LDV   da TRUL90R:
005700030613     d  wD90psl              157    166
005800030616      *
005900030616      * DS per pgm di STAMPA BOLLE
006000030616     d FNLSB5DS      e ds                  inz
006100030616      *
006200910205     D KPJBA         E DS
006300990201     D DS3C          E DS
006400990201     D DS3A          E DS
006500000201     D OG143         E DS
006600020701
006700020701      *--------
006800020701      * Ds di riferimento al file esterno AzUte00F
006900020701      *--------
007000020701     d AzuteDs       e ds                  ExtName(AzUte00F)
007100020701      *--------
007200020701      * Ds per dati organigramma
007300020701      *--------
007400020701     d DDatiUte      e ds
007500020701      *--------
007600020701      * Parametri per richiamo al pgm di controllo profilo utenti
007700020701      *--------
007800020701     d Tibs34Ds      e ds                  Inz
007900020701      *--------
008000020701      * Parametri per richiamo al pgm trul06r
008100020701      *--------
008200020701     d Trul06Ds      e ds
008300020701     d  lin                    1     90  0 dim(30)
008400071029      *
008500071029      * - Parametri x ricerca/controllo tabelle
008600071029     d TIBS02ds      e ds                  inz
008700071029      * - Tab. VPO = Variabili Programmi Operativi
008800071029     d dVPO          e ds                  inz
008900071029     d  §VPOmaxLdV   e                     inz(10000)
009000020701
009100910220     C****************************************************************
009200910220     C*  RIEPILOGO INDICATORI
009300910220     C***************************************************************
009400000629     C* 01/13 - CONFRONTO DATI PASSATI
009500000629     C* 14    - NON CONTROLLO CAMPO BLPFST
009600000629     C* 30    - DI COMODO
009700910220     C* 31    - LETTURA SU FLBLP000
009800910220     C* 32    - DI COMODO
009900920421     C* 90    - ERRORE IN CONFRONTO BOLLA: DA SCARTARE
010000910220     C*****************************************************************
010100000000     C     *ENTRY        PLIST
010200000000     C                   PARM                    KPJBA
010300900511     C*---------------------------------------------------------------*
010400910207     C* ACCESSO   FLBLP                                    *
010500910207     C     KBLP          KLIST
010600941228     C                   KFLD                    V1CLNP
010700910207     C                   KFLD                    AAS
010800910207     C                   KFLD                    MGS
010900941228     C     KBLP2         KLIST
011000020701     c                   kfld                    comlnp
011100950331     C                   KFLD                    COMLNA
011200941228     C                   KFLD                    AAS
011300941228     C                   KFLD                    MGS
011400910207     C     KBLP3         KLIST
011500910207     C                   KFLD                    BLPAAS
011600910207     C                   KFLD                    BLPLNP
011700910207     C                   KFLD                    BLPNRS
011800910207     C                   KFLD                    BLPNSP
011900910903     C     KBLP4         KLIST
012000941228     C                   KFLD                    V1CAAS
012100941228     C                   KFLD                    V1CLPD
012200941228     C                   KFLD                    V1CSED
012300941228     C                   KFLD                    V1CNSD
012400910903     C     KBLP5         KLIST
012500941228     C                   KFLD                    V1CAAS
012600941228     C                   KFLD                    V1CLPD
012700020701     c     kblp6         klist
012800020701     c                   kfld                    comlnp
012900020701     c                   kfld                    comlna
013000020701
013100990201     C     KTAB          KLIST
013200990201     C                   KFLD                    CODUT             1 0
013300990201     C                   KFLD                    COD
013400990201     C                   KFLD                    KEY
013500950331     C*
013600950331     C     *LIKE         DEFINE    BLPLNA        COMLNA
013700990201     C     *LIKE         DEFINE    TBLCOD        COD
013800990201     C     *LIKE         DEFINE    TBLKEY        KEY
013900990201     C                   Z-ADD     1             CODUT
014000910205     C*---------------------------------------------------------------*
014100991116     C*
014200910205     C                   MOVEL     KPJBU         PARAM
014300020701
014400020701      * reperisco i dati utente
014500020701     c     *dtaara       define    §azute        AzuteDs
014600020701     c     *dtaara       define    §datiute      DDatiUte
014700020701
014800020701     c                   in(E)     *dtaara
014900020701     c                   IF        %Error or RSUT = *blanks
015000020701     c                   call      'TIBS34R'
015100020701     c                   parm                    Tibs34Ds
015200020701     c                   in        *dtaara
015300020701     c                   ENDIF
015400020701
015500921019     C*
015600910207     C* VEDO LE PARZIALIZZAZIONI EFFETTUATE
015700910207     C                   EXSR      PREPA
015800950331     C*
015900020701      * stampa per lna imposto la prima linea di partenza
016000020701     c                   if        *in11
016100020701     c                   z-add     1             xx
016200020701     c                   movel     lin(xx)       comlnp
016300020701     c                   endif
016400020701      * stampa per lna imposto la chiave se non richieste tutte
016500950331     C     *IN11         IFEQ      *ON
016600950331     C     V1CLNA        ANDEQ     999
016700950331     C* Scelte tutte le linee di arrivo: posizionamento sulla prima
016800020702     c     comlnp        setll     fnblp76l
016900020702     c                   read      fnblp76l                               31
017000020702     C  N31              Z-ADD     BLPLNA        COMLNA
017100950331     C                   ELSE
017200950331     C                   Z-ADD     V1CLNA        COMLNA
017300950331     C                   END
017400020701      * stampa per lnp e richiesta nelle parzializzazioni la lna imposto
017500020701      * la chiave
017600020701     c                   if        *in01 and *in06
017700020701     c                   movel     v1clnp        comlnp
017800020701     c                   movel     vsclna        comlna
017900020701     c                   endif
018000020701     C*
018100950331     C     *IN31         DOWEQ     *OFF
018200910207     C***
018300910903     C* PER NUMERO SPEDIZIONE
018400941228     C   12KBLP4         SETLL     FNBLP01L
018500020701
018600941228     C* PER LNP E  DATA   SPEDIZIONE
018700020701      * no lna nelle selezioni
018800020701     c  n06
018900020701     Can 01KBLP          SETLL     FNBLP31L
019000020701      * con lna nelle selezioni
019100020701     c   06
019200020701     Can 01kblp2         setll     fnblp76l
019300020701
019400941228     C* PER LNA E  DATA   SPEDIZIONE
019500020701     c   11kblp2         setll     fnblp76l
019600910903     C*
019700941228     C     *IN31         DOWEQ     *OFF
019800020701
019900910903     C* PER NUMERO SPEDIZIONE
020000941228     C   12
020100941228     CANN05KBLP4         READE     FNBLP01L                               31
020200941228     C   12
020300941228     CAN 05KBLP5         READE     FNBLP01L                               31
020400020701
020500941228     C* PER LNP E  DATA   SPEDIZIONE
020600020701      * no lna nelle selezioni
020700020701     c                   if        not *in06
020800010717     C   01
020900010717     CANN02KBLP          READE     FNBLP31L                               31
021000010717     C   01
021100010717     CAN 02V1CLNP        READE     FNBLP31L                               31
021200020701     c                   else
021300020701      * con lna nelle selezioni
021400020701     C   01
021500020701     CANN02kblp2         reade     fnblp76l                               31
021600020701     C   01
021700020701     CAN 02kblp6         reade     fnblp76l                               31
021800020701     c                   endif
021900020701
022000941228     C* PER LNA E  DATA   SPEDIZIONE
022100020701     c   11
022200020701     cann02kblp2         reade     fnblp76l                               31
022300020701     c   11
022400020701     can 02kblp6         reade     fnblp76l                               31
022500020701
022600910903     C*
022700910903     C* CON NUMERO AL VEDO SE COMPRESA
022800941228     C   12
022900941228     CAN 05
023000941228     CANN31              Z-ADD     BLPNSP        NUM9              9 0
023100941228     C   12
023200941228     CAN 05
023300941228     CANN31              MOVEL     BLPNRS        NUM9
023400941228     C   12
023500941228     CAN 05
023600941228     CANN31NUM9          IFGT      V1CSPE
023700910903     C                   SETON                                        31
023800910903     C                   END
023900910903     C*
024000910903     C* CON DATA   AL VEDO SE COMPRESA
024100941228     C  N12
024200941228     CAN 02
024300941228     CANN31              Z-ADD     BLPMGS        NUM8              8 0
024400941228     C  N12
024500941228     CAN 02
024600941228     CANN31              MOVEL     BLPAAS        NUM8
024700941228     C  N12
024800941228     CAN 02
024900941228     CANN31NUM8          IFGT      WDSA
025000910903     C                   SETON                                        31
025100910903     C                   END
025200941228     C*
025300910207     C* CONTROLLO LE PARZIALIZZAZIONI
025400910903     C  N31              EXSR      CONFR
025500941228     C*
025600000629     C  N31
025700000629     CANN90
025800000629     CAN 14              MOVE      '!'           BLPFST
025900050329
026000050329      * se stampa ldv x cliente forzo la stampa
026100050329     c                   If        vldvcli <> *Blanks
026200050329     c                   Eval      blpfst = '!'
026300050329     c                   EndIf
026400000629     C*
026500941228     C* S T A M P A   B O L L A
026600000629     C* SOLO SE L'HO GIA' STAMPATA O SE SCELTA SPEDIZIONE SECCA
026700000629     C* AVENDO TRASMESSO LA BOLLA IN ARRIVO
026800970227     C  N31
026900970227     CANN90BLPFST        IFNE      'N'
027000970227     C     BLPFST        ANDNE     ' '
027100030616     c                   clear                   FNLSB5DS
027200950419     C*
027300950419     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
027400950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
027500071029if  3c*** totale ?       if        wPagA4 + wPagA5 >= §VPOmaxLdV
027600071029if  3c                   if           wPagA4 >= §VPOmaxLdV
027700071029     c                             or wPagA5 >= §VPOmaxLdV
027800071029     c                             or wPagB0 >= §VPOmaxLdV
027900950419     C                   MOVEL     'E'           PARFLG
028000950419     C* DB0GES = 'R' INDICA CHE COME ULTIMA PAGINA VIENE STAMPATO UN
028100950419     C*   MESSAGGIO PER AVVISARE
028200950419     C                   MOVEL     'R'           DB0GES
028300030616e   3C                   endif
028400950419     C*
028500941228     C                   MOVEL     BLPAAS        DB0AAS
028600941228     C                   MOVEL     BLPLNP        DB0LNP
028700941228     C                   MOVEL     BLPNRS        DB0NRS
028800941228     C                   MOVEL     BLPNSP        DB0NSP
028900941228     C                   MOVEL     'P'           DB0TBO
029000941228     C                   MOVEL     'R'           DB0RIS
029100050331      * se stampa ldv x cliente forzo il flag db0ris con 'C'
029200050331      * per poi stampare la dicitura 'COPIA'
029300050331     c                   If        vldvcli <> *Blanks
029400050331     c                   Eval      db0ris = 'C'
029500050331     c                   EndIf
029600030616     c                   call      wD90psl
029700030616     C                   parm                    FNLSB5DS
029800030616      * Aggiorno il conteggio delle pagine stampate (x tipo)
029900030616sel 3c                   select
030000030616      * - formato A4 da FNLSB5R (stampante laser)
030100030616w   3c                   when      DB0tms = '4'
030200030616     c                   add       1             wPagA4
030300030616      * - formato A5 da FNLSB5R (stampante laser)
030400030616w   3c                   when      DB0tms = '5'
030500030616     c                   add       1             wPagA5
030600030616      * - altri..... da FNLSB0R (stampante ad aghi)
030700030616x   1c                   other
030800030616     c                   add       1             wPagB0
030900030616e   3c                   endsl
031000030616      *
031100941228     C                   ENDIF
031200941228     C*
031300950420     C* SE SUPERATE LE 6000 PAGINE (PARFLG='E'), VADO A FINE PGM
031400950419     C     PARFLG        IFEQ      'E'
031500950419     C                   GOTO      FINE
031600950419     C                   ENDIF
031700950419     C*
031800941228     C                   END
031900950331     C*
032000020702      * Richiesta di tutte le linee di arrivo
032100950331     C     *IN11         IFEQ      *ON
032200020701     C     V1CLNA        ANDEQ     999
032300020702      * cerco linea di arrivo sucessiva
032400020702     c     kblp6         setgt     fnblp76l
032500020702     c                   read      fnblp76l                               31
032600020702     c                   if        blplnp = comlnp
032700020702     c                   z-add     blplna        comlna
032800020702      * cerco la linea di partenza sucessiva
032900020702     c                   else
033000020701     c                   add       1             xx
033100020702      * non ci sono più linee di partenza vado a fine
033200020701     c                   if        lin(xx) = *zeros
033300020701     c                   goto      fine
033400020701     c                   endif
033500020702      * cerco la prima linea di arrivo della linea di partenza che
033600020702      * devo leggere
033700020701     c                   eval      comlnp = lin(xx)
033800020702     c     comlnp        setll     fnblp76l
033900020702     c                   read      fnblp76l                               31
034000020702     c  n31              z-add     blplna        comlna
034100020701     c                   endif
034200020701     C                   END
034300020702
034400020702      * richiesta solo una linea di arrivo
034500020702     c                   if        *in11 and v1clna <> 999
034600020702     c                   eval      *in31 = *off
034700020702      * cerco la linea di partenza sucessiva
034800020702     c                   add       1             xx
034900020702     c                   if        lin(xx) = *zeros
035000020702      * non ci sono più linee di partenza vado a fine
035100020702     c                   goto      fine
035200020702     c                   endif
035300020702     c                   eval      comlnp = lin(xx)
035400020702     c                   endif
035500950331     C*
035600950331     C                   ENDDO
035700921019     C*
035800950419     C     FINE          TAG
035900020701     C                   MOVEL(p)  PARAM         KPJBU
036000000000     C                   SETON                                        LR
036100910207     C*
036200910207     C*--- PREPARA/VEDO LE PARZIALIZZAZIONE FATTE -------------------*
036300910207     C     PREPA         BEGSR
036400941228     C**
036500941228     C* CONTROLLO COME DEVO LEGGERE
036600941228     C**
036700941228     C* PER LINEA DI PARTENZA
036800941228     C     V1CLNP        COMP      0                                  01
036900941228     C* PER LINEA DI ARRIVO
037000941228     C     V1CLNA        COMP      0                                  11
037100941228     C* PER NUMERO SPEDIZIONE
037200941228     C     V1CLPD        COMP      0                                  12
037300941228     C**
037400941228     C* CONTROLLO LE PARZIALIZZAZIONEI MESSE
037500941228     C**
037600941228     C* NUMERO SPEDIZIONE AL
037700941228     C     V1CNSA        COMP      0                                  05
037800910903     C* DATA AL
037900941228     C     WDSA          COMP      0                                  02
038000910207     C* SIGLA OPERATORE
038100941228     C     VSCSOP        COMP      *BLANKS                            0303
038200910207     C* LINEA DI ARRIVO
038300941228     C     VSCLNA        COMP      0                                  06
038400910207     C* SEGNACOLLO AL
038500941228     C     VSCSCA        COMP      0                                  08
038600910207     C* SEGNACOLLO DAL
038700941228     C  N08VSCSCD        COMP      0                                  07
038800941228     C* FILIALE SEGNACOLLO
038900941228     C     VSCFLS        COMP      0                                  17
039000910207     C* CODICE CLIENTE MITTENTE
039100941228     C     VSCKSC        COMP      0                                  09
039200910207     C* CODICE PADRONCINO
039300941228     C     VSCCPR        COMP      0                                  10
039400910207     C*
039500000629     C* C O N T R O L L O  SE STATA IMMESSA UNA SPEDIZIONE SECCA.
039600000629     C*
039700000629     C                   SETOFF                                       13
039800000629     C*
039900000629     C     *IN12         IFEQ      *ON
040000000629     C     V1CSPE        IFEQ      0
040100000629     C     V1CSPE        OREQ      SECSPE
040200000629     C                   SETON                                        13
040300000629     C                   ENDIF
040400000629     C                   ENDIF
040500000629     C*
040600000629     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
040700000629     C*
040800941228     C                   MOVEL     DSD           AAS               4 0
040900920320     C                   MOVE      DSD           MGS               4 0
041000910903     C                   SETOFF                                       31
041100020701      * stampa per lna
041200020701     c                   if        *in11
041300020701      * se p.o. di secondo livello imposto nella sk delle lnp se
041400020701      * stesso
041500020701     c                   clear                   lin
041600020701     c                   if        dutlpo = '2' or dutlpo = *blanks
041700020701     c                   eval      lin(1) = dutpou
041800020701      * se p.o. di primo livello carico tutti i p.o. del terminal di partenza
041900020701     c                   else
042000020701     c                   clear                   Trul06Ds
042100020701     c                   eval      D06Cod = '£1'
042200020701     c                   eval      D06Tla = 'L'
042300020701     c                   movel     Simfel        D06Key
042400020701     c                   movel(p)  Trul06Ds      Kpjbu
042500020701     c                   call      'TRUL06R'
042600020701     c                   parm                    Kpjba
042700020701     c                   movel     Kpjbu         Trul06Ds
042800020701     c                   endif
042900020701     c                   endif
043000071029      *
043100071029      * Aggancio tabella "VPO" per reperimento il n° LdV stampabili
043200071029      *   limite (per congelamento e blocco stampa)
043300071029     c                   reset                   dVPO
043400071029     c                   clear                   TIBS02ds
043500071029     c                   eval      T02tla  = 'L'
043600071029     c                   eval      T02mod  = 'C'
043700071029     c                   eval      T02sif  = KNSIF
043800071029     c                   eval      T02cod  = 'VPO'
043900071029     c                   movel(P)  'VPO'         T02ke1
044000071029     c                   call      'TIBS02R'
044100071029     c                   parm                    KPJBA
044200071029     c                   parm                    TIBS02ds
044300071029     c                   if        T02err  = *blanks
044400071029     c                   movel     T02uni        dVPO
044500071029     c                   endif
044600071029     c                   eval      wVPOmaxLdV = §VPOmaxLdV
044700950419     C*
044800030616      * Azzero i contatori delle pagine stampate
044900030616      *  (formati A4 e A5 - laser - ed altri - ad aghi)
045000030616     c                   clear                   wPagA4
045100030616     c                   clear                   wPagA5
045200030616     c                   clear                   wPagB0
045300910207     C                   ENDSR
045400910207     C*
045500910207     C*--- CONFRONTO PARZIALIZZAZIONI FATTE -------------------------*
045600910207     C     CONFR         BEGSR
045700000629     C                   SETOFF                                       9014
045800000629      * SE 14=ON RISTAMPO ANCHE SE NON E' STATA STAMPATA
045900000629     C   13BLPFT1        IFNE      *BLANKS
046000000629     C                   SETON                                        14
046100000629     C                   ENDIF
046200991012     C* ESCLUDO SEMPRE I PREPGATI NON CONFERMATI
046300991012     C     BLPPDR        IFEQ      0
046400991012     C                   SETON                                        90
046500991012     C                   GOTO      ENDCON
046600991012     C                   END
046700000201     C* SIGLA OPERATORE
046800941228     C   03BLPSOP        IFNE      VSCSOP
046900910207     C                   SETON                                        90
047000910207     C                   GOTO      ENDCON
047100910207     C                   END
047200910207     C* LINEA ARRIVO
047300941228     C   06BLPLNA        IFNE      VSCLNA
047400910207     C                   SETON                                        90
047500910207     C                   GOTO      ENDCON
047600910207     C                   END
047700941228     C* FILIALE SEGNACOLLO
047800941228     C   17BLPFLS        IFNE      VSCFLS
047900941228     C                   SETON                                        90
048000941228     C                   GOTO      ENDCON
048100941228     C                   END
048200910903     C* SEGNACOLLI DAL  AL
048300941228     C   08
048400910903     COR 07              DO
048500910903     C                   SETON                                        90
048600090304     C     KBLP3         SETLL     FiART01L
048700090304     C     KBLP3         READE     FiART01L                               32
048800910903     C     *IN32         DOWEQ     '0'
048900910903     C* DAL
049000090304     C   07ARTNSC        IFEQ      VSCSCD
049100910903     C                   SETOFF                                       90
049200910903     C                   SETON                                        32
049300910903     C                   END
049400910903     C* DAL + AL
049500090304     C   08ARTNSC        IFGE      VSCSCD
049600090304     C     ARTNSC        ANDLE     VSCSCA
049700910903     C                   SETOFF                                       90
049800910903     C                   SETON                                        32
049900910903     C                   END
050000090304     C  N32KBLP3         READE     FIART01L                               32
050100910903     C                   END
050200910903     C*
050300910903     C   90              GOTO      ENDCON
050400910903     C                   END
050500910903     C*
050600910207     C* CODICE CLIENTE MITTENTE
050700910207     C   09BLPCCM        IFNE      0
050800941228     C     BLPCCM        IFNE      VSCKSC
050900910207     C                   SETON                                            90
051000910207     C                   GOTO      ENDCON
051100910207     C                   END
051200910207     C                   ELSE
051300941228     C     BLPKSC        IFNE      VSCKSC
051400910207     C                   SETON                                            90
051500910207     C                   GOTO      ENDCON
051600910207     C                   END
051700910207     C                   END
051800910207     C* CODICE PADRONCINO
051900941228     C   10BLPPDR        IFNE      VSCCPR
052000910207     C                   SETON                                            90
052100910207     C                   GOTO      ENDCON
052200910207     C                   END
052300950116     C* RITIRO MATTINA/POMERIGGIO
052400950116     C     VSCFPP        IFNE      ' '
052500950116     C     BLPFPP        ANDNE     VSCFPP
052600950116     C                   SETON                                            90
052700950116     C                   GOTO      ENDCON
052800950116     C                   END
052900950116     C* NUMERO RITIRO
053000950116     C     VSCNRT        IFNE      ' '
053100950116     C                   MOVEL     BLPNRT        W001A             1
053200950116     C     W001A         IFNE      VSCNRT
053300950116     C                   SETON                                            90
053400950116     C                   GOTO      ENDCON
053500950116     C                   END
053600950116     C                   END
053700970701     C* SI NO DDT
053800000629     C  N14VSCBAM        IFNE      ' '
053900970701     C     VSCBAM        IFEQ      'S'
054000970701     C     BLPFST        IFEQ      'N'
054100970701     C     BLPFST        OREQ      'Y'
054200970701     C     BLPFST        OREQ      'K'
054300970701     C                   SETON                                            90
054400970701     C                   GOTO      ENDCON
054500970701     C                   END
054600970701     C                   END
054700970701     C     VSCBAM        IFEQ      'N'
054800970701     C     BLPFST        IFEQ      'S'
054900970701     C     BLPFST        OREQ      'C'
055000970701     C                   SETON                                            90
055100970701     C                   GOTO      ENDCON
055200970701     C                   END
055300970701     C                   END
055400970701     C                   END
055500000201     C*
055600000201     C* ESCLUDO LE BOLLE EXPORT DPD
055700000201     C     BLPLNA        CHAIN     AZORG01L                           30
055800000201     C                   MOVEL     ORGDE3        OG143
055900020206     C     §OGNTW        IFEQ      'DPD'
056000000201     C                   SETON                                        90
056100000201     C                   GOTO      ENDCON
056200000201     C                   END
056300000201     C* ESCLUDO LE BOLLE EUROEXPRESS NO DDT
056400020206     C  N14§OGNTW        IFEQ      'EEX'
056500000201     C     BLPFST        IFEQ      'N'
056600000201     C     BLPFST        OREQ      'Y'
056700000201     C     BLPFST        OREQ      'K'
056800000201     C                   SETON                                        90
056900000201     C                   GOTO      ENDCON
057000000201     C                   END
057100000201     C                   END
057200990201     C**
057300990201     C* VEDO SE BOLLA RISTAMPABILE
057400990201    1C     BLPNRS        IFGT      0
057500990201     C* VEDO SE BOLLA FRANCO O ASSEGNATO
057600990201     C                   MOVEL     '3A'          COD
057700990201     C                   MOVEL(P)  BLPCBO        KEY
057800990201     C     KTAB          CHAIN     TABEL                              30
057900990201    2C     *IN30         IFEQ      *OFF
058000990201     C                   MOVEL     TBLUNI        DS3A
058100990201     C                   ELSE
058200990201     C                   CLEAR                   DS3A
058300990201    2C                   ENDIF
058400990201     C                   MOVEL     §3ATB1        W001A             1
058500990201     C* ASSEGNATO
058600990201    2C     W001A         IFEQ      'A'
058700990201     C                   MOVEL(P)  BLPCCM        KEY
058800990201     C                   ELSE
058900990201     C* FRANCO
059000990201     C                   MOVEL(P)  BLPKSC        KEY
059100990201    2C                   ENDIF
059200990201     C**
059300990203     C                   MOVEL     '3C'          COD
059400990201     C     KTAB          CHAIN     TABEL                              30
059500990201    2C     *IN30         IFEQ      *OFF
059600990201     C                   MOVEL     TBLUNI        DS3C
059700990201     C                   ELSE
059800990201     C                   CLEAR                   DS3C
059900990201    2C                   ENDIF
060000990201     C* NON RISTAMPABILE
060100990201    3C     §3CRSB        IFEQ      'N'
060200990201     C                   SETON                                            90
060300990201     C                   GOTO      ENDCON
060400990201     C                   ENDIF
060500990201     C                   ENDIF
060600000614      * SI/NO/SOLO POSTE
060700090304     C**                 CLEAR                   OG143
060800090304     C**   BLPLNP        CHAIN     AZORG                              30
060900090304     C**N30              MOVEL     ORGDE3        OG143
061000000614      *
061100090304     C**   VSCSNP        IFEQ      'N'
061200090304     C*+   §OGNTW        ANDEQ     'PPT'
061300090304     C**                 SETON                                            90
061400090304     C**                 GOTO      ENDCON
061500090304     C**                 ENDIF
061600000614      *
061700090304     C**   VSCSNP        IFEQ      'P'
061800090304     C**   §OGNTW        ANDNE     'PPT'
061900090304     C**                 SETON                                            90
062000090304     C**                 GOTO      ENDCON
062100090304     C**                 ENDIF
062200910207     C*
062300910207     C     ENDCON        ENDSR
