000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS77R *----------------------------------------------------*
000300910207     H*         - RISTAMPA BOLLE                BATCH
000400000000     H*--------------------------------------------------------------*
000500010717     FFNBLP31L  IF   E           K DISK
000600020701     ffnblp76l  if   e           k disk    rename(fnblp000:fnblp076)
000700941228     FFNBLP01L  IF   E           K DISK
000800941228     F                                     RENAME(FNBLP000:FNBLP001)
000900941228     FFNBLT01L  IF   E           K DISK
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
004700000614     D  VSCSNP                86     86
004800050329
004900050329     d  vldvcli               87     87
005000050329
005100071029     d* · num. limite di LdV stampabili (prima FISSO 6000)
005200071029     d  wVPOmaxLdV           151    155s 0 inz
005300950420     D* PARFLG = 'E' --> STAMPATE PIU' DI 6000 PAGINE
005400030613     D  PARFLG               156    156
005500030613     d* · pgm.   per stampa LDV   da TRUL90R:
005600030613     d  wD90psl              157    166
005700030616      *
005800030616      * DS per pgm di STAMPA BOLLE
005900030616     d FNLSB5DS      e ds                  inz
006000030616      *
006100910205     D KPJBA         E DS
006200990201     D DS3C          E DS
006300990201     D DS3A          E DS
006400000201     D OG143         E DS
006500020701
006600020701      *--------
006700020701      * Ds di riferimento al file esterno AzUte00F
006800020701      *--------
006900020701     d AzuteDs       e ds                  ExtName(AzUte00F)
007000020701      *--------
007100020701      * Ds per dati organigramma
007200020701      *--------
007300020701     d DDatiUte      e ds
007400020701      *--------
007500020701      * Parametri per richiamo al pgm di controllo profilo utenti
007600020701      *--------
007700020701     d Tibs34Ds      e ds                  Inz
007800020701      *--------
007900020701      * Parametri per richiamo al pgm trul06r
008000020701      *--------
008100020701     d Trul06Ds      e ds
008200020701     d  lin                    1     90  0 dim(30)
008300071029      *
008400071029      * - Parametri x ricerca/controllo tabelle
008500071029     d TIBS02ds      e ds                  inz
008600071029      * - Tab. VPO = Variabili Programmi Operativi
008700071029     d dVPO          e ds                  inz
008800071029     d  §VPOmaxLdV   e                     inz(10000)
008900020701
009000910220     C****************************************************************
009100910220     C*  RIEPILOGO INDICATORI
009200910220     C***************************************************************
009300000629     C* 01/13 - CONFRONTO DATI PASSATI
009400000629     C* 14    - NON CONTROLLO CAMPO BLPFST
009500000629     C* 30    - DI COMODO
009600910220     C* 31    - LETTURA SU FLBLP000
009700910220     C* 32    - DI COMODO
009800920421     C* 90    - ERRORE IN CONFRONTO BOLLA: DA SCARTARE
009900910220     C*****************************************************************
010000000000     C     *ENTRY        PLIST
010100000000     C                   PARM                    KPJBA
010200900511     C*---------------------------------------------------------------*
010300910207     C* ACCESSO   FLBLP                                    *
010400910207     C     KBLP          KLIST
010500941228     C                   KFLD                    V1CLNP
010600910207     C                   KFLD                    AAS
010700910207     C                   KFLD                    MGS
010800941228     C     KBLP2         KLIST
010900020701     c                   kfld                    comlnp
011000950331     C                   KFLD                    COMLNA
011100941228     C                   KFLD                    AAS
011200941228     C                   KFLD                    MGS
011300910207     C     KBLP3         KLIST
011400910207     C                   KFLD                    BLPAAS
011500910207     C                   KFLD                    BLPLNP
011600910207     C                   KFLD                    BLPNRS
011700910207     C                   KFLD                    BLPNSP
011800910903     C     KBLP4         KLIST
011900941228     C                   KFLD                    V1CAAS
012000941228     C                   KFLD                    V1CLPD
012100941228     C                   KFLD                    V1CSED
012200941228     C                   KFLD                    V1CNSD
012300910903     C     KBLP5         KLIST
012400941228     C                   KFLD                    V1CAAS
012500941228     C                   KFLD                    V1CLPD
012600020701     c     kblp6         klist
012700020701     c                   kfld                    comlnp
012800020701     c                   kfld                    comlna
012900020701
013000990201     C     KTAB          KLIST
013100990201     C                   KFLD                    CODUT             1 0
013200990201     C                   KFLD                    COD
013300990201     C                   KFLD                    KEY
013400950331     C*
013500950331     C     *LIKE         DEFINE    BLPLNA        COMLNA
013600990201     C     *LIKE         DEFINE    TBLCOD        COD
013700990201     C     *LIKE         DEFINE    TBLKEY        KEY
013800990201     C                   Z-ADD     1             CODUT
013900910205     C*---------------------------------------------------------------*
014000991116     C*
014100910205     C                   MOVEL     KPJBU         PARAM
014200020701
014300020701      * reperisco i dati utente
014400020701     c     *dtaara       define    §azute        AzuteDs
014500020701     c     *dtaara       define    §datiute      DDatiUte
014600020701
014700020701     c                   in(E)     *dtaara
014800020701     c                   IF        %Error or RSUT = *blanks
014900020701     c                   call      'TIBS34R'
015000020701     c                   parm                    Tibs34Ds
015100020701     c                   in        *dtaara
015200020701     c                   ENDIF
015300020701
015400921019     C*
015500910207     C* VEDO LE PARZIALIZZAZIONI EFFETTUATE
015600910207     C                   EXSR      PREPA
015700950331     C*
015800020701      * stampa per lna imposto la prima linea di partenza
015900020701     c                   if        *in11
016000020701     c                   z-add     1             xx
016100020701     c                   movel     lin(xx)       comlnp
016200020701     c                   endif
016300020701      * stampa per lna imposto la chiave se non richieste tutte
016400950331     C     *IN11         IFEQ      *ON
016500950331     C     V1CLNA        ANDEQ     999
016600950331     C* Scelte tutte le linee di arrivo: posizionamento sulla prima
016700020702     c     comlnp        setll     fnblp76l
016800020702     c                   read      fnblp76l                               31
016900020702     C  N31              Z-ADD     BLPLNA        COMLNA
017000950331     C                   ELSE
017100950331     C                   Z-ADD     V1CLNA        COMLNA
017200950331     C                   END
017300020701      * stampa per lnp e richiesta nelle parzializzazioni la lna imposto
017400020701      * la chiave
017500020701     c                   if        *in01 and *in06
017600020701     c                   movel     v1clnp        comlnp
017700020701     c                   movel     vsclna        comlna
017800020701     c                   endif
017900020701     C*
018000950331     C     *IN31         DOWEQ     *OFF
018100910207     C***
018200910903     C* PER NUMERO SPEDIZIONE
018300941228     C   12KBLP4         SETLL     FNBLP01L
018400020701
018500941228     C* PER LNP E  DATA   SPEDIZIONE
018600020701      * no lna nelle selezioni
018700020701     c  n06
018800020701     Can 01KBLP          SETLL     FNBLP31L
018900020701      * con lna nelle selezioni
019000020701     c   06
019100020701     Can 01kblp2         setll     fnblp76l
019200020701
019300941228     C* PER LNA E  DATA   SPEDIZIONE
019400020701     c   11kblp2         setll     fnblp76l
019500910903     C*
019600941228     C     *IN31         DOWEQ     *OFF
019700020701
019800910903     C* PER NUMERO SPEDIZIONE
019900941228     C   12
020000941228     CANN05KBLP4         READE     FNBLP01L                               31
020100941228     C   12
020200941228     CAN 05KBLP5         READE     FNBLP01L                               31
020300020701
020400941228     C* PER LNP E  DATA   SPEDIZIONE
020500020701      * no lna nelle selezioni
020600020701     c                   if        not *in06
020700010717     C   01
020800010717     CANN02KBLP          READE     FNBLP31L                               31
020900010717     C   01
021000010717     CAN 02V1CLNP        READE     FNBLP31L                               31
021100020701     c                   else
021200020701      * con lna nelle selezioni
021300020701     C   01
021400020701     CANN02kblp2         reade     fnblp76l                               31
021500020701     C   01
021600020701     CAN 02kblp6         reade     fnblp76l                               31
021700020701     c                   endif
021800020701
021900941228     C* PER LNA E  DATA   SPEDIZIONE
022000020701     c   11
022100020701     cann02kblp2         reade     fnblp76l                               31
022200020701     c   11
022300020701     can 02kblp6         reade     fnblp76l                               31
022400020701
022500910903     C*
022600910903     C* CON NUMERO AL VEDO SE COMPRESA
022700941228     C   12
022800941228     CAN 05
022900941228     CANN31              Z-ADD     BLPNSP        NUM9              9 0
023000941228     C   12
023100941228     CAN 05
023200941228     CANN31              MOVEL     BLPNRS        NUM9
023300941228     C   12
023400941228     CAN 05
023500941228     CANN31NUM9          IFGT      V1CSPE
023600910903     C                   SETON                                        31
023700910903     C                   END
023800910903     C*
023900910903     C* CON DATA   AL VEDO SE COMPRESA
024000941228     C  N12
024100941228     CAN 02
024200941228     CANN31              Z-ADD     BLPMGS        NUM8              8 0
024300941228     C  N12
024400941228     CAN 02
024500941228     CANN31              MOVEL     BLPAAS        NUM8
024600941228     C  N12
024700941228     CAN 02
024800941228     CANN31NUM8          IFGT      WDSA
024900910903     C                   SETON                                        31
025000910903     C                   END
025100941228     C*
025200910207     C* CONTROLLO LE PARZIALIZZAZIONI
025300910903     C  N31              EXSR      CONFR
025400941228     C*
025500000629     C  N31
025600000629     CANN90
025700000629     CAN 14              MOVE      '!'           BLPFST
025800050329
025900050329      * se stampa ldv x cliente forzo la stampa
026000050329     c                   If        vldvcli <> *Blanks
026100050329     c                   Eval      blpfst = '!'
026200050329     c                   EndIf
026300000629     C*
026400941228     C* S T A M P A   B O L L A
026500000629     C* SOLO SE L'HO GIA' STAMPATA O SE SCELTA SPEDIZIONE SECCA
026600000629     C* AVENDO TRASMESSO LA BOLLA IN ARRIVO
026700970227     C  N31
026800970227     CANN90BLPFST        IFNE      'N'
026900970227     C     BLPFST        ANDNE     ' '
027000030616     c                   clear                   FNLSB5DS
027100950419     C*
027200950419     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
027300950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
027400071029if  3c*** totale ?       if        wPagA4 + wPagA5 >= §VPOmaxLdV
027500071029if  3c                   if           wPagA4 >= §VPOmaxLdV
027600071029     c                             or wPagA5 >= §VPOmaxLdV
027700071029     c                             or wPagB0 >= §VPOmaxLdV
027800950419     C                   MOVEL     'E'           PARFLG
027900950419     C* DB0GES = 'R' INDICA CHE COME ULTIMA PAGINA VIENE STAMPATO UN
028000950419     C*   MESSAGGIO PER AVVISARE
028100950419     C                   MOVEL     'R'           DB0GES
028200030616e   3C                   endif
028300950419     C*
028400941228     C                   MOVEL     BLPAAS        DB0AAS
028500941228     C                   MOVEL     BLPLNP        DB0LNP
028600941228     C                   MOVEL     BLPNRS        DB0NRS
028700941228     C                   MOVEL     BLPNSP        DB0NSP
028800941228     C                   MOVEL     'P'           DB0TBO
028900941228     C                   MOVEL     'R'           DB0RIS
029000050331      * se stampa ldv x cliente forzo il flag db0ris con 'C'
029100050331      * per poi stampare la dicitura 'COPIA'
029200050331     c                   If        vldvcli <> *Blanks
029300050331     c                   Eval      db0ris = 'C'
029400050331     c                   EndIf
029500030616     c                   call      wD90psl
029600030616     C                   parm                    FNLSB5DS
029700030616      * Aggiorno il conteggio delle pagine stampate (x tipo)
029800030616sel 3c                   select
029900030616      * - formato A4 da FNLSB5R (stampante laser)
030000030616w   3c                   when      DB0tms = '4'
030100030616     c                   add       1             wPagA4
030200030616      * - formato A5 da FNLSB5R (stampante laser)
030300030616w   3c                   when      DB0tms = '5'
030400030616     c                   add       1             wPagA5
030500030616      * - altri..... da FNLSB0R (stampante ad aghi)
030600030616x   1c                   other
030700030616     c                   add       1             wPagB0
030800030616e   3c                   endsl
030900030616      *
031000941228     C                   ENDIF
031100941228     C*
031200950420     C* SE SUPERATE LE 6000 PAGINE (PARFLG='E'), VADO A FINE PGM
031300950419     C     PARFLG        IFEQ      'E'
031400950419     C                   GOTO      FINE
031500950419     C                   ENDIF
031600950419     C*
031700941228     C                   END
031800950331     C*
031900020702      * Richiesta di tutte le linee di arrivo
032000950331     C     *IN11         IFEQ      *ON
032100020701     C     V1CLNA        ANDEQ     999
032200020702      * cerco linea di arrivo sucessiva
032300020702     c     kblp6         setgt     fnblp76l
032400020702     c                   read      fnblp76l                               31
032500020702     c                   if        blplnp = comlnp
032600020702     c                   z-add     blplna        comlna
032700020702      * cerco la linea di partenza sucessiva
032800020702     c                   else
032900020701     c                   add       1             xx
033000020702      * non ci sono più linee di partenza vado a fine
033100020701     c                   if        lin(xx) = *zeros
033200020701     c                   goto      fine
033300020701     c                   endif
033400020702      * cerco la prima linea di arrivo della linea di partenza che
033500020702      * devo leggere
033600020701     c                   eval      comlnp = lin(xx)
033700020702     c     comlnp        setll     fnblp76l
033800020702     c                   read      fnblp76l                               31
033900020702     c  n31              z-add     blplna        comlna
034000020701     c                   endif
034100020701     C                   END
034200020702
034300020702      * richiesta solo una linea di arrivo
034400020702     c                   if        *in11 and v1clna <> 999
034500020702     c                   eval      *in31 = *off
034600020702      * cerco la linea di partenza sucessiva
034700020702     c                   add       1             xx
034800020702     c                   if        lin(xx) = *zeros
034900020702      * non ci sono più linee di partenza vado a fine
035000020702     c                   goto      fine
035100020702     c                   endif
035200020702     c                   eval      comlnp = lin(xx)
035300020702     c                   endif
035400950331     C*
035500950331     C                   ENDDO
035600921019     C*
035700950419     C     FINE          TAG
035800020701     C                   MOVEL(p)  PARAM         KPJBU
035900000000     C                   SETON                                        LR
036000910207     C*
036100910207     C*--- PREPARA/VEDO LE PARZIALIZZAZIONE FATTE -------------------*
036200910207     C     PREPA         BEGSR
036300941228     C**
036400941228     C* CONTROLLO COME DEVO LEGGERE
036500941228     C**
036600941228     C* PER LINEA DI PARTENZA
036700941228     C     V1CLNP        COMP      0                                  01
036800941228     C* PER LINEA DI ARRIVO
036900941228     C     V1CLNA        COMP      0                                  11
037000941228     C* PER NUMERO SPEDIZIONE
037100941228     C     V1CLPD        COMP      0                                  12
037200941228     C**
037300941228     C* CONTROLLO LE PARZIALIZZAZIONEI MESSE
037400941228     C**
037500941228     C* NUMERO SPEDIZIONE AL
037600941228     C     V1CNSA        COMP      0                                  05
037700910903     C* DATA AL
037800941228     C     WDSA          COMP      0                                  02
037900910207     C* SIGLA OPERATORE
038000941228     C     VSCSOP        COMP      *BLANKS                            0303
038100910207     C* LINEA DI ARRIVO
038200941228     C     VSCLNA        COMP      0                                  06
038300910207     C* SEGNACOLLO AL
038400941228     C     VSCSCA        COMP      0                                  08
038500910207     C* SEGNACOLLO DAL
038600941228     C  N08VSCSCD        COMP      0                                  07
038700941228     C* FILIALE SEGNACOLLO
038800941228     C     VSCFLS        COMP      0                                  17
038900910207     C* CODICE CLIENTE MITTENTE
039000941228     C     VSCKSC        COMP      0                                  09
039100910207     C* CODICE PADRONCINO
039200941228     C     VSCCPR        COMP      0                                  10
039300910207     C*
039400000629     C* C O N T R O L L O  SE STATA IMMESSA UNA SPEDIZIONE SECCA.
039500000629     C*
039600000629     C                   SETOFF                                       13
039700000629     C*
039800000629     C     *IN12         IFEQ      *ON
039900000629     C     V1CSPE        IFEQ      0
040000000629     C     V1CSPE        OREQ      SECSPE
040100000629     C                   SETON                                        13
040200000629     C                   ENDIF
040300000629     C                   ENDIF
040400000629     C*
040500000629     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
040600000629     C*
040700941228     C                   MOVEL     DSD           AAS               4 0
040800920320     C                   MOVE      DSD           MGS               4 0
040900910903     C                   SETOFF                                       31
041000020701      * stampa per lna
041100020701     c                   if        *in11
041200020701      * se p.o. di secondo livello imposto nella sk delle lnp se
041300020701      * stesso
041400020701     c                   clear                   lin
041500020701     c                   if        dutlpo = '2' or dutlpo = *blanks
041600020701     c                   eval      lin(1) = dutpou
041700020701      * se p.o. di primo livello carico tutti i p.o. del terminal di partenza
041800020701     c                   else
041900020701     c                   clear                   Trul06Ds
042000020701     c                   eval      D06Cod = '£1'
042100020701     c                   eval      D06Tla = 'L'
042200020701     c                   movel     Simfel        D06Key
042300020701     c                   movel(p)  Trul06Ds      Kpjbu
042400020701     c                   call      'TRUL06R'
042500020701     c                   parm                    Kpjba
042600020701     c                   movel     Kpjbu         Trul06Ds
042700020701     c                   endif
042800020701     c                   endif
042900071029      *
043000071029      * Aggancio tabella "VPO" per reperimento il n° LdV stampabili
043100071029      *   limite (per congelamento e blocco stampa)
043200071029     c                   reset                   dVPO
043300071029     c                   clear                   TIBS02ds
043400071029     c                   eval      T02tla  = 'L'
043500071029     c                   eval      T02mod  = 'C'
043600071029     c                   eval      T02sif  = KNSIF
043700071029     c                   eval      T02cod  = 'VPO'
043800071029     c                   movel(P)  'VPO'         T02ke1
043900071029     c                   call      'TIBS02R'
044000071029     c                   parm                    KPJBA
044100071029     c                   parm                    TIBS02ds
044200071029     c                   if        T02err  = *blanks
044300071029     c                   movel     T02uni        dVPO
044400071029     c                   endif
044500071029     c                   eval      wVPOmaxLdV = §VPOmaxLdV
044600950419     C*
044700030616      * Azzero i contatori delle pagine stampate
044800030616      *  (formati A4 e A5 - laser - ed altri - ad aghi)
044900030616     c                   clear                   wPagA4
045000030616     c                   clear                   wPagA5
045100030616     c                   clear                   wPagB0
045200910207     C                   ENDSR
045300910207     C*
045400910207     C*--- CONFRONTO PARZIALIZZAZIONI FATTE -------------------------*
045500910207     C     CONFR         BEGSR
045600000629     C                   SETOFF                                       9014
045700000629      * SE 14=ON RISTAMPO ANCHE SE NON E' STATA STAMPATA
045800000629     C   13BLPFT1        IFNE      *BLANKS
045900000629     C                   SETON                                        14
046000000629     C                   ENDIF
046100991012     C* ESCLUDO SEMPRE I PREPGATI NON CONFERMATI
046200991012     C     BLPPDR        IFEQ      0
046300991012     C                   SETON                                        90
046400991012     C                   GOTO      ENDCON
046500991012     C                   END
046600000201     C* SIGLA OPERATORE
046700941228     C   03BLPSOP        IFNE      VSCSOP
046800910207     C                   SETON                                        90
046900910207     C                   GOTO      ENDCON
047000910207     C                   END
047100910207     C* LINEA ARRIVO
047200941228     C   06BLPLNA        IFNE      VSCLNA
047300910207     C                   SETON                                        90
047400910207     C                   GOTO      ENDCON
047500910207     C                   END
047600941228     C* FILIALE SEGNACOLLO
047700941228     C   17BLPFLS        IFNE      VSCFLS
047800941228     C                   SETON                                        90
047900941228     C                   GOTO      ENDCON
048000941228     C                   END
048100910903     C* SEGNACOLLI DAL  AL
048200941228     C   08
048300910903     COR 07              DO
048400910903     C                   SETON                                        90
048500941228     C     KBLP3         SETLL     FNBLT01L
048600941228     C     KBLP3         READE     FNBLT01L                               32
048700910903     C     *IN32         DOWEQ     '0'
048800910903     C* DAL
048900941228     C   07BLTNSC        IFEQ      VSCSCD
049000910903     C                   SETOFF                                       90
049100910903     C                   SETON                                        32
049200910903     C                   END
049300910903     C* DAL + AL
049400941228     C   08BLTNSC        IFGE      VSCSCD
049500941228     C     BLTNSC        ANDLE     VSCSCA
049600910903     C                   SETOFF                                       90
049700910903     C                   SETON                                        32
049800910903     C                   END
049900941228     C  N32KBLP3         READE     FNBLT01L                               32
050000910903     C                   END
050100910903     C*
050200910903     C   90              GOTO      ENDCON
050300910903     C                   END
050400910903     C*
050500910207     C* CODICE CLIENTE MITTENTE
050600910207     C   09BLPCCM        IFNE      0
050700941228     C     BLPCCM        IFNE      VSCKSC
050800910207     C                   SETON                                            90
050900910207     C                   GOTO      ENDCON
051000910207     C                   END
051100910207     C                   ELSE
051200941228     C     BLPKSC        IFNE      VSCKSC
051300910207     C                   SETON                                            90
051400910207     C                   GOTO      ENDCON
051500910207     C                   END
051600910207     C                   END
051700910207     C* CODICE PADRONCINO
051800941228     C   10BLPPDR        IFNE      VSCCPR
051900910207     C                   SETON                                            90
052000910207     C                   GOTO      ENDCON
052100910207     C                   END
052200950116     C* RITIRO MATTINA/POMERIGGIO
052300950116     C     VSCFPP        IFNE      ' '
052400950116     C     BLPFPP        ANDNE     VSCFPP
052500950116     C                   SETON                                            90
052600950116     C                   GOTO      ENDCON
052700950116     C                   END
052800950116     C* NUMERO RITIRO
052900950116     C     VSCNRT        IFNE      ' '
053000950116     C                   MOVEL     BLPNRT        W001A             1
053100950116     C     W001A         IFNE      VSCNRT
053200950116     C                   SETON                                            90
053300950116     C                   GOTO      ENDCON
053400950116     C                   END
053500950116     C                   END
053600970701     C* SI NO DDT
053700000629     C  N14VSCBAM        IFNE      ' '
053800970701     C     VSCBAM        IFEQ      'S'
053900970701     C     BLPFST        IFEQ      'N'
054000970701     C     BLPFST        OREQ      'Y'
054100970701     C     BLPFST        OREQ      'K'
054200970701     C                   SETON                                            90
054300970701     C                   GOTO      ENDCON
054400970701     C                   END
054500970701     C                   END
054600970701     C     VSCBAM        IFEQ      'N'
054700970701     C     BLPFST        IFEQ      'S'
054800970701     C     BLPFST        OREQ      'C'
054900970701     C                   SETON                                            90
055000970701     C                   GOTO      ENDCON
055100970701     C                   END
055200970701     C                   END
055300970701     C                   END
055400000201     C*
055500000201     C* ESCLUDO LE BOLLE EXPORT DPD
055600000201     C     BLPLNA        CHAIN     AZORG01L                           30
055700000201     C                   MOVEL     ORGDE3        OG143
055800020206     C     §OGNTW        IFEQ      'DPD'
055900000201     C                   SETON                                        90
056000000201     C                   GOTO      ENDCON
056100000201     C                   END
056200000201     C* ESCLUDO LE BOLLE EUROEXPRESS NO DDT
056300020206     C  N14§OGNTW        IFEQ      'EEX'
056400000201     C     BLPFST        IFEQ      'N'
056500000201     C     BLPFST        OREQ      'Y'
056600000201     C     BLPFST        OREQ      'K'
056700000201     C                   SETON                                        90
056800000201     C                   GOTO      ENDCON
056900000201     C                   END
057000000201     C                   END
057100990201     C**
057200990201     C* VEDO SE BOLLA RISTAMPABILE
057300990201    1C     BLPNRS        IFGT      0
057400990201     C* VEDO SE BOLLA FRANCO O ASSEGNATO
057500990201     C                   MOVEL     '3A'          COD
057600990201     C                   MOVEL(P)  BLPCBO        KEY
057700990201     C     KTAB          CHAIN     TABEL                              30
057800990201    2C     *IN30         IFEQ      *OFF
057900990201     C                   MOVEL     TBLUNI        DS3A
058000990201     C                   ELSE
058100990201     C                   CLEAR                   DS3A
058200990201    2C                   ENDIF
058300990201     C                   MOVEL     §3ATB1        W001A             1
058400990201     C* ASSEGNATO
058500990201    2C     W001A         IFEQ      'A'
058600990201     C                   MOVEL(P)  BLPCCM        KEY
058700990201     C                   ELSE
058800990201     C* FRANCO
058900990201     C                   MOVEL(P)  BLPKSC        KEY
059000990201    2C                   ENDIF
059100990201     C**
059200990203     C                   MOVEL     '3C'          COD
059300990201     C     KTAB          CHAIN     TABEL                              30
059400990201    2C     *IN30         IFEQ      *OFF
059500990201     C                   MOVEL     TBLUNI        DS3C
059600990201     C                   ELSE
059700990201     C                   CLEAR                   DS3C
059800990201    2C                   ENDIF
059900990201     C* NON RISTAMPABILE
060000990201    3C     §3CRSB        IFEQ      'N'
060100990201     C                   SETON                                            90
060200990201     C                   GOTO      ENDCON
060300990201     C                   ENDIF
060400990201     C                   ENDIF
060500000614      * SI/NO/SOLO POSTE
060600000614     C                   CLEAR                   OG143
060700000614     C     BLPLNP        CHAIN     AZORG                              30
060800000614     C  N30              MOVEL     ORGDE3        OG143
060900000614      *
061000000614     C     VSCSNP        IFEQ      'N'
061100020206     C     §OGNTW        ANDEQ     'PPT'
061200000614     C                   SETON                                            90
061300000614     C                   GOTO      ENDCON
061400000614     C                   ENDIF
061500000614      *
061600000614     C     VSCSNP        IFEQ      'P'
061700020206     C     §OGNTW        ANDNE     'PPT'
061800000614     C                   SETON                                            90
061900000614     C                   GOTO      ENDCON
062000000614     C                   ENDIF
062100910207     C*
062200910207     C     ENDCON        ENDSR
062300910207     C*---------------------------------------------------------------*
