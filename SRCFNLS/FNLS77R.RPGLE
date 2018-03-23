000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400941228     H* FNLS77R *----------------------------------------------------*
000500910207     H*         - RISTAMPA BOLLE                BATCH
000600000000     H*--------------------------------------------------------------*
000700010717     FFNBLP31L  IF   E           K DISK
000800020701     ffnblp76l  if   e           k disk    rename(fnblp000:fnblp076)
000900941228     FFNBLP01L  IF   E           K DISK
001000941228     F                                     RENAME(FNBLP000:FNBLP001)
001100941228     FFNBLT01L  IF   E           K DISK
001200990201     FTABEL00F  IF   E           K DISK
001300000201     FAZORG01L  IF   E           K DISK
001400020701
001500020701     d xx              s              2  0
001600030616     d wPagA4          s              5  0 inz
001700030616     d wPagA5          s              5  0 inz
001800020701     d comlnp          s                   like(v1clnp)
001900050329     d pflg            s              1    inz
002000020701
002100941228     D PARAM           DS
002200941228     D  V1CLNP                 1      3  0
002300941228     D  V1CLNA                 4      6  0
002400941228     D  DSD                    7     14  0
002500941228     D  WDSA                  15     22  0
002600941228     D*
002700941228     D  V1CLPD                23     25  0
002800941228     D  V1CSED                26     27  0
002900941228     D  V1CNSD                28     34  0
003000000629     D  SECSPE                26     34  0
003100941228     D*
003200941228     D  V1CSEA                35     36  0
003300941228     D  V1CNSA                37     43  0
003400941228     D  V1CSPE                35     43  0
003500941228     D  V1CAAS                44     47  0
003600941228     D*
003700941228     D  VSCSOP                48     48
003800941228     D  VSCLNA                49     51  0
003900941228     D  VSCFLS                52     54  0
004000941228     D  VSCSCD                55     61  0
004100941228     D  VSCSCA                62     68  0
004200941228     D  VSCKSC                69     75  0
004300950116     D*
004400941228     D  VSCCPR                76     82  0
004500950116     D  VSCFPP                83     83
004600950116     D  VSCNRT                84     84
004700970701     D  VSCBAM                85     85
004800000614     D  VSCSNP                86     86
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
006100130314     d FNLSB6ds1     e ds                  inz
006200130314     d  DB6pdf       e                     inz('F')
006300030616      *
006400910205     D KPJBA         E DS
006500990201     D DS3C          E DS
006600990201     D DS3A          E DS
006700000201     D OG143         E DS
006800020701
006900020701      *--------
007000020701      * Ds di riferimento al file esterno AzUte00F
007100020701      *--------
007200020701     d AzuteDs       e ds                  ExtName(AzUte00F)
007300020701      *--------
007400020701      * Ds per dati organigramma
007500020701      *--------
007600020701     d DDatiUte      e ds
007700020701      *--------
007800020701      * Parametri per richiamo al pgm di controllo profilo utenti
007900020701      *--------
008000020701     d Tibs34Ds      e ds                  Inz
008100020701      *--------
008200020701      * Parametri per richiamo al pgm trul06r
008300020701      *--------
008400020701     d Trul06Ds      e ds
008500020701     d  lin                    1     90  0 dim(30)
008600071029      *
008700071029      * - Parametri x ricerca/controllo tabelle
008800071029     d TIBS02ds      e ds                  inz
008900071029      * - Tab. VPO = Variabili Programmi Operativi
009000071029     d dVPO          e ds                  inz
009100071029     d  §VPOmaxLdV   e                     inz(10000)
009200020701
009300910220     C****************************************************************
009400910220     C*  RIEPILOGO INDICATORI
009500910220     C***************************************************************
009600000629     C* 01/13 - CONFRONTO DATI PASSATI
009700000629     C* 14    - NON CONTROLLO CAMPO BLPFST
009800000629     C* 30    - DI COMODO
009900910220     C* 31    - LETTURA SU FLBLP000
010000910220     C* 32    - DI COMODO
010100920421     C* 90    - ERRORE IN CONFRONTO BOLLA: DA SCARTARE
010200910220     C*****************************************************************
010300000000     C     *ENTRY        PLIST
010400000000     C                   PARM                    KPJBA
010500900511     C*---------------------------------------------------------------*
010600910207     C* ACCESSO   FLBLP                                    *
010700910207     C     KBLP          KLIST
010800941228     C                   KFLD                    V1CLNP
010900910207     C                   KFLD                    AAS
011000910207     C                   KFLD                    MGS
011100941228     C     KBLP2         KLIST
011200020701     c                   kfld                    comlnp
011300950331     C                   KFLD                    COMLNA
011400941228     C                   KFLD                    AAS
011500941228     C                   KFLD                    MGS
011600910207     C     KBLP3         KLIST
011700910207     C                   KFLD                    BLPAAS
011800910207     C                   KFLD                    BLPLNP
011900910207     C                   KFLD                    BLPNRS
012000910207     C                   KFLD                    BLPNSP
012100910903     C     KBLP4         KLIST
012200941228     C                   KFLD                    V1CAAS
012300941228     C                   KFLD                    V1CLPD
012400941228     C                   KFLD                    V1CSED
012500941228     C                   KFLD                    V1CNSD
012600910903     C     KBLP5         KLIST
012700941228     C                   KFLD                    V1CAAS
012800941228     C                   KFLD                    V1CLPD
012900020701     c     kblp6         klist
013000020701     c                   kfld                    comlnp
013100020701     c                   kfld                    comlna
013200020701
013300990201     C     KTAB          KLIST
013400990201     C                   KFLD                    CODUT             1 0
013500990201     C                   KFLD                    COD
013600990201     C                   KFLD                    KEY
013700950331     C*
013800950331     C     *LIKE         DEFINE    BLPLNA        COMLNA
013900990201     C     *LIKE         DEFINE    TBLCOD        COD
014000990201     C     *LIKE         DEFINE    TBLKEY        KEY
014100990201     C                   Z-ADD     1             CODUT
014200910205     C*---------------------------------------------------------------*
014300991116     C*
014400910205     C                   MOVEL     KPJBU         PARAM
014500020701
014600020701      * reperisco i dati utente
014700020701     c     *dtaara       define    §azute        AzuteDs
014800020701     c     *dtaara       define    §datiute      DDatiUte
014900020701
015000020701     c                   in(E)     *dtaara
015100020701     c                   IF        %Error or RSUT = *blanks
015200020701     c                   call      'TIBS34R'
015300020701     c                   parm                    Tibs34Ds
015400020701     c                   in        *dtaara
015500020701     c                   ENDIF
015600020701
015700921019     C*
015800910207     C* VEDO LE PARZIALIZZAZIONI EFFETTUATE
015900910207     C                   EXSR      PREPA
016000950331     C*
016100020701      * stampa per lna imposto la prima linea di partenza
016200020701     c                   if        *in11
016300020701     c                   z-add     1             xx
016400020701     c                   movel     lin(xx)       comlnp
016500020701     c                   endif
016600020701      * stampa per lna imposto la chiave se non richieste tutte
016700950331     C     *IN11         IFEQ      *ON
016800950331     C     V1CLNA        ANDEQ     999
016900950331     C* Scelte tutte le linee di arrivo: posizionamento sulla prima
017000020702     c     comlnp        setll     fnblp76l
017100020702     c                   read      fnblp76l                               31
017200020702     C  N31              Z-ADD     BLPLNA        COMLNA
017300950331     C                   ELSE
017400950331     C                   Z-ADD     V1CLNA        COMLNA
017500950331     C                   END
017600020701      * stampa per lnp e richiesta nelle parzializzazioni la lna imposto
017700020701      * la chiave
017800020701     c                   if        *in01 and *in06
017900020701     c                   movel     v1clnp        comlnp
018000020701     c                   movel     vsclna        comlna
018100020701     c                   endif
018200020701     C*
018300950331     C     *IN31         DOWEQ     *OFF
018400910207     C***
018500910903     C* PER NUMERO SPEDIZIONE
018600941228     C   12KBLP4         SETLL     FNBLP01L
018700020701
018800941228     C* PER LNP E  DATA   SPEDIZIONE
018900020701      * no lna nelle selezioni
019000020701     c  n06
019100020701     Can 01KBLP          SETLL     FNBLP31L
019200020701      * con lna nelle selezioni
019300020701     c   06
019400020701     Can 01kblp2         setll     fnblp76l
019500020701
019600941228     C* PER LNA E  DATA   SPEDIZIONE
019700020701     c   11kblp2         setll     fnblp76l
019800910903     C*
019900941228     C     *IN31         DOWEQ     *OFF
020000020701
020100910903     C* PER NUMERO SPEDIZIONE
020200941228     C   12
020300941228     CANN05KBLP4         READE     FNBLP01L                               31
020400941228     C   12
020500941228     CAN 05KBLP5         READE     FNBLP01L                               31
020600020701
020700941228     C* PER LNP E  DATA   SPEDIZIONE
020800020701      * no lna nelle selezioni
020900020701     c                   if        not *in06
021000010717     C   01
021100010717     CANN02KBLP          READE     FNBLP31L                               31
021200010717     C   01
021300010717     CAN 02V1CLNP        READE     FNBLP31L                               31
021400020701     c                   else
021500020701      * con lna nelle selezioni
021600020701     C   01
021700020701     CANN02kblp2         reade     fnblp76l                               31
021800020701     C   01
021900020701     CAN 02kblp6         reade     fnblp76l                               31
022000020701     c                   endif
022100020701
022200941228     C* PER LNA E  DATA   SPEDIZIONE
022300020701     c   11
022400020701     cann02kblp2         reade     fnblp76l                               31
022500020701     c   11
022600020701     can 02kblp6         reade     fnblp76l                               31
022700020701
022800910903     C*
022900910903     C* CON NUMERO AL VEDO SE COMPRESA
023000941228     C   12
023100941228     CAN 05
023200941228     CANN31              Z-ADD     BLPNSP        NUM9              9 0
023300941228     C   12
023400941228     CAN 05
023500941228     CANN31              MOVEL     BLPNRS        NUM9
023600941228     C   12
023700941228     CAN 05
023800941228     CANN31NUM9          IFGT      V1CSPE
023900910903     C                   SETON                                        31
024000910903     C                   END
024100910903     C*
024200910903     C* CON DATA   AL VEDO SE COMPRESA
024300941228     C  N12
024400941228     CAN 02
024500941228     CANN31              Z-ADD     BLPMGS        NUM8              8 0
024600941228     C  N12
024700941228     CAN 02
024800941228     CANN31              MOVEL     BLPAAS        NUM8
024900941228     C  N12
025000941228     CAN 02
025100941228     CANN31NUM8          IFGT      WDSA
025200910903     C                   SETON                                        31
025300910903     C                   END
025400941228     C*
025500910207     C* CONTROLLO LE PARZIALIZZAZIONI
025600910903     C  N31              EXSR      CONFR
025700941228     C*
025800000629     C  N31
025900000629     CANN90
026000000629     CAN 14              MOVE      '!'           BLPFST
026100050329
026200050329      * se stampa ldv x cliente forzo la stampa
026300050329     c                   If        vldvcli <> *Blanks
026400050329     c                   Eval      blpfst = '!'
026500050329     c                   EndIf
026600000629     C*
026700941228     C* S T A M P A   B O L L A
026800000629     C* SOLO SE L'HO GIA' STAMPATA O SE SCELTA SPEDIZIONE SECCA
026900000629     C* AVENDO TRASMESSO LA BOLLA IN ARRIVO
027000970227     C  N31
027100970227     CANN90BLPFST        IFNE      'N'
027200970227     C     BLPFST        ANDNE     ' '
027300030616     c                   clear                   FNLSB5DS
027400950419     C*
027500950419     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
027600950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
027700071029if  3c*** totale ?       if        wPagA4 + wPagA5 >= §VPOmaxLdV
027800071029if  3c                   if           wPagA4 >= §VPOmaxLdV
027900071029     c                             or wPagA5 >= §VPOmaxLdV
028000950419     C                   MOVEL     'E'           PARFLG
028100950419     C* DB0GES = 'R' INDICA CHE COME ULTIMA PAGINA VIENE STAMPATO UN
028200950419     C*   MESSAGGIO PER AVVISARE
028300950419     C                   MOVEL     'R'           DB0GES
028400030616e   3C                   endif
028500950419     C*
028600941228     C                   MOVEL     BLPAAS        DB0AAS
028700941228     C                   MOVEL     BLPLNP        DB0LNP
028800941228     C                   MOVEL     BLPNRS        DB0NRS
028900941228     C                   MOVEL     BLPNSP        DB0NSP
029000941228     C                   MOVEL     'P'           DB0TBO
029100941228     C                   MOVEL     'R'           DB0RIS
029200050331      * se stampa ldv x cliente forzo il flag db0ris con 'C'
029300050331      * per poi stampare la dicitura 'COPIA'
029400050331     c                   If        vldvcli <> *Blanks
029500050331     c                   Eval      db0ris = 'C'
029600050331     c                   EndIf
029700030616     c                   call      wD90psl
029800030616     C                   parm                    FNLSB5DS
029900130314     c                   parm                    FNLSB6ds1
030000030616      * Aggiorno il conteggio delle pagine stampate (x tipo)
030100030616sel 3c                   select
030200130325      * - formato A5 da FNLSB6R (stampante laser)
030300130325w   3c                   when      DB0tms = '5'  or  DB0tms = 'I'
030400130325     c*//                add       1             wPagA5
030500130325     c                   add       1             wPagA4
030600130325      * - formato A4 da FNLSB6R (stampante laser)
030700030616x   1c                   other
030800130325     c                   add       1             wPagA4
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
035900130320if  1c                   if        DB6num > *zero
036000130314     c                   clear                   FNLSB5ds
036100130314     c                   eval      DB0tla = 'L'
036200130314     c                   eval      DB0fa4 = 'I'
036300130314     c                   eval      DB6pdf = 'S'
036400130314     c                   call      wD90psl
036500130314     C                   parm                    FNLSB5ds
036600130314     c                   parm                    FNLSB6ds1
036700130314x   1c                   else
036800130314     c                   clear                   FNLSB5ds
036900130314     c                   eval      DB0tla = 'C'
037000130314     c                   call      wD90psl
037100130314     C                   parm                    FNLSB5ds
037200130314e   1c                   endif
037300130314      *
037400020701     C                   MOVEL(p)  PARAM         KPJBU
037500000000     C                   SETON                                        LR
037600910207     C*
037700910207     C*--- PREPARA/VEDO LE PARZIALIZZAZIONE FATTE -------------------*
037800910207     C     PREPA         BEGSR
037900941228     C**
038000941228     C* CONTROLLO COME DEVO LEGGERE
038100941228     C**
038200941228     C* PER LINEA DI PARTENZA
038300941228     C     V1CLNP        COMP      0                                  01
038400941228     C* PER LINEA DI ARRIVO
038500941228     C     V1CLNA        COMP      0                                  11
038600941228     C* PER NUMERO SPEDIZIONE
038700941228     C     V1CLPD        COMP      0                                  12
038800941228     C**
038900941228     C* CONTROLLO LE PARZIALIZZAZIONEI MESSE
039000941228     C**
039100941228     C* NUMERO SPEDIZIONE AL
039200941228     C     V1CNSA        COMP      0                                  05
039300910903     C* DATA AL
039400941228     C     WDSA          COMP      0                                  02
039500910207     C* SIGLA OPERATORE
039600941228     C     VSCSOP        COMP      *BLANKS                            0303
039700910207     C* LINEA DI ARRIVO
039800941228     C     VSCLNA        COMP      0                                  06
039900910207     C* SEGNACOLLO AL
040000941228     C     VSCSCA        COMP      0                                  08
040100910207     C* SEGNACOLLO DAL
040200941228     C  N08VSCSCD        COMP      0                                  07
040300941228     C* FILIALE SEGNACOLLO
040400941228     C     VSCFLS        COMP      0                                  17
040500910207     C* CODICE CLIENTE MITTENTE
040600941228     C     VSCKSC        COMP      0                                  09
040700910207     C* CODICE PADRONCINO
040800941228     C     VSCCPR        COMP      0                                  10
040900910207     C*
041000000629     C* C O N T R O L L O  SE STATA IMMESSA UNA SPEDIZIONE SECCA.
041100000629     C*
041200000629     C                   SETOFF                                       13
041300000629     C*
041400000629     C     *IN12         IFEQ      *ON
041500000629     C     V1CSPE        IFEQ      0
041600000629     C     V1CSPE        OREQ      SECSPE
041700000629     C                   SETON                                        13
041800000629     C                   ENDIF
041900000629     C                   ENDIF
042000000629     C*
042100000629     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
042200000629     C*
042300941228     C                   MOVEL     DSD           AAS               4 0
042400920320     C                   MOVE      DSD           MGS               4 0
042500910903     C                   SETOFF                                       31
042600020701      * stampa per lna
042700020701     c                   if        *in11
042800020701      * se p.o. di secondo livello imposto nella sk delle lnp se
042900020701      * stesso
043000020701     c                   clear                   lin
043100020701     c                   if        dutlpo = '2' or dutlpo = *blanks
043200020701     c                   eval      lin(1) = dutpou
043300020701      * se p.o. di primo livello carico tutti i p.o. del terminal di partenza
043400020701     c                   else
043500020701     c                   clear                   Trul06Ds
043600020701     c                   eval      D06Cod = '£1'
043700020701     c                   eval      D06Tla = 'L'
043800020701     c                   movel     Simfel        D06Key
043900020701     c                   movel(p)  Trul06Ds      Kpjbu
044000020701     c                   call      'TRUL06R'
044100020701     c                   parm                    Kpjba
044200020701     c                   movel     Kpjbu         Trul06Ds
044300020701     c                   endif
044400020701     c                   endif
044500071029      *
044600071029      * Aggancio tabella "VPO" per reperimento il n° LdV stampabili
044700071029      *   limite (per congelamento e blocco stampa)
044800071029     c                   reset                   dVPO
044900071029     c                   clear                   TIBS02ds
045000071029     c                   eval      T02tla  = 'L'
045100071029     c                   eval      T02mod  = 'C'
045200071029     c                   eval      T02sif  = KNSIF
045300071029     c                   eval      T02cod  = 'VPO'
045400071029     c                   movel(P)  'VPO'         T02ke1
045500071029     c                   call      'TIBS02R'
045600071029     c                   parm                    KPJBA
045700071029     c                   parm                    TIBS02ds
045800071029     c                   if        T02err  = *blanks
045900071029     c                   movel     T02uni        dVPO
046000071029     c                   endif
046100071029     c                   eval      wVPOmaxLdV = §VPOmaxLdV
046200950419     C*
046300030616      * Azzero i contatori delle pagine stampate
046400030616      *  (formati A4 e A5 - laser - ed altri - ad aghi)
046500030616     c                   clear                   wPagA4
046600030616     c                   clear                   wPagA5
046700910207     C                   ENDSR
046800910207     C*
046900910207     C*--- CONFRONTO PARZIALIZZAZIONI FATTE -------------------------*
047000910207     C     CONFR         BEGSR
047100000629     C                   SETOFF                                       9014
047200000629      * SE 14=ON RISTAMPO ANCHE SE NON E' STATA STAMPATA
047300000629     C   13BLPFT1        IFNE      *BLANKS
047400000629     C                   SETON                                        14
047500000629     C                   ENDIF
047600991012     C* ESCLUDO SEMPRE I PREPGATI NON CONFERMATI
047700991012     C     BLPPDR        IFEQ      0
047800991012     C                   SETON                                        90
047900991012     C                   GOTO      ENDCON
048000991012     C                   END
048100000201     C* SIGLA OPERATORE
048200941228     C   03BLPSOP        IFNE      VSCSOP
048300910207     C                   SETON                                        90
048400910207     C                   GOTO      ENDCON
048500910207     C                   END
048600910207     C* LINEA ARRIVO
048700941228     C   06BLPLNA        IFNE      VSCLNA
048800910207     C                   SETON                                        90
048900910207     C                   GOTO      ENDCON
049000910207     C                   END
049100941228     C* FILIALE SEGNACOLLO
049200941228     C   17BLPFLS        IFNE      VSCFLS
049300941228     C                   SETON                                        90
049400941228     C                   GOTO      ENDCON
049500941228     C                   END
049600910903     C* SEGNACOLLI DAL  AL
049700941228     C   08
049800910903     COR 07              DO
049900910903     C                   SETON                                        90
050000941228     C     KBLP3         SETLL     FNBLT01L
050100941228     C     KBLP3         READE     FNBLT01L                               32
050200910903     C     *IN32         DOWEQ     '0'
050300910903     C* DAL
050400941228     C   07BLTNSC        IFEQ      VSCSCD
050500910903     C                   SETOFF                                       90
050600910903     C                   SETON                                        32
050700910903     C                   END
050800910903     C* DAL + AL
050900941228     C   08BLTNSC        IFGE      VSCSCD
051000941228     C     BLTNSC        ANDLE     VSCSCA
051100910903     C                   SETOFF                                       90
051200910903     C                   SETON                                        32
051300910903     C                   END
051400941228     C  N32KBLP3         READE     FNBLT01L                               32
051500910903     C                   END
051600910903     C*
051700910903     C   90              GOTO      ENDCON
051800910903     C                   END
051900910903     C*
052000910207     C* CODICE CLIENTE MITTENTE
052100910207     C   09BLPCCM        IFNE      0
052200941228     C     BLPCCM        IFNE      VSCKSC
052300910207     C                   SETON                                            90
052400910207     C                   GOTO      ENDCON
052500910207     C                   END
052600910207     C                   ELSE
052700941228     C     BLPKSC        IFNE      VSCKSC
052800910207     C                   SETON                                            90
052900910207     C                   GOTO      ENDCON
053000910207     C                   END
053100910207     C                   END
053200910207     C* CODICE PADRONCINO
053300941228     C   10BLPPDR        IFNE      VSCCPR
053400910207     C                   SETON                                            90
053500910207     C                   GOTO      ENDCON
053600910207     C                   END
053700950116     C* RITIRO MATTINA/POMERIGGIO
053800950116     C     VSCFPP        IFNE      ' '
053900950116     C     BLPFPP        ANDNE     VSCFPP
054000950116     C                   SETON                                            90
054100950116     C                   GOTO      ENDCON
054200950116     C                   END
054300950116     C* NUMERO RITIRO
054400950116     C     VSCNRT        IFNE      ' '
054500950116     C                   MOVEL     BLPNRT        W001A             1
054600950116     C     W001A         IFNE      VSCNRT
054700950116     C                   SETON                                            90
054800950116     C                   GOTO      ENDCON
054900950116     C                   END
055000950116     C                   END
055100970701     C* SI NO DDT
055200000629     C  N14VSCBAM        IFNE      ' '
055300970701     C     VSCBAM        IFEQ      'S'
055400970701     C     BLPFST        IFEQ      'N'
055500970701     C     BLPFST        OREQ      'Y'
055600970701     C     BLPFST        OREQ      'K'
055700970701     C                   SETON                                            90
055800970701     C                   GOTO      ENDCON
055900970701     C                   END
056000970701     C                   END
056100970701     C     VSCBAM        IFEQ      'N'
056200970701     C     BLPFST        IFEQ      'S'
056300970701     C     BLPFST        OREQ      'C'
056400970701     C                   SETON                                            90
056500970701     C                   GOTO      ENDCON
056600970701     C                   END
056700970701     C                   END
056800970701     C                   END
056900000201     C*
057000000201     C* ESCLUDO LE BOLLE EXPORT DPD
057100000201     C     BLPLNA        CHAIN     AZORG01L                           30
057200000201     C                   MOVEL     ORGDE3        OG143
057300020206     C     §OGNTW        IFEQ      'DPD'
057400000201     C                   SETON                                        90
057500000201     C                   GOTO      ENDCON
057600000201     C                   END
057700000201     C* ESCLUDO LE BOLLE EUROEXPRESS NO DDT
057800020206     C  N14§OGNTW        IFEQ      'EEX'
057900000201     C     BLPFST        IFEQ      'N'
058000000201     C     BLPFST        OREQ      'Y'
058100000201     C     BLPFST        OREQ      'K'
058200000201     C                   SETON                                        90
058300000201     C                   GOTO      ENDCON
058400000201     C                   END
058500000201     C                   END
058600990201     C**
058700990201     C* VEDO SE BOLLA RISTAMPABILE
058800990201    1C     BLPNRS        IFGT      0
058900990201     C* VEDO SE BOLLA FRANCO O ASSEGNATO
059000990201     C                   MOVEL     '3A'          COD
059100990201     C                   MOVEL(P)  BLPCBO        KEY
059200990201     C     KTAB          CHAIN     TABEL                              30
059300990201    2C     *IN30         IFEQ      *OFF
059400990201     C                   MOVEL     TBLUNI        DS3A
059500990201     C                   ELSE
059600990201     C                   CLEAR                   DS3A
059700990201    2C                   ENDIF
059800990201     C                   MOVEL     §3ATB1        W001A             1
059900990201     C* ASSEGNATO
060000990201    2C     W001A         IFEQ      'A'
060100990201     C                   MOVEL(P)  BLPCCM        KEY
060200990201     C                   ELSE
060300990201     C* FRANCO
060400990201     C                   MOVEL(P)  BLPKSC        KEY
060500990201    2C                   ENDIF
060600990201     C**
060700990203     C                   MOVEL     '3C'          COD
060800990201     C     KTAB          CHAIN     TABEL                              30
060900990201    2C     *IN30         IFEQ      *OFF
061000990201     C                   MOVEL     TBLUNI        DS3C
061100990201     C                   ELSE
061200990201     C                   CLEAR                   DS3C
061300990201    2C                   ENDIF
061400990201     C* NON RISTAMPABILE
061500990201    3C     §3CRSB        IFEQ      'N'
061600990201     C                   SETON                                            90
061700990201     C                   GOTO      ENDCON
061800990201     C                   ENDIF
061900990201     C                   ENDIF
062000000614      * SI/NO/SOLO POSTE
062100000614     C                   CLEAR                   OG143
062200000614     C     BLPLNP        CHAIN     AZORG                              30
062300000614     C  N30              MOVEL     ORGDE3        OG143
062400000614      *
062500000614     C     VSCSNP        IFEQ      'N'
062600020206     C     §OGNTW        ANDEQ     'PPT'
062700000614     C                   SETON                                            90
062800000614     C                   GOTO      ENDCON
062900000614     C                   ENDIF
063000000614      *
063100000614     C     VSCSNP        IFEQ      'P'
063200020206     C     §OGNTW        ANDNE     'PPT'
063300000614     C                   SETON                                            90
063400000614     C                   GOTO      ENDCON
063500000614     C                   ENDIF
063600910207     C*
063700910207     C     ENDCON        ENDSR
063800910207     C*---------------------------------------------------------------*
