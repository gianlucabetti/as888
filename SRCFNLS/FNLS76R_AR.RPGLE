000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS76R *----------------------------------------------------*
000300910207     H*         - RISTAMPA SEGNACOLLI           BATCH
000400000000     H*--------------------------------------------------------------*
000500060220     FFiar401L  IF   E           K DISK
000600910207     FTABEL00F  IF   E           K DISK
000700910207     FAZORG01L  IF   E           K DISK
000800010717     F*
000900010717     FFNBLP31L  IF   E           K DISK
001000020702     ffnblp76l  if   e           k disk    rename(fnblp000:fnblp076)
001100941228     FFNBLP01L  IF   E           K DISK
001200941228     F                                     RENAME(FNBLP000:FNBLP001)
001300090304     FFIART01L  IF   E           K DISK
001400051012     FFIARS01L  IF   E           K DISK
001500941228     FFNBLD01L  IF   E           K DISK
001600050418     fFIAR501L  if   e           k disk
001700941228     FFNLV22P   O    F  132        PRINTER
001800020702
001900020702     d xx              s              2  0
002000081007     d w005a           s              5
002100020702     d comlnp          s                   like(v1clnp)
002200020702
002300980402     D W35             S              1    DIM(35)
002400991201     D W15             S              1    DIM(15)
002500980402     D*
002600900523     D PARAM           DS
002700941228     D  V1CLNP                 1      3  0
002800941228     D  V1CLNA                 4      6  0
002900941228     D  DSD                    7     14  0
003000941228     D  WDSA                  15     22  0
003100941228     D*
003200941228     D  V1CLPD                23     25  0
003300941228     D  V1CSED                26     27  0
003400941228     D  V1CNSD                28     34  0
003500941228     D*
003600941228     D  V1CSEA                35     36  0
003700941228     D  V1CNSA                37     43  0
003800941228     D  V1CSPE                35     43  0
003900941228     D  V1CAAS                44     47  0
004000941228     D*
004100941228     D  VSCSOP                48     48
004200941228     D  VSCLNA                49     51  0
004300941228     D  VSCFLS                52     54  0
004400941228     D  VSCSCD                55     61  0
004500941228     D  VSCSCA                62     68  0
004600941228     D  VSCKSC                69     75  0
004700950116     D*
004800941228     D  VSCCPR                76     82  0
004900950116     D  VSCFPP                83     83
005000950116     D  VSCNRT                84     84
005100970701     D  VSCBAM                85     85
005200000614     D  VSCSNP                86     86
005300080220     d* · num. limite di LdV stampabili (prima FISSO 6000)
005400080220     d  wVPOmaxLdV           151    155s 0 inz
005500950420     D* PARFLG = 'E' --> STAMPATE PIU' DI 6000 PAGINE
005600030613     D  PARFLG               156    156
005700941228     D* PGM DI STAMPA ETICHETTE
005800941228     D  PSS                  207    216
005900910207     D                 DS
006000941228     D  BLPAAS                 1      4  0
006100941228     D  BLPMGS                 5      8  0
006200941228     D  BLPDSP                 1      8  0
006300910903     D                 DS
006400910903     D  BLPNRS                 1      2  0
006500910903     D  BLPNSP                 3      9  0
006600910903     D  BLPSPE                 1      9  0
006700991201     D                 DS
006800991201     D  DSRMN                  1     15
006900991201     D  DSRMN1                 4      6
007000991202     D  DSRMN2                 7     14
007100991202     D  DSRMN3                15     15
007200991214     D                 DS
007300991214     D  DPDSTP                 1     14
007400991214     D  DPDS1                  1      3
007500991214     D  DPDV1                  4      4    INZ
007600991214     D  DPDS2                  5     12
007700991214     D  DPDV2                 13     13    INZ
007800991214     D  DPDS3                 14     14
007900991214     D                 DS
008000991214     D  DPDPAR                 1     12
008100991214     D  DPD1                   1      3
008200991214     D  DPD2                   4     11
008300991214     D  DPD3                  12     12
008400910207     D DS1B          E DS
008500971219     D DS15          E DS
008600971219     D DS3A          E DS
008700910207     D DSBLD         E DS
008800971219     D DSBLDE        E DS
008900060523     D*DSBL4F        E DS
009000060523     D DSBL4I        E DS
009100060523     D DSBL4K        E DS
009200050418     d dAR5gen       e ds                  inz
009300910205     D KPJBA         E DS
009400991201     D OG143         E DS
009500040525     D OG150         E DS
009600020214     d a_og143       e ds                  extname(og143)
009700020214     d                                     prefix(a_)
009800020702
009900020702      *--------
010000020702      * Ds di riferimento al file esterno AzUte00F
010100020702      *--------
010200020702     d AzuteDs       e ds                  ExtName(AzUte00F)
010300020702      *--------
010400020702      * Ds per dati organigramma
010500020702      *--------
010600020702     d DDatiUte      e ds
010700020702      *--------
010800020702      * Parametri per richiamo al pgm di controllo profilo utenti
010900020702      *--------
011000020702     d Tibs34Ds      e ds                  Inz
011100020702      *--------
011200020702      * Parametri per richiamo al pgm trul06r
011300020702      *--------
011400020702     d Trul06Ds      e ds
011500020702     d  lin                    1     90  0 dim(30)
011600080220      *
011700080220      * - Parametri x ricerca/controllo tabelle
011800080220     d TIBS02ds      e ds                  inz
011900080220      * - Tab. VPO = Variabili Programmi Operativi
012000080220     d dVPO          e ds                  inz
012100080220     d  §VPOmaxLdV   e                     inz(10000)
012200020214
012300020214     d lnpntw          s                   like(§ogntw)
012400020214     d lnantw          s                   like(§ogntw)
012500020215
012600000000     C*---------------------------------------------------------------*
012700000000     C     *ENTRY        PLIST
012800000000     C                   PARM                    KPJBA
012900921209     C                   Z-ADD     1             CODUT             1 0
013000900511     C*---------------------------------------------------------------*
013100910207     C* ACCESSO   FLBLP                                    *
013200910207     C     KBLP          KLIST
013300941228     C                   KFLD                    V1CLNP
013400910207     C                   KFLD                    AAS
013500910207     C                   KFLD                    MGS
013600941228     C     KBLP2         KLIST
013700020702     c                   kfld                    comlnp
013800950331     C                   KFLD                    COMLNA
013900941228     C                   KFLD                    AAS
014000941228     C                   KFLD                    MGS
014100910903     C     KBLP4         KLIST
014200941228     C                   KFLD                    V1CAAS
014300941228     C                   KFLD                    V1CLPD
014400941228     C                   KFLD                    V1CSED
014500941228     C                   KFLD                    V1CNSD
014600910903     C     KBLP5         KLIST
014700941228     C                   KFLD                    V1CAAS
014800941228     C                   KFLD                    V1CLPD
014900020702     c     kblp6         klist
015000020702     c                   kfld                    comlnp
015100020702     c                   kfld                    comlna
015200020702
015300910207     C     KBLD          KLIST
015400910207     C                   KFLD                    BLPAAS
015500910207     C                   KFLD                    BLPLNP
015600910207     C                   KFLD                    BLPNRS
015700910207     C                   KFLD                    BLPNSP
015800060220     C     Kar4          KLIST
015900971217     C                   KFLD                    BLPAAS
016000971217     C                   KFLD                    BLPLNP
016100971217     C                   KFLD                    BLPNRS
016200971217     C                   KFLD                    BLPNSP
016300971217     C                   KFLD                    WTRC
016400910207     C* ACCESSO   TABEL                                    *
016500910207     C     KTAB          KLIST
016600910207     C                   KFLD                    CODUT
016700910207     C                   KFLD                    COD               2
016800910207     C                   KFLD                    KEY               8
016900980402     C*
017000051012     C     KarS01        KLIST
017100980402     C                   KFLD                    BLPFLS
017200980402     C                   KFLD                    BLPLNA
017300980402     C                   KFLD                    BLPNRS
017400980402     C                   KFLD                    §LDNCD
017500980402     C                   KFLD                    WTRC
017600050418      * FIAR501L
017700050418     c     k05AR501      klist
017800050418     c                   kfld                    BLPaas
017900050418     c                   kfld                    BLPlnp
018000050418     c                   kfld                    BLPnrs
018100050418     c                   kfld                    BLPnsp
018200050418     c                   kfld                    AR5trd
018300050418     c                   eval      AR5trd = 'GEN'
018400950331     C*
018500950331     C     *LIKE         DEFINE    BLPLNA        COMLNA
018600060220     C     *LIKE         DEFINE    ar4TRC        WTRC
018700910205     C*---------------------------------------------------------------*
018800910205     C                   MOVEL     KPJBU         PARAM
018900020702
019000020702      * reperisco i dati utente
019100020702     c     *dtaara       define    §azute        AzuteDs
019200020702     c     *dtaara       define    §datiute      DDatiUte
019300020702
019400020702     c                   in(E)     *dtaara
019500020702     c                   IF        %Error or RSUT = *blanks
019600020702     c                   call      'TIBS34R'
019700020702     c                   parm                    Tibs34Ds
019800020702     c                   in        *dtaara
019900020702     c                   ENDIF
020000020702
020100971217     C*
020200971217     C                   CLEAR                   DSBLD
020300971217     C**
020400910207     C* VEDO LE PARZIALIZZAZIONI EFFETTUATE
020500910207     C                   EXSR      PREPA
020600950331     C*
020700020702      * stampa per lna imposto la prima linea di partenza
020800020702     c                   if        *in11
020900020702     c                   z-add     1             xx
021000020702     c                   movel     lin(xx)       comlnp
021100020702     c                   endif
021200020702      * stampa per lna imposto la chiave se non richieste tutte
021300950331     C     *IN11         IFEQ      *ON
021400950331     C     V1CLNA        ANDEQ     999
021500950331     C* Scelte tutte le linee di arrivo: posizionamento sulla prima
021600020702     c     comlnp        setll     fnblp76l
021700020702     c                   read      fnblp76l                               31
021800950331     C  N31              Z-ADD     BLPLNA        COMLNA
021900950331     C                   ELSE
022000950331     C                   Z-ADD     V1CLNA        COMLNA
022100950331     C                   END
022200020702      * stampa per lnp e richiesta nelle parzializzazioni la lna imposto
022300020702      * la chiave
022400020702     c                   if        *in01 and *in06
022500020702     c                   movel     v1clnp        comlnp
022600020702     c                   movel     vsclna        comlna
022700020702     c                   endif
022800950331     C*
022900950331     C     *IN31         DOWEQ     *OFF
023000941228     C***
023100941228     C* PER NUMERO SPEDIZIONE
023200941228     C   12KBLP4         SETLL     FNBLP01L
023300020702
023400941228     C* PER LNP E  DATA   SPEDIZIONE
023500020702      * no lna nelle selezioni
023600020702     c  n06
023700020702     Can 01KBLP          SETLL     FNBLP31L
023800020702      * con lna nelle selezioni
023900020702     c   06
024000020702     can 01kblp2         setll     fnblp76l
024100020702
024200941228     C* PER LNA E  DATA   SPEDIZIONE
024300020702     c   11kblp2         setll     fnblp76l
024400941228     C*
024500941228     C     *IN31         DOWEQ     *OFF
024600020702
024700941228     C* PER NUMERO SPEDIZIONE
024800941228     C   12
024900941228     CANN05KBLP4         READE     FNBLP01L                               31
025000941228     C   12
025100941228     CAN 05KBLP5         READE     FNBLP01L                               31
025200020702
025300941228     C* PER LNP E  DATA   SPEDIZIONE
025400020702      * no lna nelle selezioni
025500020702     c                   if        not *in06
025600010717     C   01
025700010717     CANN02KBLP          READE     FNBLP31L                               31
025800010717     C   01
025900010717     CAN 02V1CLNP        READE     FNBLP31L                               31
026000020702     c                   else
026100020702      * con lna nelle selezioni
026200020702     c   01
026300020702     cann02kblp2         reade     fnblp76l                               31
026400020702     c   01
026500020702     can 02kblp6         reade     fnblp76l                               31
026600020702     c                   endif
026700020702
026800941228     C* PER LNA E  DATA   SPEDIZIONE
026900020702     c   11
027000020702     cann02kblp2         reade     fnblp76l                               31
027100020702     c   11
027200020702     can 02kblp6         reade     fnblp76l                               31
027300020702
027400910207     C***
027500910903     C*
027600910903     C* CON NUMERO AL VEDO SE COMPRESA
027700941228     C   12
027800941228     CAN 05
027900941228     CANN31BLPSPE        IFGT      V1CSPE
028000910903     C                   SETON                                        31
028100910903     C                   END
028200910903     C*
028300910903     C* CON DATA   AL VEDO SE COMPRESA
028400941228     C  N12
028500941228     CAN 02
028600941228     CANN31BLPDSP        IFGT      WDSA
028700910903     C                   SETON                                        31
028800910903     C                   END
028900910903     C  N31              EXSR      CONFR
029000910207     C*
029100910903     C  N31
029200910903     CANN90              EXSR      CONTR
029300950419     C*
029400950420     C* SE SUPERATE LE 6000 PAGINE (PARFLG='E'), VADO A FINE PGM
029500950419     C     PARFLG        IFEQ      'E'
029600950419     C                   GOTO      FINE
029700950419     C                   ENDIF
029800910207     C*
029900910205     C                   END
030000950331     C*
030100020702      * Richiesta di tutte le linee di arrivo
030200950331     C     *IN11         IFEQ      *ON
030300950331     C     V1CLNA        ANDEQ     999
030400020702      * cerco linea di arrivo sucessiva
030500020702     c     kblp6         setgt     fnblp76l
030600020702     c                   read      fnblp76l                               31
030700020702     c                   if        blplnp = comlnp
030800020702     c                   z-add     blplna        comlna
030900020702      * cerco la linea di partenza sucessiva
031000020702     c                   else
031100020702     c                   add       1             xx
031200020702      * non ci sono più linee di partenza vado a fine
031300020702     c                   if        lin(xx) = *zeros
031400020702     c                   goto      fine
031500020702     c                   endif
031600020702      * cerco la prima linea di arrivo della linea di partenza che
031700020702      * devo leggere
031800020702     c                   eval      comlnp = lin(xx)
031900020702     c     comlnp        setll     fnblp76l
032000020702     c                   read      fnblp76l                               31
032100020702     c  n31              z-add     blplna        comlna
032200020702     c                   endif
032300950331     C                   END
032400020702
032500020702      * richiesta solo una linea di arrivo
032600020702     c                   if        *in11 and v1clna <> 999
032700020702     c                   eval      *in31 = *off
032800020702      * cerco la linea di partenza sucessiva
032900020702     c                   add       1             xx
033000020702     c                   if        lin(xx) = *zeros
033100020702      * non ci sono più linee di partenza vado a fine
033200020702     c                   goto      fine
033300020702     c                   endif
033400020702     c                   eval      comlnp = lin(xx)
033500020702     c                   endif
033600950331     C*
033700950331     C                   ENDDO
033800921209     C*
033900950419     C     FINE          TAG
034000020702     C                   MOVEL(p)  PARAM         KPJBU
034100000000     C                   SETON                                        LR
034200910207     C*
034300910219     C*--- PREPARO VEDO LE PARZIALIZZAZIONE FATTE -------------------*
034400910207     C     PREPA         BEGSR
034500941228     C**
034600941228     C* CONTROLLO COME DEVO LEGGERE
034700941228     C**
034800941228     C* PER LINEA DI PARTENZA
034900941228     C     V1CLNP        COMP      0                                  01
035000941228     C* PER LINEA DI ARRIVO
035100941228     C     V1CLNA        COMP      0                                  11
035200941228     C* PER NUMERO SPEDIZIONE
035300941228     C     V1CLPD        COMP      0                                  12
035400941228     C**
035500950331     C* CONTROLLO LE PARZIALIZZAZIONI MESSE
035600941228     C**
035700941228     C* NUMERO SPEDIZIONE AL
035800941228     C     V1CNSA        COMP      0                                  05
035900941228     C* DATA AL
036000941228     C     WDSA          COMP      0                                  02
036100941228     C* SIGLA OPERATORE
036200941228     C     VSCSOP        COMP      *BLANKS                            0303
036300941228     C* LINEA DI ARRIVO
036400941228     C     VSCLNA        COMP      0                                  06
036500941228     C* SEGNACOLLO AL
036600941228     C     VSCSCA        COMP      0                                  08
036700941228     C* SEGNACOLLO DAL
036800941228     C  N08VSCSCD        COMP      0                                  07
036900941228     C* FILIALE SEGNACOLLO
037000941228     C     VSCFLS        COMP      0                                  17
037100941228     C* CODICE CLIENTE MITTENTE
037200941228     C     VSCKSC        COMP      0                                  09
037300941228     C* CODICE PADRONCINO
037400941228     C     VSCCPR        COMP      0                                  10
037500910207     C*
037600941228     C                   MOVEL     DSD           AAS               4 0
037700910207     C                   MOVE      DSD           MGS               4 0
037800910903     C                   SETOFF                                       31
037900950418     C*
038000950418     C* AZZERO IL NUMERATORE PAGINE
038100950418     C                   Z-ADD     *ZERO         WPAGIN            5 0
038200020702      * stampa per lna
038300020702     c                   if        *in11
038400020702      * se p.o. di secondo livello imposto nella sk delle lnp se
038500020702      * stesso
038600020702     c                   clear                   lin
038700020702     c                   if        dutlpo = '2' or dutlpo = *blanks
038800020702     c                   eval      lin(1) = dutpou
038900020702      * se p.o. di primo livello carico tutti i p.o. del terminal di partenza
039000020702     c                   else
039100020702     c                   clear                   Trul06Ds
039200020702     c                   eval      D06Cod = '£1'
039300020702     c                   eval      D06Tla = 'L'
039400020702     c                   movel     Simfel        D06Key
039500020702     c                   movel(p)  Trul06Ds      Kpjbu
039600020702     c                   call      'TRUL06R'
039700020702     c                   parm                    Kpjba
039800020702     c                   movel     Kpjbu         Trul06Ds
039900020702     c                   endif
040000020702     c                   endif
040100020702
040200080220      * Aggancio tabella "VPO" per reperimento il n° LdV stampabili
040300080220      *   limite (per congelamento e blocco stampa)
040400080220     c                   reset                   dVPO
040500080220     c                   clear                   TIBS02ds
040600080220     c                   eval      T02tla  = 'L'
040700080220     c                   eval      T02mod  = 'C'
040800080220     c                   eval      T02sif  = KNSIF
040900080220     c                   eval      T02cod  = 'VPO'
041000080220     c                   movel(P)  'VPO'         T02ke1
041100080220     c                   call      'TIBS02R'
041200080220     c                   parm                    KPJBA
041300080220     c                   parm                    TIBS02ds
041400080220     c                   if        T02err  = *blanks
041500080220     c                   movel     T02uni        dVPO
041600080220     c                   endif
041700080220     c                   eval      wVPOmaxLdV = §VPOmaxLdV
041800080220      *
041900910207     C                   ENDSR
042000910207     C*
042100910207     C*--- CONTROLLO SE POSSO STAMPARE SEGNACOLLO -------------------*
042200910207     C     CONTR         BEGSR
042300000201     C                   CLEAR                   DSBLDE
042400010302     C                   CLEAR                   WPRMA
042500910207     C* PRENDO CODICE TRATTAMENTO MERCE
042600971219     C                   MOVEL(P)  BLPCTM        KEY
042700910207     C                   MOVEL     '1B'          COD
042800910207     C     KTAB          CHAIN     TABEL                              30
042900910207     C  N30              MOVEL     TBLUNI        DS1B
043000971219     C   30              CLEAR                   DS1B
043100971219     C**
043200971219     C* CHAIN CON LINEA DI PARTENZA
043300971219     C     BLPLNP        CHAIN     AZORG                              30
043400991201     C                   MOVEL     ORGDE3        OG143
043500020214     c                   eval      lnpntw = §ogntw
043600971219     C**
043700971219     C* IMPOSTO LA LINEA DI ARRIVO
043800971219     C                   MOVEL     BLPLNA        §LDLNA
043900971219     C     BLPLNA        CHAIN     AZORG                              30
044000971219     C  N30ORGDE5        IFNE      *BLANKS
044100971219     C                   MOVEL     ORGDE5        §LDDLA
044200971219     C                   ELSE
044300971219     C                   MOVEL     ORGDES        §LDDLA
044400971219     C                   END
044500971219     C   30              MOVEL     *BLANKS       §LDDLA
044600020214     c                   eval      a_og143 = orgde3
044700020214     c                   eval      lnantw = a_§ogntw
044800040525     c                   movel     orgtel        dpdtel           15
044900040525     c                   movel     orgfax        dpdfax           15
045000040525     c                   movel     orgdf0        og150
045100991206      *
045200991206      * RIFERIMENTO MITTENTE ALFABETICO, SE LNP DPD SCRIVO NUMERICO
045300020215     c                   if        lnpntw <> 'DPD'
045400971219     C                   MOVEL     BLPRMA        §LDCDP
045500060523     C***                ELSE
045600060523     C***                MOVEL     BLPRMN        DSRMN
045700060523     C***                CLEAR                   W15
045800060523     C***                MOVEA     DSRMN1        W15(1)
045900060523     C***                MOVEA     DSRMN2        W15(5)
046000060523     C***                MOVEA     DSRMN3        W15(14)
046100060523     C***                MOVEA     W15           §LDCDP
046200991201    1C                   ENDIF
046300991206      **
046400060220      **  CHAIN Fiar4 PER EXPORT DPD
046500060523     c                   if        lnantw = 'DPD' or lnpntw='DPD'
046600060523     C                   MOVEL     'I'           WTRC
046700060220     C     Kar4          CHAIN     Fiar401L                           39
046800060523     C  N39              MOVEL     ar4NOT        DSBL4I
046900060523     C   39              CLEAR                   DSBL4I
047000060523    2C     §B4ipn        IFNE      *BLANKS
047100060523     C                   MOVEL     §B4ipn        DPDPAR
047200991214     C                   MOVEL     DPD1          DPDS1
047300991214     C                   MOVEL     DPD2          DPDS2
047400060523     C                   MOVEL     §b4icd        DPDS3
047500991214     C                   MOVEL(P)  DPDSTP        §LDCDP
047600060523     c                   endif
047700060523     c                   endif
047800060523     c*
047900060523     c                   if        lnantw = 'DPD' and §b4ipn <> *blanks
048000060523     C* SE RMA DIVERSO DA BLANK MEMORIZZO CHE DEVO STAMPARLO PRIMA
048100060523     C* DELLA RAGIONE SOCIALE MITENTE
048200060523     C     BLPRMA        IFNE      *BLANKS
048300060523     C                   MOVE      '1'           WPRMA             1
048400060523     C                   ENDIF
048500000201     C     BLPPKF        IFLE      3
048600000201     C                   MOVEL     'X'           §DESSE
048700000201     C                   ENDIF
048800060523     c*
048900060523     C                   MOVEL     'K'           WTRC
049000060523     C     Kar4          CHAIN     Fiar401L                           39
049100060523     C  N39              MOVEL     ar4NOT        DSBL4K
049200060523     C   39              CLEAR                   DSBL4K
049300000201     C*
049400060523     C                   MOVEL     blprsd        §DESIA
049500060523     C                   MOVEL     §B4krt        §DEAWB
049600060523     C                   MOVEL     §B4ipn        §DEEUR
049700060523     c                   eval      %subst(§deeur:12:1) = §b4icd
049800000201     C                   MOVEL     DPDTEL        §DEXX2
049900000201     C                   MOVE      DPDFAX        §DEXX2
050000060523     c                   eval      w005a = %subst(§b4ida:2:3) + §b4iza
050100000201     C                   MOVE      W005A         §DEXX1
050200991206    2C                   ENDIF
050300991206      **
050400000201     C** IMPOSTO CAMPI FISSI SERVONO SOLO PER EXPORT
050500020214
050600000207     C** IMPOSTO EUR IN TS PER EUROEXPRESS
050700020214
050800020214      * Se arrivo EuroExpress
050900040507      * imposta EEX
051000020214     c                   if        lnantw = 'EEX'
051100040507     C                   MOVEL     'EEX'         §DETSE
051200000207     C                   ENDIF
051300921209     C*
051400910207     C* FLAG6 DI STAMPA BOLLA
051500950419    1C     §1BFG6        IFNE      'D'
051600971219     C     §1BFG9        ANDNE     'S'
051700971219     C**
051800971219    2C     BLPNCD        IFGT      0
051900910207     C                   EXSR      STASCI
052000921209     C*
052100971219   X2C                   ELSE
052200921209     C*
052300921209     C* STAMPO ANCHE SE NON SONO SEQUENZIALI
052400971219    3C     BLPFNS        IFEQ      'S'
052500921209     C                   EXSR      STASCN
052600971219    3C                   ENDIF
052700971219    2C                   ENDIF
052800921209     C*
052900950419   X1C                   ELSE
053000910207     C*
053100941228     C     KBLD          SETLL     FNBLD000
053200921209     C                   SETOFF                                       32
053300921209     C*
053400971219    2C     *IN32         DOWEQ     *OFF
053500941228     C     KBLD          READE     FNBLD000                               32
053600971219    3C  N32BLDFST        IFEQ      'S'
053700971219    3C     BLDFST        OREQ      'Z'
053800910207     C                   EXSR      STASCD
053900950420     C* SE SUPERATE LE 6000 PAGINE (PARFLG='E'), INTERROMPO IL CICLO
054000971219    3C     PARFLG        IFEQ      'E'
054100950419     C                   SETON                                        32
054200971219    3C                   ENDIF
054300971219    3C                   ENDIF
054400971219    2C                   ENDDO
054500910207     C*
054600971219    1C                   ENDIF
054700910207     C*
054800910207     C                   ENDSR
054900910207     C*
055000921209     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI SEQUENZIALI ---------*
055100910207     C     STASCI        BEGSR
055200910207     C                   Z-ADD     BLPNCD        COMSCD            7 0
055300910207     C     BLPNCA        IFEQ      0
055400910207     C                   Z-ADD     BLPNCD        BLPNCA
055500910207     C                   END
055600921209     C* IMPOSTO CAMPI DI STAMPA
055700921209     C                   EXSR      IMPO
055800910208     C*
055900910208     C     COMSCD        DOWLE     BLPNCA
056000990318     C     COMSCD        ANDGT     0
056100910208     C                   ADD       1             §LDNC1
056200910208     C                   SETOFF                                       21
056300910208     C* SE SELEZIONE SEGNACOLLI CONTROLLO CHE SIA NEL RANGE
056400910403     C   07
056500941228     COR 08COMSCD        IFLT      VSCSCD
056600910208     C                   SETON                                        21
056700910208     C                   END
056800910403     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      DAL / AL
056900941228     C   08
057000941228     CANN21COMSCD        IFGT      VSCSCA
057100910208     C                   Z-ADD     BLPNCA        COMSCD
057200910208     C                   SETON                                        21
057300910208     C                   END
057400910403     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      SOLO DAL
057500941228     C   07
057600941228     CANN21COMSCD        IFGT      VSCSCD
057700910403     C                   Z-ADD     BLPNCA        COMSCD
057800910403     C                   SETON                                        21
057900910403     C                   END
058000910208     C* S T A M P A
058100910208     C  N21              MOVEL     COMSCD        §LDNCD
058200020215
058300020215     c                   if        not *in21
058400020215     c                   select
058500020215      * per segnacolli FedEx imposta §LDCDP
058600020215     c                   when      lnantw = 'FED'
058700020215     c                   exsr      sr_fedex
058800020215      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
058900040507     c                   when      lnpntw = 'EEX'
059000020215     C                   EXSR      EUREXP
059100020215     c                   endsl
059200020215     c                   endif
059300020215
059400910208     C*
059500941228     C  N21              CALL      PSS
059600910207     C                   PARM                    DSBLD
059700971219     C                   PARM                    DSBLDE
059800910207     C*
059900910207     C                   ADD       1             COMSCD
060000950419     C*
060100950419     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
060200950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
060300041012     C  n21              ADD       1             WPAGIN
060400080220     c                   IF        NOT  *in21
060500080220     c                             and  wPagin > §VPOmaxLdV
060600950419     C                   MOVEL     'E'           PARFLG
060700950419     C* IMPOSTO 9999999 IN COMSCD PER INTERROMPERE IL CICLO
060800950419     C                   Z-ADD     *ALL'9'       COMSCD
060900950419     C                   ENDIF
061000950419     C*
061100910207     C                   END
061200910208     C*
061300910207     C                   ENDSR
061400921209     C*
061500921209     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI NON SEQUENZIALI -----*
061600921209     C     STASCN        BEGSR
061700921209     C*
061800921209     C                   EXSR      IMPO
061900921209     C*
062000090304     C     KBLD          SETLL     FIART01L
062100090304     C     KBLD          READE     FIART01L                               33
062200921209     C*
062300941228     C     *IN33         DOWEQ     *OFF
062400921209     C*
062500090304     C                   Z-ADD     ARTNSC        COMSCD
062600921209     C                   ADD       1             §LDNC1
062700921209     C                   SETOFF                                       21
062800921209     C* SE SELEZIONE SEGNACOLLI CONTROLLO CHE SIA NEL RANGE
062900921209     C   07
063000941228     COR 08COMSCD        IFLT      VSCSCD
063100921209     C                   SETON                                        21
063200921209     C                   END
063300921209     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      DAL / AL
063400941228     C   08
063500941228     CANN21COMSCD        IFGT      VSCSCA
063600921209     C                   SETON                                        2133
063700921209     C                   END
063800921209     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      SOLO DAL
063900941228     C   07
064000941228     CANN21COMSCD        IFGT      VSCSCD
064100921209     C                   SETON                                        2133
064200921209     C                   END
064300921209     C* S T A M P A
064400921209     C  N21              MOVEL     COMSCD        §LDNCD
064500020215
064600020215     c                   if        not *in21
064700020215     c                   select
064800020215      * per segnacolli FedEx imposta §LDCDP
064900020215     c                   when      lnantw = 'FED'
065000020215     c                   exsr      sr_fedex
065100020215      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
065200040507     c                   when      lnpntw = 'EEX'
065300020215     C                   EXSR      EUREXP
065400020215     c                   endsl
065500020215     c                   endif
065600020215
065700921209     C*
065800941228     C  N21              CALL      PSS
065900921209     C                   PARM                    DSBLD
066000971219     C                   PARM                    DSBLDE
066100950418     C*
066200950418     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
066300950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
066400041012     C  n21              ADD       1             WPAGIN
066500080220     c                   IF        NOT  *in21
066600080220     c                             and  wPagin > §VPOmaxLdV
066700950419     C                   MOVEL     'E'           PARFLG
066800950419     C* ACCENDO *IN33  PER INTERROMPERE IL CICLO
066900950419     C                   SETON                                        33
067000950419     C                   ENDIF
067100921209     C*
067200090304     C  N33KBLD          READE     FIART01L                               33
067300921209     C                   END
067400921209     C*
067500921209     C                   ENDSR
067600921209     C*
067700921209     C* IMPOSTO CAMPI PER STAMPA IN IMMEDIATO ------------------------*
067800921209     C     IMPO          BEGSR
067900050418      *
068000050418      * Verifica se spedizione "di valore"
068100050418     c                   exsr      CtrSpeVal
068200921209     C*
068300921209     C                   Z-ADD     0             §LDNC1
068400921209     C                   MOVEL     BLPFLS        §LDLNP
068500921209     C     BLPFLS        CHAIN     AZORG                              30
068600921209     C  N30              MOVEL     ORGDES        §LDDLP
068700921209     C   30              MOVEL     *BLANKS       §LDDLP
068800921209     C                   MOVEL     BLPNRS        §LDNRS
068900921209     C                   MOVEL     BLPTFA        §LDTNA
069000921209     C                   MOVEL     BLPTSP        §LDTSP
069100921209     C                   MOVEL     BLPZNC        §LDZSC
069200921209     C                   MOVE      BLPNCL        §LDNC2
069300010302     C* SE DEVO STAMPARE ETICHETTA EXPORT DPD PRIMA DELLA RAGIONE SOC.
069400010302     C* MITTENTE DEVO STAMPARE IL RMA SENZA I BLANK NON SIGNIFICATIVI
069500010302     C     WPRMA         IFEQ      '1'
069600010302     C                   CLEAR                   W35
069700010302     C                   MOVEA     BLPRMA        W35
069800010302     C     ' '           CHECKR    BLPRMA        §Z                2 0
069900010302     C                   ADD       2             §Z
070000010302     C                   MOVEA     BLPRSM        W35(§Z)
070100010302     C                   MOVEA     W35           §LDRSM
070200010302     C                   ELSE
070300921209     C                   MOVEL     BLPRSM        §LDRSM
070400010302     C                   ENDIF
070500921209     C                   MOVEL     BLPPRM        §LDPRM
070600921209     C                   MOVEL     BLPRSD        §LDRSD
070700921209     C                   MOVEL     BLPCAD        §LDCAD
070800921209     C                   MOVEL     BLPLOD        §LDLOD
070900921209     C                   MOVEL     BLPIND        §LDIND
071000921209     C                   MOVEL     BLPPRD        §LDPRD
071100941228     C                   MOVE      BLPPDR        §LDCPD
071200921209     C                   MOVEL     *ZEROS        §LDNCP
071300921209     C                   MOVEL     *ZEROS        §LDNCU
071400050418if  1c                   if        §AR5bva = 'V'
071500050418     c                   movel     'X'           §LDrst
071600050418x   1c                   else
071700921209     C                   MOVEL     'R'           §LDRST
071800050418e   1c                   endif
071900941228     C                   Z-ADD     BLPPKF        §LDPKG
072000941228     C                   Z-ADD     BLPVLF        §LDVOL
072100941228     C                   MOVEL     BLPFVF        §LDFVR
072200950116     C                   MOVEL     BLPAAS        §LDAAS
072300950116     C                   MOVEL     BLPMGS        §LDMGS
072400971219     C*
072500971219     C                   EXSR      ETIEST
072600980402     C*
072700921209     C                   ENDSR
072800921209     C*
072900910207     C*--- STAMPA BOLLA IN IMMEDIATO  COME RISTAMPA DEL "D" ----------*
073000910207     C     STASCD        BEGSR
073100910207     C* RICHIAMO DIRETTAMENTE IL PROGRAMMA DI STAMPA SEGNACOLLO
073200910207     C                   Z-ADD     BLDNCD        COMSCD
073300910207     C     BLDNCA        IFEQ      0
073400910207     C                   Z-ADD     BLDNCD        BLDNCA
073500910207     C                   END
073600050418      *
073700050418      * Verifica se spedizione "di valore"
073800050418     c                   exsr      CtrSpeVal
073900910207     C*
074000910219     C                   Z-ADD     BLDNC1        §LDNC1
074100910207     C                   MOVEL     BLPFLS        §LDLNP
074200910207     C     BLPFLS        CHAIN     AZORG                              30
074300910207     C  N30              MOVEL     ORGDES        §LDDLP
074400910207     C   30              MOVEL     *BLANKS       §LDDLP
074500991214     C     BLDCDP        IFNE      *BLANKS
074600910207     C                   MOVEL     BLDCDP        §LDCDP
074700991214     C                   ENDIF
074800910207     C                   MOVEL     BLDNRS        §LDNRS
074900910313     C                   MOVEL     BLDTFA        §LDTNA
075000910207     C                   MOVEL     BLPTSP        §LDTSP
075100910207     C                   MOVEL     BLPZNC        §LDZSC
075200910313     C                   MOVE      BLPNCL        §LDNC2
075300010302     C* SE DEVO STAMPARE ETICHETTA EXPORT DPD PRIMA DELLA RAGIONE SOC.
075400010302     C* MITTENTE DEVO STAMPARE IL RMA SENZA I BLANK NON SIGNIFICATIVI
075500010302     C     WPRMA         IFEQ      '1'
075600010302     C                   CLEAR                   W35
075700010302     C                   MOVEA     BLPRMA        W35
075800010302     C     ' '           CHECKR    BLPRMA        §Z                2 0
075900010302     C                   ADD       2             §Z
076000010302     C                   MOVEA     BLPRSM        W35(§Z)
076100010302     C                   MOVEA     W35           §LDRSM
076200010302     C                   ELSE
076300910207     C                   MOVEL     BLPRSM        §LDRSM
076400010302     C                   ENDIF
076500910207     C                   MOVEL     BLPPRM        §LDPRM
076600910207     C                   MOVEL     BLPRSD        §LDRSD
076700910207     C                   MOVEL     BLPCAD        §LDCAD
076800910207     C                   MOVEL     BLPLOD        §LDLOD
076900910404     C                   MOVEL     BLPIND        §LDIND
077000910207     C                   MOVEL     BLPPRD        §LDPRD
077100950116     C                   MOVE      BLPAAS        §LDAAS
077200950116     C                   MOVE      BLPMGS        §LDMGS
077300950116     C                   MOVE      BLDPDR        §LDCPD
077400910207     C                   MOVEL     *ZEROS        §LDNCP
077500910207     C                   MOVEL     *ZEROS        §LDNCU
077600050418if  1c                   if        §AR5bva = 'V'
077700050418     c                   movel     'X'           §LDrst
077800050418x   1c                   else
077900910415     C                   MOVEL     'R'           §LDRST
078000050418e   1c                   endif
078100941228     C                   Z-ADD     BLPPKF        §LDPKG
078200941228     C                   Z-ADD     BLPVLF        §LDVOL
078300941228     C                   MOVEL     BLPFVF        §LDFVR
078400971219     C*
078500971219     C                   EXSR      ETIEST
078600910208     C*
078700910208     C     COMSCD        DOWLE     BLDNCA
078800990318     C     COMSCD        ANDGT     0
078900910208     C                   SETOFF                                       21
079000910208     C* SE SELEZIONE SEGNACOLLI CONTROLLO CHE SIA NEL RANGE
079100910408     C   07
079200941228     COR 08COMSCD        IFLT      VSCSCD
079300910208     C                   SETON                                        21
079400910208     C                   END
079500910208     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE
079600941228     C   08
079700941228     CANN21COMSCD        IFGT      VSCSCA
079800910208     C                   Z-ADD     BLDNCA        COMSCD
079900910208     C                   SETON                                        21
080000910208     C                   END
080100910408     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      SOLO DAL
080200941228     C   07
080300941228     CANN21COMSCD        IFGT      VSCSCD
080400910409     C                   Z-ADD     BLDNCA        COMSCD
080500910408     C                   SETON                                        21
080600910408     C                   END
080700910208     C* S T A M P A
080800910208     C  N21              MOVEL     COMSCD        §LDNCD
080900020215
081000020215     c                   if        not *in21
081100020215     c                   select
081200020215      * per segnacolli FedEx imposta §LDCDP
081300020215     c                   when      lnantw = 'FED'
081400020215     c                   exsr      sr_fedex
081500020215      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
081600040507     c                   when      lnpntw = 'EEX'
081700020215     C                   EXSR      EUREXP
081800020215     c                   endsl
081900020215     c                   endif
082000020215
082100910208     C*
082200941228     C  N21              CALL      PSS
082300910207     C                   PARM                    DSBLD
082400971219     C                   PARM                    DSBLDE
082500910207     C*
082600910207     C                   ADD       1             COMSCD
082700910219     C                   ADD       1             §LDNC1
082800950419     C*
082900950419     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
083000950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
083100041012     C  n21              ADD       1             WPAGIN
083200080220     c                   IF        NOT  *in21
083300080220     c                             and  wPagin > §VPOmaxLdV
083400950419     C                   MOVEL     'E'           PARFLG
083500950419     C* IMPOSTO 9999999 IN COMSCD PER INTERROMPERE IL CICLO
083600950419     C                   Z-ADD     *ALL'9'       COMSCD
083700950419     C                   ENDIF
083800950419     C*
083900910207     C                   END
084000910208     C*
084100910207     C                   ENDSR
084200910207     C*
084300910207     C*--- CONFRONTO PARZIALIZZAZIONI FATTE -------------------------*
084400910207     C     CONFR         BEGSR
084500910207     C                   SETOFF                                       90
084600941228     C* SIGLA OPERATORE
084700941228     C   03BLPSOP        IFNE      VSCSOP
084800941228     C                   SETON                                        90
084900941228     C                   GOTO      ENDCON
085000941228     C                   END
085100941228     C* LINEA ARRIVO
085200941228     C   06BLPLNA        IFNE      VSCLNA
085300941228     C                   SETON                                        90
085400941228     C                   GOTO      ENDCON
085500941228     C                   END
085600941228     C* FILIALE SEGNACOLLO
085700941228     C   17BLPFLS        IFNE      VSCFLS
085800941228     C                   SETON                                        90
085900941228     C                   GOTO      ENDCON
086000941228     C                   END
086100941228     C*
086200941228     C* CODICE CLIENTE MITTENTE
086300941228     C   09BLPCCM        IFNE      0
086400941228     C     BLPCCM        IFNE      VSCKSC
086500941228     C                   SETON                                            90
086600941228     C                   GOTO      ENDCON
086700941228     C                   END
086800941228     C                   ELSE
086900941228     C     BLPKSC        IFNE      VSCKSC
087000941228     C                   SETON                                            90
087100941228     C                   GOTO      ENDCON
087200941228     C                   END
087300941228     C                   END
087400941228     C* CODICE PADRONCINO
087500941228     C   10BLPPDR        IFNE      VSCCPR
087600941228     C                   SETON                                            90
087700941228     C                   GOTO      ENDCON
087800941228     C                   END
087900950116     C* RITIRO MATTINA/POMERIGGIO
088000950116     C     VSCFPP        IFNE      ' '
088100950116     C     BLPFPP        ANDNE     VSCFPP
088200950116     C                   SETON                                            90
088300950116     C                   GOTO      ENDCON
088400950116     C                   END
088500950116     C* NUMERO RITIRO
088600950116     C     VSCNRT        IFNE      ' '
088700950116     C                   MOVEL     BLPNRT        W001A             1
088800950116     C     W001A         IFNE      VSCNRT
088900950116     C                   SETON                                            90
089000950116     C                   GOTO      ENDCON
089100950116     C                   END
089200950116     C                   END
089300970701     C* SI NO DDT
089400970701     C     VSCBAM        IFNE      ' '
089500970701     C     VSCBAM        IFEQ      'S'
089600970701     C     BLPFST        IFEQ      'N'
089700970701     C     BLPFST        OREQ      'Y'
089800970701     C     BLPFST        OREQ      'K'
089900970701     C                   SETON                                            90
090000970701     C                   GOTO      ENDCON
090100970701     C                   END
090200970701     C                   END
090300970701     C     VSCBAM        IFEQ      'N'
090400970701     C     BLPFST        IFEQ      'S'
090500970701     C     BLPFST        OREQ      'C'
090600970701     C                   SETON                                            90
090700970701     C                   GOTO      ENDCON
090800970701     C                   END
090900970701     C                   END
091000970701     C                   END
091100000614      * SI/NO/SOLO POSTE
091200000614     C                   CLEAR                   OG143
091300000614     C     BLPLNP        CHAIN     AZORG                              30
091400000614     C  N30              MOVEL     ORGDE3        OG143
091500000614      *
091600000614     C     VSCSNP        IFEQ      'N'
091700020215     c     §ogntw        andeq     'PPT'
091800000614     C                   SETON                                            90
091900000614     C                   GOTO      ENDCON
092000000614     C                   ENDIF
092100000614      *
092200000614     C     VSCSNP        IFEQ      'P'
092300020220     c     §ogntw        andne     'PPT'
092400000614     C                   SETON                                            90
092500000614     C                   GOTO      ENDCON
092600000614     C                   ENDIF
092700910207     C*
092800910207     C     ENDCON        ENDSR
092900971217     C**
093000020215      *---------------------------------------------------------------
093100971217     C* STAMPA ETICHETTA ESTERA SDI O BARTOLINI ----------------------*
093200020215      *---------------------------------------------------------------
093300971217     C     ETIEST        BEGSR
093400971217     C*
093500081007     c     §detse        ifeq      'EEX'
093600971219     C* IMPOSTO DSBLD
093700081007     C                   MOVEL     'ITA'         §DETST
093800971219     C**
093900971217     C* CHAIN SULLE NAZIONI
094000971217     C                   MOVEL     '15'          COD
094100971217     C                   MOVEL(P)  BLPNZD        KEY
094200971219     C     KTAB          CHAIN     TABEL                              30
094300971217     C  N30TBLFLG        IFNE      ' '
094400971217     C                   SETON                                        30
094500971217     C                   END
094600971217     C**
094700971217     C  N30              MOVEL     TBLUNI        DS15
094800971217     C   30              CLEAR                   DS15
094900971217     C                   MOVEL     §15COD        §DEISA
095000971217     C**
095100971217     C                   MOVEL     '3A'          COD
095200971217     C                   MOVEL(P)  BLPCBO        KEY
095300971219     C     KTAB          CHAIN     TABEL                              30
095400971217     C  N30TBLFLG        IFNE      ' '
095500971217     C                   SETON                                        30
095600971217     C                   END
095700971217     C**
095800971217     C                   MOVEL     TBLUNI        DS3A
095900971217     C                   MOVEL     §3ATB1        FLGTB1            1
096000971217     C**
096100971217    3C                   SELECT
096200971217     C     FLGTB1        WHENEQ    'A'
096300971217     C     §3AFCA        ANDEQ     0
096400971217     C                   MOVEL     'A'           §DEFCA
096500971217     C     FLGTB1        WHENEQ    'A'
096600971217     C     §3AFCA        ANDNE     0
096700971217     C                   MOVEL     'X'           §DEFCA
096800971217     C     §3AFCA        WHENNE    0
096900971217     C                   MOVEL     'S'           §DEFCA
097000971217    3C                   ENDSL
097100971219     C* ETICHETTA ITALIA
097200971219   X1C                   ELSE
097300971219     C                   MOVEL     'ITA'         §DETST
097400971217    1C                   ENDIF
097500971217     C**
097600971217     C                   ENDSR
097700020215      *----------------------------------------------------------------
097800020215      *--- IMPOSTO CAMPI SEGNACOLLO DA PASSARE A PGM DI STAMPA     ---*
097900020215      *----------------------------------------------------------------
098000020215     C     EUREXP        BEGSR
098100020215
098200020215     C                   MOVEL     'E'           WTRC
098300051012     C     KarS01        CHAIN     FIarS01L                           30
098400980402    2C     *IN30         IFEQ      *OFF
098500980402     C* PASSO AL PGM DI STAMPA ETICHETTE IL N. SEGNACOLLO EUROEXPRESS
098600051012     C                   MOVEL     arSNOT        §DEEUR
098700980402     C* SE IMPORT BARTOLINI METTO NEL CODICE PRODOTTO DELL'ETICHETTA
098800980402     C* ITALIA IL N.SEGNACOLLO EUROEXPRESS
098900020215   3 c                   if        lnpntw = 'EEX'
099000051012     C                   MOVEA     arSNOT        W35
099100980402     C                   Z-ADD     1             BB                2 0
099200980402     C     ' '           LOOKUP    W35(BB)                                30
099300980402     C   30              MOVEA     *BLANKS       W35(BB)
099400980402     C                   MOVEA     W35(6)        §LDCDP
099500980402    3C                   ENDIF
099600980402    2C                   ENDIF
099700980402     C                   ENDSR
099800020215
099900020215      *----------------------------------------------------------------
100000020215      *--- IMPOSTO CAMPI SEGNACOLLO DA PASSARE A PGM DI STAMPA     ---*
100100020215      *---------------------------------------------------------------*
100200020215     c     sr_fedex      begsr
100300020215
100400020215     c                   eval      wtrc = 'F'
100500051012     c     kars01        chain     fIars01l
100600051012     c                   if        %found(fIars01l) and arsnot <> *blanks
100700051012     c                   movel     arsnot        §ldcdp
100800020215     c                   endif
100900020215
101000020215     c                   endsr
101100050418      *---------------------------------------------------------------*
101200050418      *  Verifica se spedizione "DI VALORE"                           *
101300050418      *---------------------------------------------------------------*
101400050418     c     CtrSpeVal     BEGSR
101500050418      *
101600050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
101700050418if  1c                   if            BLPaas = AR5aas
101800050418     c                             and BLPlnp = AR5lnp
101900050418     c                             and BLPnrs = AR5nrs
102000050418     c                             and BLPnsp = AR5nsp
102100050418     c                   leavesr
102200050418e   1c                   endif
102300050418      *
102400050418      * Reperisce i dati da FIAR5
102500050418     c                   clear                   dAR5gen
102600050418     c     K05AR501      setgt     FIAR5000
102700050418do  1c                   dou            %eof(FIAR501L)
102800050418     c                             or   AR5atb =  *blanks
102900050418     c     K05AR501      readpe    FIAR5000
103000050418e   1c                   enddo
103100050418if  1c                   if             NOT %eof(FIAR501L)
103200050418     c                             and  AR5atb =  *blanks
103300050418     c                   movel     AR5uni        dAR5gen
103400050418e   1c                   endif
103500050418      *
103600050418     c                   ENDSR
103700020215
103800941228     OFNLV22P   T    LRNLR
