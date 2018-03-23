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
001300941228     FFNBLT01L  IF   E           K DISK
001400051012     FFIARS01L  IF   E           K DISK
001500941228     FFNBLD01L  IF   E           K DISK
001600050418     fFIAR501L  if   e           k disk
001700941228     FFNLV22P   O    F  132        PRINTER
001800020702
001900020702     d xx              s              2  0
001901081007     d w005a           s              5
002000020702     d comlnp          s                   like(v1clnp)
002100020702
002200980402     D W35             S              1    DIM(35)
002300991201     D W15             S              1    DIM(15)
002400980402     D*
002500900523     D PARAM           DS
002600941228     D  V1CLNP                 1      3  0
002700941228     D  V1CLNA                 4      6  0
002800941228     D  DSD                    7     14  0
002900941228     D  WDSA                  15     22  0
003000941228     D*
003100941228     D  V1CLPD                23     25  0
003200941228     D  V1CSED                26     27  0
003300941228     D  V1CNSD                28     34  0
003400941228     D*
003500941228     D  V1CSEA                35     36  0
003600941228     D  V1CNSA                37     43  0
003700941228     D  V1CSPE                35     43  0
003800941228     D  V1CAAS                44     47  0
003900941228     D*
004000941228     D  VSCSOP                48     48
004100941228     D  VSCLNA                49     51  0
004200941228     D  VSCFLS                52     54  0
004300941228     D  VSCSCD                55     61  0
004400941228     D  VSCSCA                62     68  0
004500941228     D  VSCKSC                69     75  0
004600950116     D*
004700941228     D  VSCCPR                76     82  0
004800950116     D  VSCFPP                83     83
004900950116     D  VSCNRT                84     84
005000970701     D  VSCBAM                85     85
005100000614     D  VSCSNP                86     86
005200080220     d* · num. limite di LdV stampabili (prima FISSO 6000)
005300080220     d  wVPOmaxLdV           151    155s 0 inz
005400950420     D* PARFLG = 'E' --> STAMPATE PIU' DI 6000 PAGINE
005500030613     D  PARFLG               156    156
005600941228     D* PGM DI STAMPA ETICHETTE
005700941228     D  PSS                  207    216
005800910207     D                 DS
005900941228     D  BLPAAS                 1      4  0
006000941228     D  BLPMGS                 5      8  0
006100941228     D  BLPDSP                 1      8  0
006200910903     D                 DS
006300910903     D  BLPNRS                 1      2  0
006400910903     D  BLPNSP                 3      9  0
006500910903     D  BLPSPE                 1      9  0
006600991201     D                 DS
006700991201     D  DSRMN                  1     15
006800991201     D  DSRMN1                 4      6
006900991202     D  DSRMN2                 7     14
007000991202     D  DSRMN3                15     15
007100991214     D                 DS
007200991214     D  DPDSTP                 1     14
007300991214     D  DPDS1                  1      3
007400991214     D  DPDV1                  4      4    INZ
007500991214     D  DPDS2                  5     12
007600991214     D  DPDV2                 13     13    INZ
007700991214     D  DPDS3                 14     14
007800991214     D                 DS
007900991214     D  DPDPAR                 1     12
008000991214     D  DPD1                   1      3
008100991214     D  DPD2                   4     11
008200991214     D  DPD3                  12     12
008300910207     D DS1B          E DS
008400971219     D DS15          E DS
008600971219     D DS3A          E DS
008700910207     D DSBLD         E DS
008800971219     D DSBLDE        E DS
009000060523     D*DSBL4F        E DS
009100060523     D DSBL4I        E DS
009200060523     D DSBL4K        E DS
009300050418     d dAR5gen       e ds                  inz
009400910205     D KPJBA         E DS
009500991201     D OG143         E DS
009600040525     D OG150         E DS
009700020214     d a_og143       e ds                  extname(og143)
009800020214     d                                     prefix(a_)
009900020702
010000020702      *--------
010100020702      * Ds di riferimento al file esterno AzUte00F
010200020702      *--------
010300020702     d AzuteDs       e ds                  ExtName(AzUte00F)
010400020702      *--------
010500020702      * Ds per dati organigramma
010600020702      *--------
010700020702     d DDatiUte      e ds
010800020702      *--------
010900020702      * Parametri per richiamo al pgm di controllo profilo utenti
011000020702      *--------
011100020702     d Tibs34Ds      e ds                  Inz
011200020702      *--------
011300020702      * Parametri per richiamo al pgm trul06r
011400020702      *--------
011500020702     d Trul06Ds      e ds
011600020702     d  lin                    1     90  0 dim(30)
011700080220      *
011800080220      * - Parametri x ricerca/controllo tabelle
011900080220     d TIBS02ds      e ds                  inz
012000080220      * - Tab. VPO = Variabili Programmi Operativi
012100080220     d dVPO          e ds                  inz
012200080220     d  §VPOmaxLdV   e                     inz(10000)
012300020214
012400020214     d lnpntw          s                   like(§ogntw)
012500020214     d lnantw          s                   like(§ogntw)
012700020215
012800000000     C*---------------------------------------------------------------*
012900000000     C     *ENTRY        PLIST
013000000000     C                   PARM                    KPJBA
013100921209     C                   Z-ADD     1             CODUT             1 0
013200900511     C*---------------------------------------------------------------*
013300910207     C* ACCESSO   FLBLP                                    *
013400910207     C     KBLP          KLIST
013500941228     C                   KFLD                    V1CLNP
013600910207     C                   KFLD                    AAS
013700910207     C                   KFLD                    MGS
013800941228     C     KBLP2         KLIST
013900020702     c                   kfld                    comlnp
014000950331     C                   KFLD                    COMLNA
014100941228     C                   KFLD                    AAS
014200941228     C                   KFLD                    MGS
014300910903     C     KBLP4         KLIST
014400941228     C                   KFLD                    V1CAAS
014500941228     C                   KFLD                    V1CLPD
014600941228     C                   KFLD                    V1CSED
014700941228     C                   KFLD                    V1CNSD
014800910903     C     KBLP5         KLIST
014900941228     C                   KFLD                    V1CAAS
015000941228     C                   KFLD                    V1CLPD
015100020702     c     kblp6         klist
015200020702     c                   kfld                    comlnp
015300020702     c                   kfld                    comlna
015400020702
015500910207     C     KBLD          KLIST
015600910207     C                   KFLD                    BLPAAS
015700910207     C                   KFLD                    BLPLNP
015800910207     C                   KFLD                    BLPNRS
015900910207     C                   KFLD                    BLPNSP
016000060220     C     Kar4          KLIST
016100971217     C                   KFLD                    BLPAAS
016200971217     C                   KFLD                    BLPLNP
016300971217     C                   KFLD                    BLPNRS
016400971217     C                   KFLD                    BLPNSP
016500971217     C                   KFLD                    WTRC
016600910207     C* ACCESSO   TABEL                                    *
016700910207     C     KTAB          KLIST
016800910207     C                   KFLD                    CODUT
016900910207     C                   KFLD                    COD               2
017000910207     C                   KFLD                    KEY               8
017100980402     C*
017200051012     C     KarS01        KLIST
017300980402     C                   KFLD                    BLPFLS
017400980402     C                   KFLD                    BLPLNA
017500980402     C                   KFLD                    BLPNRS
017600980402     C                   KFLD                    §LDNCD
017700980402     C                   KFLD                    WTRC
017800050418      * FIAR501L
017900050418     c     k05AR501      klist
018000050418     c                   kfld                    BLPaas
018100050418     c                   kfld                    BLPlnp
018200050418     c                   kfld                    BLPnrs
018300050418     c                   kfld                    BLPnsp
018400050418     c                   kfld                    AR5trd
018500050418     c                   eval      AR5trd = 'GEN'
018600950331     C*
018700950331     C     *LIKE         DEFINE    BLPLNA        COMLNA
018800060220     C     *LIKE         DEFINE    ar4TRC        WTRC
018900910205     C*---------------------------------------------------------------*
019000910205     C                   MOVEL     KPJBU         PARAM
019100020702
019200020702      * reperisco i dati utente
019300020702     c     *dtaara       define    §azute        AzuteDs
019400020702     c     *dtaara       define    §datiute      DDatiUte
019500020702
019600020702     c                   in(E)     *dtaara
019700020702     c                   IF        %Error or RSUT = *blanks
019800020702     c                   call      'TIBS34R'
019900020702     c                   parm                    Tibs34Ds
020000020702     c                   in        *dtaara
020100020702     c                   ENDIF
020200020702
020300971217     C*
020400971217     C                   CLEAR                   DSBLD
020500971217     C**
020600910207     C* VEDO LE PARZIALIZZAZIONI EFFETTUATE
020700910207     C                   EXSR      PREPA
020800950331     C*
020900020702      * stampa per lna imposto la prima linea di partenza
021000020702     c                   if        *in11
021100020702     c                   z-add     1             xx
021200020702     c                   movel     lin(xx)       comlnp
021300020702     c                   endif
021400020702      * stampa per lna imposto la chiave se non richieste tutte
021500950331     C     *IN11         IFEQ      *ON
021600950331     C     V1CLNA        ANDEQ     999
021700950331     C* Scelte tutte le linee di arrivo: posizionamento sulla prima
021800020702     c     comlnp        setll     fnblp76l
021900020702     c                   read      fnblp76l                               31
022000950331     C  N31              Z-ADD     BLPLNA        COMLNA
022100950331     C                   ELSE
022200950331     C                   Z-ADD     V1CLNA        COMLNA
022300950331     C                   END
022400020702      * stampa per lnp e richiesta nelle parzializzazioni la lna imposto
022500020702      * la chiave
022600020702     c                   if        *in01 and *in06
022700020702     c                   movel     v1clnp        comlnp
022800020702     c                   movel     vsclna        comlna
022900020702     c                   endif
023000950331     C*
023100950331     C     *IN31         DOWEQ     *OFF
023200941228     C***
023300941228     C* PER NUMERO SPEDIZIONE
023400941228     C   12KBLP4         SETLL     FNBLP01L
023500020702
023600941228     C* PER LNP E  DATA   SPEDIZIONE
023700020702      * no lna nelle selezioni
023800020702     c  n06
023900020702     Can 01KBLP          SETLL     FNBLP31L
024000020702      * con lna nelle selezioni
024100020702     c   06
024200020702     can 01kblp2         setll     fnblp76l
024300020702
024400941228     C* PER LNA E  DATA   SPEDIZIONE
024500020702     c   11kblp2         setll     fnblp76l
024600941228     C*
024700941228     C     *IN31         DOWEQ     *OFF
024800020702
024900941228     C* PER NUMERO SPEDIZIONE
025000941228     C   12
025100941228     CANN05KBLP4         READE     FNBLP01L                               31
025200941228     C   12
025300941228     CAN 05KBLP5         READE     FNBLP01L                               31
025400020702
025500941228     C* PER LNP E  DATA   SPEDIZIONE
025600020702      * no lna nelle selezioni
025700020702     c                   if        not *in06
025800010717     C   01
025900010717     CANN02KBLP          READE     FNBLP31L                               31
026000010717     C   01
026100010717     CAN 02V1CLNP        READE     FNBLP31L                               31
026200020702     c                   else
026300020702      * con lna nelle selezioni
026400020702     c   01
026500020702     cann02kblp2         reade     fnblp76l                               31
026600020702     c   01
026700020702     can 02kblp6         reade     fnblp76l                               31
026800020702     c                   endif
026900020702
027000941228     C* PER LNA E  DATA   SPEDIZIONE
027100020702     c   11
027200020702     cann02kblp2         reade     fnblp76l                               31
027300020702     c   11
027400020702     can 02kblp6         reade     fnblp76l                               31
027500020702
027600910207     C***
027700910903     C*
027800910903     C* CON NUMERO AL VEDO SE COMPRESA
027900941228     C   12
028000941228     CAN 05
028100941228     CANN31BLPSPE        IFGT      V1CSPE
028200910903     C                   SETON                                        31
028300910903     C                   END
028400910903     C*
028500910903     C* CON DATA   AL VEDO SE COMPRESA
028600941228     C  N12
028700941228     CAN 02
028800941228     CANN31BLPDSP        IFGT      WDSA
028900910903     C                   SETON                                        31
029000910903     C                   END
029100910903     C  N31              EXSR      CONFR
029200910207     C*
029300910903     C  N31
029400910903     CANN90              EXSR      CONTR
029500950419     C*
029600950420     C* SE SUPERATE LE 6000 PAGINE (PARFLG='E'), VADO A FINE PGM
029700950419     C     PARFLG        IFEQ      'E'
029800950419     C                   GOTO      FINE
029900950419     C                   ENDIF
030000910207     C*
030100910205     C                   END
030200950331     C*
030300020702      * Richiesta di tutte le linee di arrivo
030400950331     C     *IN11         IFEQ      *ON
030500950331     C     V1CLNA        ANDEQ     999
030600020702      * cerco linea di arrivo sucessiva
030700020702     c     kblp6         setgt     fnblp76l
030800020702     c                   read      fnblp76l                               31
030900020702     c                   if        blplnp = comlnp
031000020702     c                   z-add     blplna        comlna
031100020702      * cerco la linea di partenza sucessiva
031200020702     c                   else
031300020702     c                   add       1             xx
031400020702      * non ci sono più linee di partenza vado a fine
031500020702     c                   if        lin(xx) = *zeros
031600020702     c                   goto      fine
031700020702     c                   endif
031800020702      * cerco la prima linea di arrivo della linea di partenza che
031900020702      * devo leggere
032000020702     c                   eval      comlnp = lin(xx)
032100020702     c     comlnp        setll     fnblp76l
032200020702     c                   read      fnblp76l                               31
032300020702     c  n31              z-add     blplna        comlna
032400020702     c                   endif
032500950331     C                   END
032600020702
032700020702      * richiesta solo una linea di arrivo
032800020702     c                   if        *in11 and v1clna <> 999
032900020702     c                   eval      *in31 = *off
033000020702      * cerco la linea di partenza sucessiva
033100020702     c                   add       1             xx
033200020702     c                   if        lin(xx) = *zeros
033300020702      * non ci sono più linee di partenza vado a fine
033400020702     c                   goto      fine
033500020702     c                   endif
033600020702     c                   eval      comlnp = lin(xx)
033700020702     c                   endif
033800950331     C*
033900950331     C                   ENDDO
034000921209     C*
034100950419     C     FINE          TAG
034200020702     C                   MOVEL(p)  PARAM         KPJBU
034300000000     C                   SETON                                        LR
034400910207     C*
034500910219     C*--- PREPARO VEDO LE PARZIALIZZAZIONE FATTE -------------------*
034600910207     C     PREPA         BEGSR
034700941228     C**
034800941228     C* CONTROLLO COME DEVO LEGGERE
034900941228     C**
035000941228     C* PER LINEA DI PARTENZA
035100941228     C     V1CLNP        COMP      0                                  01
035200941228     C* PER LINEA DI ARRIVO
035300941228     C     V1CLNA        COMP      0                                  11
035400941228     C* PER NUMERO SPEDIZIONE
035500941228     C     V1CLPD        COMP      0                                  12
035600941228     C**
035700950331     C* CONTROLLO LE PARZIALIZZAZIONI MESSE
035800941228     C**
035900941228     C* NUMERO SPEDIZIONE AL
036000941228     C     V1CNSA        COMP      0                                  05
036100941228     C* DATA AL
036200941228     C     WDSA          COMP      0                                  02
036300941228     C* SIGLA OPERATORE
036400941228     C     VSCSOP        COMP      *BLANKS                            0303
036500941228     C* LINEA DI ARRIVO
036600941228     C     VSCLNA        COMP      0                                  06
036700941228     C* SEGNACOLLO AL
036800941228     C     VSCSCA        COMP      0                                  08
036900941228     C* SEGNACOLLO DAL
037000941228     C  N08VSCSCD        COMP      0                                  07
037100941228     C* FILIALE SEGNACOLLO
037200941228     C     VSCFLS        COMP      0                                  17
037300941228     C* CODICE CLIENTE MITTENTE
037400941228     C     VSCKSC        COMP      0                                  09
037500941228     C* CODICE PADRONCINO
037600941228     C     VSCCPR        COMP      0                                  10
037700910207     C*
037800941228     C                   MOVEL     DSD           AAS               4 0
037900910207     C                   MOVE      DSD           MGS               4 0
038000910903     C                   SETOFF                                       31
038100950418     C*
038200950418     C* AZZERO IL NUMERATORE PAGINE
038300950418     C                   Z-ADD     *ZERO         WPAGIN            5 0
038400020702      * stampa per lna
038500020702     c                   if        *in11
038600020702      * se p.o. di secondo livello imposto nella sk delle lnp se
038700020702      * stesso
038800020702     c                   clear                   lin
038900020702     c                   if        dutlpo = '2' or dutlpo = *blanks
039000020702     c                   eval      lin(1) = dutpou
039100020702      * se p.o. di primo livello carico tutti i p.o. del terminal di partenza
039200020702     c                   else
039300020702     c                   clear                   Trul06Ds
039400020702     c                   eval      D06Cod = '£1'
039500020702     c                   eval      D06Tla = 'L'
039600020702     c                   movel     Simfel        D06Key
039700020702     c                   movel(p)  Trul06Ds      Kpjbu
039800020702     c                   call      'TRUL06R'
039900020702     c                   parm                    Kpjba
040000020702     c                   movel     Kpjbu         Trul06Ds
040100020702     c                   endif
040200020702     c                   endif
040300020702
040400080220      * Aggancio tabella "VPO" per reperimento il n° LdV stampabili
040500080220      *   limite (per congelamento e blocco stampa)
040600080220     c                   reset                   dVPO
040700080220     c                   clear                   TIBS02ds
040800080220     c                   eval      T02tla  = 'L'
040900080220     c                   eval      T02mod  = 'C'
041000080220     c                   eval      T02sif  = KNSIF
041100080220     c                   eval      T02cod  = 'VPO'
041200080220     c                   movel(P)  'VPO'         T02ke1
041300080220     c                   call      'TIBS02R'
041400080220     c                   parm                    KPJBA
041500080220     c                   parm                    TIBS02ds
041600080220     c                   if        T02err  = *blanks
041700080220     c                   movel     T02uni        dVPO
041800080220     c                   endif
041900080220     c                   eval      wVPOmaxLdV = §VPOmaxLdV
042000080220      *
042100910207     C                   ENDSR
042200910207     C*
042300910207     C*--- CONTROLLO SE POSSO STAMPARE SEGNACOLLO -------------------*
042400910207     C     CONTR         BEGSR
042500000201     C                   CLEAR                   DSBLDE
042600010302     C                   CLEAR                   WPRMA
042700910207     C* PRENDO CODICE TRATTAMENTO MERCE
042800971219     C                   MOVEL(P)  BLPCTM        KEY
042900910207     C                   MOVEL     '1B'          COD
043000910207     C     KTAB          CHAIN     TABEL                              30
043100910207     C  N30              MOVEL     TBLUNI        DS1B
043200971219     C   30              CLEAR                   DS1B
043300971219     C**
043400971219     C* CHAIN CON LINEA DI PARTENZA
043500971219     C     BLPLNP        CHAIN     AZORG                              30
043600991201     C                   MOVEL     ORGDE3        OG143
043700020214     c                   eval      lnpntw = §ogntw
043800971219     C**
043900971219     C* IMPOSTO LA LINEA DI ARRIVO
044000971219     C                   MOVEL     BLPLNA        §LDLNA
044100971219     C     BLPLNA        CHAIN     AZORG                              30
044200971219     C  N30ORGDE5        IFNE      *BLANKS
044300971219     C                   MOVEL     ORGDE5        §LDDLA
044400971219     C                   ELSE
044500971219     C                   MOVEL     ORGDES        §LDDLA
044600971219     C                   END
044700971219     C   30              MOVEL     *BLANKS       §LDDLA
044800020214     c                   eval      a_og143 = orgde3
044900020214     c                   eval      lnantw = a_§ogntw
045000040525     c                   movel     orgtel        dpdtel           15
045100040525     c                   movel     orgfax        dpdfax           15
045200040525     c                   movel     orgdf0        og150
045300991206      *
045400991206      * RIFERIMENTO MITTENTE ALFABETICO, SE LNP DPD SCRIVO NUMERICO
045500020215     c                   if        lnpntw <> 'DPD'
045600971219     C                   MOVEL     BLPRMA        §LDCDP
045700060523     C***                ELSE
045800060523     C***                MOVEL     BLPRMN        DSRMN
045900060523     C***                CLEAR                   W15
046000060523     C***                MOVEA     DSRMN1        W15(1)
046100060523     C***                MOVEA     DSRMN2        W15(5)
046200060523     C***                MOVEA     DSRMN3        W15(14)
046300060523     C***                MOVEA     W15           §LDCDP
046400991201    1C                   ENDIF
046500991206      **
046600060220      **  CHAIN Fiar4 PER EXPORT DPD
046700060523     c                   if        lnantw = 'DPD' or lnpntw='DPD'
046800060523     C                   MOVEL     'I'           WTRC
046900060220     C     Kar4          CHAIN     Fiar401L                           39
047000060523     C  N39              MOVEL     ar4NOT        DSBL4I
047100060523     C   39              CLEAR                   DSBL4I
047200060523    2C     §B4ipn        IFNE      *BLANKS
047300060523     C                   MOVEL     §B4ipn        DPDPAR
047400991214     C                   MOVEL     DPD1          DPDS1
047500991214     C                   MOVEL     DPD2          DPDS2
047600060523     C                   MOVEL     §b4icd        DPDS3
047700991214     C                   MOVEL(P)  DPDSTP        §LDCDP
047800060523     c                   endif
047900060523     c                   endif
048000060523     c*
048100060523     c                   if        lnantw = 'DPD' and §b4ipn <> *blanks
048200060523     C* SE RMA DIVERSO DA BLANK MEMORIZZO CHE DEVO STAMPARLO PRIMA
048300060523     C* DELLA RAGIONE SOCIALE MITENTE
048400060523     C     BLPRMA        IFNE      *BLANKS
048500060523     C                   MOVE      '1'           WPRMA             1
048600060523     C                   ENDIF
049500000201     C     BLPPKF        IFLE      3
049600000201     C                   MOVEL     'X'           §DESSE
049700000201     C                   ENDIF
049800060523     c*
049900060523     C                   MOVEL     'K'           WTRC
050000060523     C     Kar4          CHAIN     Fiar401L                           39
050100060523     C  N39              MOVEL     ar4NOT        DSBL4K
050200060523     C   39              CLEAR                   DSBL4K
050300000201     C*
050400060523     C                   MOVEL     blprsd        §DESIA
050500060523     C                   MOVEL     §B4krt        §DEAWB
050600060523     C                   MOVEL     §B4ipn        §DEEUR
050700060523     c                   eval      %subst(§deeur:12:1) = §b4icd
050800000201     C                   MOVEL     DPDTEL        §DEXX2
050900000201     C                   MOVE      DPDFAX        §DEXX2
051000060523     c                   eval      w005a = %subst(§b4ida:2:3) + §b4iza
051100000201     C                   MOVE      W005A         §DEXX1
052200991206    2C                   ENDIF
052400991206      **
052600000201     C** IMPOSTO CAMPI FISSI SERVONO SOLO PER EXPORT
052700020214
054400000207     C** IMPOSTO EUR IN TS PER EUROEXPRESS
054500020214
054600020214      * Se arrivo EuroExpress
054700040507      * imposta EEX
054800020214     c                   if        lnantw = 'EEX'
054900040507     C                   MOVEL     'EEX'         §DETSE
055000000207     C                   ENDIF
057100921209     C*
057200910207     C* FLAG6 DI STAMPA BOLLA
057300950419    1C     §1BFG6        IFNE      'D'
057400971219     C     §1BFG9        ANDNE     'S'
057500971219     C**
057600971219    2C     BLPNCD        IFGT      0
057700910207     C                   EXSR      STASCI
057800921209     C*
057900971219   X2C                   ELSE
058000921209     C*
058100921209     C* STAMPO ANCHE SE NON SONO SEQUENZIALI
058200971219    3C     BLPFNS        IFEQ      'S'
058300921209     C                   EXSR      STASCN
058400971219    3C                   ENDIF
058500971219    2C                   ENDIF
058600921209     C*
058700950419   X1C                   ELSE
058800910207     C*
058900941228     C     KBLD          SETLL     FNBLD000
059000921209     C                   SETOFF                                       32
059100921209     C*
059200971219    2C     *IN32         DOWEQ     *OFF
059300941228     C     KBLD          READE     FNBLD000                               32
059400971219    3C  N32BLDFST        IFEQ      'S'
059500971219    3C     BLDFST        OREQ      'Z'
059600910207     C                   EXSR      STASCD
059700950420     C* SE SUPERATE LE 6000 PAGINE (PARFLG='E'), INTERROMPO IL CICLO
059800971219    3C     PARFLG        IFEQ      'E'
059900950419     C                   SETON                                        32
060000971219    3C                   ENDIF
060100971219    3C                   ENDIF
060200971219    2C                   ENDDO
060300910207     C*
060400971219    1C                   ENDIF
060500910207     C*
060600910207     C                   ENDSR
060700910207     C*
060800921209     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI SEQUENZIALI ---------*
060900910207     C     STASCI        BEGSR
061000910207     C                   Z-ADD     BLPNCD        COMSCD            7 0
061100910207     C     BLPNCA        IFEQ      0
061200910207     C                   Z-ADD     BLPNCD        BLPNCA
061300910207     C                   END
061400921209     C* IMPOSTO CAMPI DI STAMPA
061500921209     C                   EXSR      IMPO
061600910208     C*
061700910208     C     COMSCD        DOWLE     BLPNCA
061800990318     C     COMSCD        ANDGT     0
061900910208     C                   ADD       1             §LDNC1
062000910208     C                   SETOFF                                       21
062100910208     C* SE SELEZIONE SEGNACOLLI CONTROLLO CHE SIA NEL RANGE
062200910403     C   07
062300941228     COR 08COMSCD        IFLT      VSCSCD
062400910208     C                   SETON                                        21
062500910208     C                   END
062600910403     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      DAL / AL
062700941228     C   08
062800941228     CANN21COMSCD        IFGT      VSCSCA
062900910208     C                   Z-ADD     BLPNCA        COMSCD
063000910208     C                   SETON                                        21
063100910208     C                   END
063200910403     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      SOLO DAL
063300941228     C   07
063400941228     CANN21COMSCD        IFGT      VSCSCD
063500910403     C                   Z-ADD     BLPNCA        COMSCD
063600910403     C                   SETON                                        21
063700910403     C                   END
063800910208     C* S T A M P A
063900910208     C  N21              MOVEL     COMSCD        §LDNCD
064000020215
064100020215     c                   if        not *in21
064200020215     c                   select
064300020215      * per segnacolli FedEx imposta §LDCDP
064400020215     c                   when      lnantw = 'FED'
064500020215     c                   exsr      sr_fedex
064600020215      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
064700040507     c                   when      lnpntw = 'EEX'
064800020215     C                   EXSR      EUREXP
064900020215     c                   endsl
065000020215     c                   endif
065100020215
065200910208     C*
065300941228     C  N21              CALL      PSS
065400910207     C                   PARM                    DSBLD
065500971219     C                   PARM                    DSBLDE
065600910207     C*
065700910207     C                   ADD       1             COMSCD
065800950419     C*
065900950419     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
066000950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
066100041012     C  n21              ADD       1             WPAGIN
066200080220     c                   IF        NOT  *in21
066300080220     c                             and  wPagin > §VPOmaxLdV
066400950419     C                   MOVEL     'E'           PARFLG
066500950419     C* IMPOSTO 9999999 IN COMSCD PER INTERROMPERE IL CICLO
066600950419     C                   Z-ADD     *ALL'9'       COMSCD
066700950419     C                   ENDIF
066800950419     C*
066900910207     C                   END
067000910208     C*
067100910207     C                   ENDSR
067200921209     C*
067300921209     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI NON SEQUENZIALI -----*
067400921209     C     STASCN        BEGSR
067500921209     C*
067600921209     C                   EXSR      IMPO
067700921209     C*
067800941228     C     KBLD          SETLL     FNBLT01L
067900941228     C     KBLD          READE     FNBLT01L                               33
068000921209     C*
068100941228     C     *IN33         DOWEQ     *OFF
068200921209     C*
068300941228     C                   Z-ADD     BLTNSC        COMSCD
068400921209     C                   ADD       1             §LDNC1
068500921209     C                   SETOFF                                       21
068600921209     C* SE SELEZIONE SEGNACOLLI CONTROLLO CHE SIA NEL RANGE
068700921209     C   07
068800941228     COR 08COMSCD        IFLT      VSCSCD
068900921209     C                   SETON                                        21
069000921209     C                   END
069100921209     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      DAL / AL
069200941228     C   08
069300941228     CANN21COMSCD        IFGT      VSCSCA
069400921209     C                   SETON                                        2133
069500921209     C                   END
069600921209     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      SOLO DAL
069700941228     C   07
069800941228     CANN21COMSCD        IFGT      VSCSCD
069900921209     C                   SETON                                        2133
070000921209     C                   END
070100921209     C* S T A M P A
070200921209     C  N21              MOVEL     COMSCD        §LDNCD
070300020215
070400020215     c                   if        not *in21
070500020215     c                   select
070600020215      * per segnacolli FedEx imposta §LDCDP
070700020215     c                   when      lnantw = 'FED'
070800020215     c                   exsr      sr_fedex
070900020215      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
071000040507     c                   when      lnpntw = 'EEX'
071100020215     C                   EXSR      EUREXP
071200020215     c                   endsl
071300020215     c                   endif
071400020215
071500921209     C*
071600941228     C  N21              CALL      PSS
071700921209     C                   PARM                    DSBLD
071800971219     C                   PARM                    DSBLDE
071900950418     C*
072000950418     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
072100950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
072200041012     C  n21              ADD       1             WPAGIN
072300080220     c                   IF        NOT  *in21
072400080220     c                             and  wPagin > §VPOmaxLdV
072500950419     C                   MOVEL     'E'           PARFLG
072600950419     C* ACCENDO *IN33  PER INTERROMPERE IL CICLO
072700950419     C                   SETON                                        33
072800950419     C                   ENDIF
072900921209     C*
073000941228     C  N33KBLD          READE     FNBLT01L                               33
073100921209     C                   END
073200921209     C*
073300921209     C                   ENDSR
073400921209     C*
073500921209     C* IMPOSTO CAMPI PER STAMPA IN IMMEDIATO ------------------------*
073600921209     C     IMPO          BEGSR
073700050418      *
073800050418      * Verifica se spedizione "di valore"
073900050418     c                   exsr      CtrSpeVal
074000921209     C*
074100921209     C                   Z-ADD     0             §LDNC1
074200921209     C                   MOVEL     BLPFLS        §LDLNP
074300921209     C     BLPFLS        CHAIN     AZORG                              30
074400921209     C  N30              MOVEL     ORGDES        §LDDLP
074500921209     C   30              MOVEL     *BLANKS       §LDDLP
074600921209     C                   MOVEL     BLPNRS        §LDNRS
074700921209     C                   MOVEL     BLPTFA        §LDTNA
074800921209     C                   MOVEL     BLPTSP        §LDTSP
074900921209     C                   MOVEL     BLPZNC        §LDZSC
075000921209     C                   MOVE      BLPNCL        §LDNC2
075100010302     C* SE DEVO STAMPARE ETICHETTA EXPORT DPD PRIMA DELLA RAGIONE SOC.
075200010302     C* MITTENTE DEVO STAMPARE IL RMA SENZA I BLANK NON SIGNIFICATIVI
075300010302     C     WPRMA         IFEQ      '1'
075400010302     C                   CLEAR                   W35
075500010302     C                   MOVEA     BLPRMA        W35
075600010302     C     ' '           CHECKR    BLPRMA        §Z                2 0
075700010302     C                   ADD       2             §Z
075800010302     C                   MOVEA     BLPRSM        W35(§Z)
075900010302     C                   MOVEA     W35           §LDRSM
076000010302     C                   ELSE
076100921209     C                   MOVEL     BLPRSM        §LDRSM
076200010302     C                   ENDIF
076300921209     C                   MOVEL     BLPPRM        §LDPRM
076400921209     C                   MOVEL     BLPRSD        §LDRSD
076500921209     C                   MOVEL     BLPCAD        §LDCAD
076600921209     C                   MOVEL     BLPLOD        §LDLOD
076700921209     C                   MOVEL     BLPIND        §LDIND
076800921209     C                   MOVEL     BLPPRD        §LDPRD
076900941228     C                   MOVE      BLPPDR        §LDCPD
077000921209     C                   MOVEL     *ZEROS        §LDNCP
077100921209     C                   MOVEL     *ZEROS        §LDNCU
077200050418if  1c                   if        §AR5bva = 'V'
077300050418     c                   movel     'X'           §LDrst
077400050418x   1c                   else
077500921209     C                   MOVEL     'R'           §LDRST
077600050418e   1c                   endif
077700941228     C                   Z-ADD     BLPPKF        §LDPKG
077800941228     C                   Z-ADD     BLPVLF        §LDVOL
077900941228     C                   MOVEL     BLPFVF        §LDFVR
078000950116     C                   MOVEL     BLPAAS        §LDAAS
078100950116     C                   MOVEL     BLPMGS        §LDMGS
078200971219     C*
078300971219     C                   EXSR      ETIEST
078400980402     C*
078500921209     C                   ENDSR
078600921209     C*
078700910207     C*--- STAMPA BOLLA IN IMMEDIATO  COME RISTAMPA DEL "D" ----------*
078800910207     C     STASCD        BEGSR
078900910207     C* RICHIAMO DIRETTAMENTE IL PROGRAMMA DI STAMPA SEGNACOLLO
079000910207     C                   Z-ADD     BLDNCD        COMSCD
079100910207     C     BLDNCA        IFEQ      0
079200910207     C                   Z-ADD     BLDNCD        BLDNCA
079300910207     C                   END
079400050418      *
079500050418      * Verifica se spedizione "di valore"
079600050418     c                   exsr      CtrSpeVal
079700910207     C*
079800910219     C                   Z-ADD     BLDNC1        §LDNC1
079900910207     C                   MOVEL     BLPFLS        §LDLNP
080000910207     C     BLPFLS        CHAIN     AZORG                              30
080100910207     C  N30              MOVEL     ORGDES        §LDDLP
080200910207     C   30              MOVEL     *BLANKS       §LDDLP
080300991214     C     BLDCDP        IFNE      *BLANKS
080400910207     C                   MOVEL     BLDCDP        §LDCDP
080500991214     C                   ENDIF
080600910207     C                   MOVEL     BLDNRS        §LDNRS
080700910313     C                   MOVEL     BLDTFA        §LDTNA
080800910207     C                   MOVEL     BLPTSP        §LDTSP
080900910207     C                   MOVEL     BLPZNC        §LDZSC
081000910313     C                   MOVE      BLPNCL        §LDNC2
081100010302     C* SE DEVO STAMPARE ETICHETTA EXPORT DPD PRIMA DELLA RAGIONE SOC.
081200010302     C* MITTENTE DEVO STAMPARE IL RMA SENZA I BLANK NON SIGNIFICATIVI
081300010302     C     WPRMA         IFEQ      '1'
081400010302     C                   CLEAR                   W35
081500010302     C                   MOVEA     BLPRMA        W35
081600010302     C     ' '           CHECKR    BLPRMA        §Z                2 0
081700010302     C                   ADD       2             §Z
081800010302     C                   MOVEA     BLPRSM        W35(§Z)
081900010302     C                   MOVEA     W35           §LDRSM
082000010302     C                   ELSE
082100910207     C                   MOVEL     BLPRSM        §LDRSM
082200010302     C                   ENDIF
082300910207     C                   MOVEL     BLPPRM        §LDPRM
082400910207     C                   MOVEL     BLPRSD        §LDRSD
082500910207     C                   MOVEL     BLPCAD        §LDCAD
082600910207     C                   MOVEL     BLPLOD        §LDLOD
082700910404     C                   MOVEL     BLPIND        §LDIND
082800910207     C                   MOVEL     BLPPRD        §LDPRD
082900950116     C                   MOVE      BLPAAS        §LDAAS
083000950116     C                   MOVE      BLPMGS        §LDMGS
083100950116     C                   MOVE      BLDPDR        §LDCPD
083200910207     C                   MOVEL     *ZEROS        §LDNCP
083300910207     C                   MOVEL     *ZEROS        §LDNCU
083400050418if  1c                   if        §AR5bva = 'V'
083500050418     c                   movel     'X'           §LDrst
083600050418x   1c                   else
083700910415     C                   MOVEL     'R'           §LDRST
083800050418e   1c                   endif
083900941228     C                   Z-ADD     BLPPKF        §LDPKG
084000941228     C                   Z-ADD     BLPVLF        §LDVOL
084100941228     C                   MOVEL     BLPFVF        §LDFVR
084200971219     C*
084300971219     C                   EXSR      ETIEST
084400910208     C*
084500910208     C     COMSCD        DOWLE     BLDNCA
084600990318     C     COMSCD        ANDGT     0
084700910208     C                   SETOFF                                       21
084800910208     C* SE SELEZIONE SEGNACOLLI CONTROLLO CHE SIA NEL RANGE
084900910408     C   07
085000941228     COR 08COMSCD        IFLT      VSCSCD
085100910208     C                   SETON                                        21
085200910208     C                   END
085300910208     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE
085400941228     C   08
085500941228     CANN21COMSCD        IFGT      VSCSCA
085600910208     C                   Z-ADD     BLDNCA        COMSCD
085700910208     C                   SETON                                        21
085800910208     C                   END
085900910408     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      SOLO DAL
086000941228     C   07
086100941228     CANN21COMSCD        IFGT      VSCSCD
086200910409     C                   Z-ADD     BLDNCA        COMSCD
086300910408     C                   SETON                                        21
086400910408     C                   END
086500910208     C* S T A M P A
086600910208     C  N21              MOVEL     COMSCD        §LDNCD
086700020215
086800020215     c                   if        not *in21
086900020215     c                   select
087000020215      * per segnacolli FedEx imposta §LDCDP
087100020215     c                   when      lnantw = 'FED'
087200020215     c                   exsr      sr_fedex
087300020215      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
087400040507     c                   when      lnpntw = 'EEX'
087500020215     C                   EXSR      EUREXP
087600020215     c                   endsl
087700020215     c                   endif
087800020215
087900910208     C*
088000941228     C  N21              CALL      PSS
088100910207     C                   PARM                    DSBLD
088200971219     C                   PARM                    DSBLDE
088300910207     C*
088400910207     C                   ADD       1             COMSCD
088500910219     C                   ADD       1             §LDNC1
088600950419     C*
088700950419     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
088800950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
088900041012     C  n21              ADD       1             WPAGIN
089000080220     c                   IF        NOT  *in21
089100080220     c                             and  wPagin > §VPOmaxLdV
089200950419     C                   MOVEL     'E'           PARFLG
089300950419     C* IMPOSTO 9999999 IN COMSCD PER INTERROMPERE IL CICLO
089400950419     C                   Z-ADD     *ALL'9'       COMSCD
089500950419     C                   ENDIF
089600950419     C*
089700910207     C                   END
089800910208     C*
089900910207     C                   ENDSR
090000910207     C*
090100910207     C*--- CONFRONTO PARZIALIZZAZIONI FATTE -------------------------*
090200910207     C     CONFR         BEGSR
090300910207     C                   SETOFF                                       90
090400941228     C* SIGLA OPERATORE
090500941228     C   03BLPSOP        IFNE      VSCSOP
090600941228     C                   SETON                                        90
090700941228     C                   GOTO      ENDCON
090800941228     C                   END
090900941228     C* LINEA ARRIVO
091000941228     C   06BLPLNA        IFNE      VSCLNA
091100941228     C                   SETON                                        90
091200941228     C                   GOTO      ENDCON
091300941228     C                   END
091400941228     C* FILIALE SEGNACOLLO
091500941228     C   17BLPFLS        IFNE      VSCFLS
091600941228     C                   SETON                                        90
091700941228     C                   GOTO      ENDCON
091800941228     C                   END
091900941228     C*
092000941228     C* CODICE CLIENTE MITTENTE
092100941228     C   09BLPCCM        IFNE      0
092200941228     C     BLPCCM        IFNE      VSCKSC
092300941228     C                   SETON                                            90
092400941228     C                   GOTO      ENDCON
092500941228     C                   END
092600941228     C                   ELSE
092700941228     C     BLPKSC        IFNE      VSCKSC
092800941228     C                   SETON                                            90
092900941228     C                   GOTO      ENDCON
093000941228     C                   END
093100941228     C                   END
093200941228     C* CODICE PADRONCINO
093300941228     C   10BLPPDR        IFNE      VSCCPR
093400941228     C                   SETON                                            90
093500941228     C                   GOTO      ENDCON
093600941228     C                   END
093700950116     C* RITIRO MATTINA/POMERIGGIO
093800950116     C     VSCFPP        IFNE      ' '
093900950116     C     BLPFPP        ANDNE     VSCFPP
094000950116     C                   SETON                                            90
094100950116     C                   GOTO      ENDCON
094200950116     C                   END
094300950116     C* NUMERO RITIRO
094400950116     C     VSCNRT        IFNE      ' '
094500950116     C                   MOVEL     BLPNRT        W001A             1
094600950116     C     W001A         IFNE      VSCNRT
094700950116     C                   SETON                                            90
094800950116     C                   GOTO      ENDCON
094900950116     C                   END
095000950116     C                   END
095100970701     C* SI NO DDT
095200970701     C     VSCBAM        IFNE      ' '
095300970701     C     VSCBAM        IFEQ      'S'
095400970701     C     BLPFST        IFEQ      'N'
095500970701     C     BLPFST        OREQ      'Y'
095600970701     C     BLPFST        OREQ      'K'
095700970701     C                   SETON                                            90
095800970701     C                   GOTO      ENDCON
095900970701     C                   END
096000970701     C                   END
096100970701     C     VSCBAM        IFEQ      'N'
096200970701     C     BLPFST        IFEQ      'S'
096300970701     C     BLPFST        OREQ      'C'
096400970701     C                   SETON                                            90
096500970701     C                   GOTO      ENDCON
096600970701     C                   END
096700970701     C                   END
096800970701     C                   END
096900000614      * SI/NO/SOLO POSTE
097000000614     C                   CLEAR                   OG143
097100000614     C     BLPLNP        CHAIN     AZORG                              30
097200000614     C  N30              MOVEL     ORGDE3        OG143
097300000614      *
097400000614     C     VSCSNP        IFEQ      'N'
097500020215     c     §ogntw        andeq     'PPT'
097600000614     C                   SETON                                            90
097700000614     C                   GOTO      ENDCON
097800000614     C                   ENDIF
097900000614      *
098000000614     C     VSCSNP        IFEQ      'P'
098100020220     c     §ogntw        andne     'PPT'
098200000614     C                   SETON                                            90
098300000614     C                   GOTO      ENDCON
098400000614     C                   ENDIF
098500910207     C*
098600910207     C     ENDCON        ENDSR
098700971217     C**
098800020215      *---------------------------------------------------------------
098900971217     C* STAMPA ETICHETTA ESTERA SDI O BARTOLINI ----------------------*
099000020215      *---------------------------------------------------------------
099100971217     C     ETIEST        BEGSR
099200971217     C*
099400081007     c     §detse        ifeq      'EEX'
099500971219     C* IMPOSTO DSBLD
099600081007     C                   MOVEL     'ITA'         §DETST
099700971219     C**
099800971217     C* CHAIN SULLE NAZIONI
099900971217     C                   MOVEL     '15'          COD
100000971217     C                   MOVEL(P)  BLPNZD        KEY
100100971219     C     KTAB          CHAIN     TABEL                              30
100200971217     C  N30TBLFLG        IFNE      ' '
100300971217     C                   SETON                                        30
100400971217     C                   END
100500971217     C**
100600971217     C  N30              MOVEL     TBLUNI        DS15
100700971217     C   30              CLEAR                   DS15
100800971217     C                   MOVEL     §15COD        §DEISA
103100971217     C**
103200971217     C                   MOVEL     '3A'          COD
103300971217     C                   MOVEL(P)  BLPCBO        KEY
103400971219     C     KTAB          CHAIN     TABEL                              30
103500971217     C  N30TBLFLG        IFNE      ' '
103600971217     C                   SETON                                        30
103700971217     C                   END
103800971217     C**
103900971217     C                   MOVEL     TBLUNI        DS3A
104000971217     C                   MOVEL     §3ATB1        FLGTB1            1
104100971217     C**
104200971217    3C                   SELECT
104300971217     C     FLGTB1        WHENEQ    'A'
104400971217     C     §3AFCA        ANDEQ     0
104500971217     C                   MOVEL     'A'           §DEFCA
104600971217     C     FLGTB1        WHENEQ    'A'
104700971217     C     §3AFCA        ANDNE     0
104800971217     C                   MOVEL     'X'           §DEFCA
104900971217     C     §3AFCA        WHENNE    0
105000971217     C                   MOVEL     'S'           §DEFCA
105100971217    3C                   ENDSL
105300971219     C* ETICHETTA ITALIA
105400971219   X1C                   ELSE
105500971219     C                   MOVEL     'ITA'         §DETST
105600971217    1C                   ENDIF
105700971217     C**
105800971217     C                   ENDSR
105900020215      *----------------------------------------------------------------
106000020215      *--- IMPOSTO CAMPI SEGNACOLLO DA PASSARE A PGM DI STAMPA     ---*
106100020215      *----------------------------------------------------------------
106200020215     C     EUREXP        BEGSR
106300020215
106400020215     C                   MOVEL     'E'           WTRC
106500051012     C     KarS01        CHAIN     FIarS01L                           30
106600980402    2C     *IN30         IFEQ      *OFF
106700980402     C* PASSO AL PGM DI STAMPA ETICHETTE IL N. SEGNACOLLO EUROEXPRESS
106800051012     C                   MOVEL     arSNOT        §DEEUR
106900980402     C* SE IMPORT BARTOLINI METTO NEL CODICE PRODOTTO DELL'ETICHETTA
107000980402     C* ITALIA IL N.SEGNACOLLO EUROEXPRESS
107100020215   3 c                   if        lnpntw = 'EEX'
107200051012     C                   MOVEA     arSNOT        W35
107300980402     C                   Z-ADD     1             BB                2 0
107400980402     C     ' '           LOOKUP    W35(BB)                                30
107500980402     C   30              MOVEA     *BLANKS       W35(BB)
107600980402     C                   MOVEA     W35(6)        §LDCDP
107700980402    3C                   ENDIF
107800980402    2C                   ENDIF
107900980402     C                   ENDSR
108000020215
108100020215      *----------------------------------------------------------------
108200020215      *--- IMPOSTO CAMPI SEGNACOLLO DA PASSARE A PGM DI STAMPA     ---*
108300020215      *---------------------------------------------------------------*
108400020215     c     sr_fedex      begsr
108500020215
108600020215     c                   eval      wtrc = 'F'
108700051012     c     kars01        chain     fIars01l
108800051012     c                   if        %found(fIars01l) and arsnot <> *blanks
108900051012     c                   movel     arsnot        §ldcdp
109000020215     c                   endif
109100020215
109200020215     c                   endsr
109300050418      *---------------------------------------------------------------*
109400050418      *  Verifica se spedizione "DI VALORE"                           *
109500050418      *---------------------------------------------------------------*
109600050418     c     CtrSpeVal     BEGSR
109700050418      *
109800050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
109900050418if  1c                   if            BLPaas = AR5aas
110000050418     c                             and BLPlnp = AR5lnp
110100050418     c                             and BLPnrs = AR5nrs
110200050418     c                             and BLPnsp = AR5nsp
110300050418     c                   leavesr
110400050418e   1c                   endif
110500050418      *
110600050418      * Reperisce i dati da FIAR5
110700050418     c                   clear                   dAR5gen
110800050418     c     K05AR501      setgt     FIAR5000
110900050418do  1c                   dou            %eof(FIAR501L)
111000050418     c                             or   AR5atb =  *blanks
111100050418     c     K05AR501      readpe    FIAR5000
111200050418e   1c                   enddo
111300050418if  1c                   if             NOT %eof(FIAR501L)
111400050418     c                             and  AR5atb =  *blanks
111500050418     c                   movel     AR5uni        dAR5gen
111600050418e   1c                   endif
111700050418      *
111800050418     c                   ENDSR
111900020215
112000941228     OFNLV22P   T    LRNLR
