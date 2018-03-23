000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS76R *----------------------------------------------------*
000300910207     H*         - RISTAMPA SEGNACOLLI           BATCH
000400000000     H*--------------------------------------------------------------*
000500171011     F*Fiar401L  IF   E           K DISK
000600171010     FFipnd01L  IF   E           K DISK
000700910207     FTABEL00F  IF   E           K DISK
000800910207     FAZORG01L  IF   E           K DISK
000900010717     F*
001000010717     FFNBLP31L  IF   E           K DISK
001100020702     ffnblp76l  if   e           k disk    rename(fnblp000:fnblp076)
001200941228     FFNBLP01L  IF   E           K DISK
001300941228     F                                     RENAME(FNBLP000:FNBLP001)
001400941228     FFNBLT01L  IF   E           K DISK
001500051012     FFIARS01L  IF   E           K DISK
001600941228     FFNBLD01L  IF   E           K DISK
001700050418     fFIAR501L  if   e           k disk
001800941228     FFNLV22P   O    F  132        PRINTER
001900020702
002000020702     d xx              s              2  0
002100171011     d*w005a           s              5
002200020702     d comlnp          s                   like(v1clnp)
002300020702
002400980402     D W35             S              1    DIM(35)
002500171011     D*W15             S              1    DIM(15)
002600980402     D*
002700900523     D PARAM           DS
002800941228     D  V1CLNP                 1      3  0
002900941228     D  V1CLNA                 4      6  0
003000941228     D  DSD                    7     14  0
003100941228     D  WDSA                  15     22  0
003200941228     D*
003300941228     D  V1CLPD                23     25  0
003400941228     D  V1CSED                26     27  0
003500941228     D  V1CNSD                28     34  0
003600941228     D*
003700941228     D  V1CSEA                35     36  0
003800941228     D  V1CNSA                37     43  0
003900941228     D  V1CSPE                35     43  0
004000941228     D  V1CAAS                44     47  0
004100941228     D*
004200941228     D  VSCSOP                48     48
004300941228     D  VSCLNA                49     51  0
004400941228     D  VSCFLS                52     54  0
004500941228     D  VSCSCD                55     61  0
004600941228     D  VSCSCA                62     68  0
004700941228     D  VSCKSC                69     75  0
004800950116     D*
004900941228     D  VSCCPR                76     82  0
005000950116     D  VSCFPP                83     83
005100950116     D  VSCNRT                84     84
005200970701     D  VSCBAM                85     85
005300000614     D  VSCSNP                86     86
005400080220     d* · num. limite di LdV stampabili (prima FISSO 6000)
005500080220     d  wVPOmaxLdV           151    155s 0 inz
005600950420     D* PARFLG = 'E' --> STAMPATE PIU' DI 6000 PAGINE
005700030613     D  PARFLG               156    156
005800941228     D* PGM DI STAMPA ETICHETTE
005900941228     D  PSS                  207    216
006000910207     D                 DS
006100941228     D  BLPAAS                 1      4  0
006200941228     D  BLPMGS                 5      8  0
006300941228     D  BLPDSP                 1      8  0
006400910903     D                 DS
006500910903     D  BLPNRS                 1      2  0
006600910903     D  BLPNSP                 3      9  0
006700910903     D  BLPSPE                 1      9  0
006800991201     D                 DS
006900991201     D  DSRMN                  1     15
007000991201     D  DSRMN1                 4      6
007100991202     D  DSRMN2                 7     14
007200991202     D  DSRMN3                15     15
007300171011     D*                DS
007400171011     D* DPDSTP                 1     14
007500171011     D* DPDS1                  1      3
007600171011     D* DPDV1                  4      4    INZ
007700171011     D* DPDS2                  5     12
007800171011     D* DPDV2                 13     13    INZ
007900171011     D* DPDS3                 14     14
008000171011     D*                DS
008100171011     D* DPDPAR                 1     12
008200171011     D* DPD1                   1      3
008300171011     D* DPD2                   4     11
008400171011     D* DPD3                  12     12
008500910207     D DS1B          E DS
008600971219     D DS15          E DS
008700971219     D DS3A          E DS
008800910207     D DSBLD         E DS
008900971219     D DSBLDE        E DS
009000060523     D*DSBL4F        E DS
009100171011     D*DSBL4I        E DS
009200171011     D*DSBL4K        E DS
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
012600020215
012700000000     C*---------------------------------------------------------------*
012800000000     C     *ENTRY        PLIST
012900000000     C                   PARM                    KPJBA
013000921209     C                   Z-ADD     1             CODUT             1 0
013100900511     C*---------------------------------------------------------------*
013200910207     C* ACCESSO   FLBLP                                    *
013300910207     C     KBLP          KLIST
013400941228     C                   KFLD                    V1CLNP
013500910207     C                   KFLD                    AAS
013600910207     C                   KFLD                    MGS
013700941228     C     KBLP2         KLIST
013800020702     c                   kfld                    comlnp
013900950331     C                   KFLD                    COMLNA
014000941228     C                   KFLD                    AAS
014100941228     C                   KFLD                    MGS
014200910903     C     KBLP4         KLIST
014300941228     C                   KFLD                    V1CAAS
014400941228     C                   KFLD                    V1CLPD
014500941228     C                   KFLD                    V1CSED
014600941228     C                   KFLD                    V1CNSD
014700910903     C     KBLP5         KLIST
014800941228     C                   KFLD                    V1CAAS
014900941228     C                   KFLD                    V1CLPD
015000020702     c     kblp6         klist
015100020702     c                   kfld                    comlnp
015200020702     c                   kfld                    comlna
015300020702
015400910207     C     KBLD          KLIST
015500910207     C                   KFLD                    BLPAAS
015600910207     C                   KFLD                    BLPLNP
015700910207     C                   KFLD                    BLPNRS
015800910207     C                   KFLD                    BLPNSP
015900171011     C*    Kar4          KLIST
016000171011     C*                  KFLD                    BLPAAS
016100171011     C*                  KFLD                    BLPLNP
016200171011     C*                  KFLD                    BLPNRS
016300171011     C*                  KFLD                    BLPNSP
016400171011     C*                  KFLD                    WTRC
016500910207     C* ACCESSO   TABEL                                    *
016600910207     C     KTAB          KLIST
016700910207     C                   KFLD                    CODUT
016800910207     C                   KFLD                    COD               2
016900910207     C                   KFLD                    KEY               8
017000980402     C*
017100051012     C     KarS01        KLIST
017200980402     C                   KFLD                    BLPFLS
017300980402     C                   KFLD                    BLPLNA
017400980402     C                   KFLD                    BLPNRS
017500980402     C                   KFLD                    §LDNCD
017600980402     C                   KFLD                    WTRC
017700050418      * FIAR501L
017800050418     c     k05AR501      klist
017900050418     c                   kfld                    BLPaas
018000050418     c                   kfld                    BLPlnp
018100050418     c                   kfld                    BLPnrs
018200050418     c                   kfld                    BLPnsp
018300050418     c                   kfld                    AR5trd
018400050418     c                   eval      AR5trd = 'GEN'
018500950331     C*
018600950331     C     *LIKE         DEFINE    BLPLNA        COMLNA
018700171011     C     *LIKE         DEFINE    arsTRC        WTRC
018800910205     C*---------------------------------------------------------------*
018900910205     C                   MOVEL     KPJBU         PARAM
019000020702
019100020702      * reperisco i dati utente
019200020702     c     *dtaara       define    §azute        AzuteDs
019300020702     c     *dtaara       define    §datiute      DDatiUte
019400020702
019500020702     c                   in(E)     *dtaara
019600020702     c                   IF        %Error or RSUT = *blanks
019700020702     c                   call      'TIBS34R'
019800020702     c                   parm                    Tibs34Ds
019900020702     c                   in        *dtaara
020000020702     c                   ENDIF
020100020702
020200971217     C*
020300971217     C                   CLEAR                   DSBLD
020400971217     C**
020500910207     C* VEDO LE PARZIALIZZAZIONI EFFETTUATE
020600910207     C                   EXSR      PREPA
020700950331     C*
020800020702      * stampa per lna imposto la prima linea di partenza
020900020702     c                   if        *in11
021000020702     c                   z-add     1             xx
021100020702     c                   movel     lin(xx)       comlnp
021200020702     c                   endif
021300020702      * stampa per lna imposto la chiave se non richieste tutte
021400950331     C     *IN11         IFEQ      *ON
021500950331     C     V1CLNA        ANDEQ     999
021600950331     C* Scelte tutte le linee di arrivo: posizionamento sulla prima
021700020702     c     comlnp        setll     fnblp76l
021800020702     c                   read      fnblp76l                               31
021900950331     C  N31              Z-ADD     BLPLNA        COMLNA
022000950331     C                   ELSE
022100950331     C                   Z-ADD     V1CLNA        COMLNA
022200950331     C                   END
022300020702      * stampa per lnp e richiesta nelle parzializzazioni la lna imposto
022400020702      * la chiave
022500020702     c                   if        *in01 and *in06
022600020702     c                   movel     v1clnp        comlnp
022700020702     c                   movel     vsclna        comlna
022800020702     c                   endif
022900950331     C*
023000950331     C     *IN31         DOWEQ     *OFF
023100941228     C***
023200941228     C* PER NUMERO SPEDIZIONE
023300941228     C   12KBLP4         SETLL     FNBLP01L
023400020702
023500941228     C* PER LNP E  DATA   SPEDIZIONE
023600020702      * no lna nelle selezioni
023700020702     c  n06
023800020702     Can 01KBLP          SETLL     FNBLP31L
023900020702      * con lna nelle selezioni
024000020702     c   06
024100020702     can 01kblp2         setll     fnblp76l
024200020702
024300941228     C* PER LNA E  DATA   SPEDIZIONE
024400020702     c   11kblp2         setll     fnblp76l
024500941228     C*
024600941228     C     *IN31         DOWEQ     *OFF
024700020702
024800941228     C* PER NUMERO SPEDIZIONE
024900941228     C   12
025000941228     CANN05KBLP4         READE     FNBLP01L                               31
025100941228     C   12
025200941228     CAN 05KBLP5         READE     FNBLP01L                               31
025300020702
025400941228     C* PER LNP E  DATA   SPEDIZIONE
025500020702      * no lna nelle selezioni
025600020702     c                   if        not *in06
025700010717     C   01
025800010717     CANN02KBLP          READE     FNBLP31L                               31
025900010717     C   01
026000010717     CAN 02V1CLNP        READE     FNBLP31L                               31
026100020702     c                   else
026200020702      * con lna nelle selezioni
026300020702     c   01
026400020702     cann02kblp2         reade     fnblp76l                               31
026500020702     c   01
026600020702     can 02kblp6         reade     fnblp76l                               31
026700020702     c                   endif
026800020702
026900941228     C* PER LNA E  DATA   SPEDIZIONE
027000020702     c   11
027100020702     cann02kblp2         reade     fnblp76l                               31
027200020702     c   11
027300020702     can 02kblp6         reade     fnblp76l                               31
027400020702
027500910207     C***
027600910903     C*
027700910903     C* CON NUMERO AL VEDO SE COMPRESA
027800941228     C   12
027900941228     CAN 05
028000941228     CANN31BLPSPE        IFGT      V1CSPE
028100910903     C                   SETON                                        31
028200910903     C                   END
028300910903     C*
028400910903     C* CON DATA   AL VEDO SE COMPRESA
028500941228     C  N12
028600941228     CAN 02
028700941228     CANN31BLPDSP        IFGT      WDSA
028800910903     C                   SETON                                        31
028900910903     C                   END
029000910903     C  N31              EXSR      CONFR
029100910207     C*
029200910903     C  N31
029300910903     CANN90              EXSR      CONTR
029400950419     C*
029500950420     C* SE SUPERATE LE 6000 PAGINE (PARFLG='E'), VADO A FINE PGM
029600950419     C     PARFLG        IFEQ      'E'
029700950419     C                   GOTO      FINE
029800950419     C                   ENDIF
029900910207     C*
030000910205     C                   END
030100950331     C*
030200020702      * Richiesta di tutte le linee di arrivo
030300950331     C     *IN11         IFEQ      *ON
030400950331     C     V1CLNA        ANDEQ     999
030500020702      * cerco linea di arrivo sucessiva
030600020702     c     kblp6         setgt     fnblp76l
030700020702     c                   read      fnblp76l                               31
030800020702     c                   if        blplnp = comlnp
030900020702     c                   z-add     blplna        comlna
031000020702      * cerco la linea di partenza sucessiva
031100020702     c                   else
031200020702     c                   add       1             xx
031300020702      * non ci sono più linee di partenza vado a fine
031400020702     c                   if        lin(xx) = *zeros
031500020702     c                   goto      fine
031600020702     c                   endif
031700020702      * cerco la prima linea di arrivo della linea di partenza che
031800020702      * devo leggere
031900020702     c                   eval      comlnp = lin(xx)
032000020702     c     comlnp        setll     fnblp76l
032100020702     c                   read      fnblp76l                               31
032200020702     c  n31              z-add     blplna        comlna
032300020702     c                   endif
032400950331     C                   END
032500020702
032600020702      * richiesta solo una linea di arrivo
032700020702     c                   if        *in11 and v1clna <> 999
032800020702     c                   eval      *in31 = *off
032900020702      * cerco la linea di partenza sucessiva
033000020702     c                   add       1             xx
033100020702     c                   if        lin(xx) = *zeros
033200020702      * non ci sono più linee di partenza vado a fine
033300020702     c                   goto      fine
033400020702     c                   endif
033500020702     c                   eval      comlnp = lin(xx)
033600020702     c                   endif
033700950331     C*
033800950331     C                   ENDDO
033900921209     C*
034000950419     C     FINE          TAG
034100020702     C                   MOVEL(p)  PARAM         KPJBU
034200000000     C                   SETON                                        LR
034300910207     C*
034400910219     C*--- PREPARO VEDO LE PARZIALIZZAZIONE FATTE -------------------*
034500910207     C     PREPA         BEGSR
034600941228     C**
034700941228     C* CONTROLLO COME DEVO LEGGERE
034800941228     C**
034900941228     C* PER LINEA DI PARTENZA
035000941228     C     V1CLNP        COMP      0                                  01
035100941228     C* PER LINEA DI ARRIVO
035200941228     C     V1CLNA        COMP      0                                  11
035300941228     C* PER NUMERO SPEDIZIONE
035400941228     C     V1CLPD        COMP      0                                  12
035500941228     C**
035600950331     C* CONTROLLO LE PARZIALIZZAZIONI MESSE
035700941228     C**
035800941228     C* NUMERO SPEDIZIONE AL
035900941228     C     V1CNSA        COMP      0                                  05
036000941228     C* DATA AL
036100941228     C     WDSA          COMP      0                                  02
036200941228     C* SIGLA OPERATORE
036300941228     C     VSCSOP        COMP      *BLANKS                            0303
036400941228     C* LINEA DI ARRIVO
036500941228     C     VSCLNA        COMP      0                                  06
036600941228     C* SEGNACOLLO AL
036700941228     C     VSCSCA        COMP      0                                  08
036800941228     C* SEGNACOLLO DAL
036900941228     C  N08VSCSCD        COMP      0                                  07
037000941228     C* FILIALE SEGNACOLLO
037100941228     C     VSCFLS        COMP      0                                  17
037200941228     C* CODICE CLIENTE MITTENTE
037300941228     C     VSCKSC        COMP      0                                  09
037400941228     C* CODICE PADRONCINO
037500941228     C     VSCCPR        COMP      0                                  10
037600910207     C*
037700941228     C                   MOVEL     DSD           AAS               4 0
037800910207     C                   MOVE      DSD           MGS               4 0
037900910903     C                   SETOFF                                       31
038000950418     C*
038100950418     C* AZZERO IL NUMERATORE PAGINE
038200950418     C                   Z-ADD     *ZERO         WPAGIN            5 0
038300020702      * stampa per lna
038400020702     c                   if        *in11
038500020702      * se p.o. di secondo livello imposto nella sk delle lnp se
038600020702      * stesso
038700020702     c                   clear                   lin
038800020702     c                   if        dutlpo = '2' or dutlpo = *blanks
038900020702     c                   eval      lin(1) = dutpou
039000020702      * se p.o. di primo livello carico tutti i p.o. del terminal di partenza
039100020702     c                   else
039200020702     c                   clear                   Trul06Ds
039300020702     c                   eval      D06Cod = '£1'
039400020702     c                   eval      D06Tla = 'L'
039500020702     c                   movel     Simfel        D06Key
039600020702     c                   movel(p)  Trul06Ds      Kpjbu
039700020702     c                   call      'TRUL06R'
039800020702     c                   parm                    Kpjba
039900020702     c                   movel     Kpjbu         Trul06Ds
040000020702     c                   endif
040100020702     c                   endif
040200020702
040300080220      * Aggancio tabella "VPO" per reperimento il n° LdV stampabili
040400080220      *   limite (per congelamento e blocco stampa)
040500080220     c                   reset                   dVPO
040600080220     c                   clear                   TIBS02ds
040700080220     c                   eval      T02tla  = 'L'
040800080220     c                   eval      T02mod  = 'C'
040900080220     c                   eval      T02sif  = KNSIF
041000080220     c                   eval      T02cod  = 'VPO'
041100080220     c                   movel(P)  'VPO'         T02ke1
041200080220     c                   call      'TIBS02R'
041300080220     c                   parm                    KPJBA
041400080220     c                   parm                    TIBS02ds
041500080220     c                   if        T02err  = *blanks
041600080220     c                   movel     T02uni        dVPO
041700080220     c                   endif
041800080220     c                   eval      wVPOmaxLdV = §VPOmaxLdV
041900080220      *
042000910207     C                   ENDSR
042100910207     C*
042200910207     C*--- CONTROLLO SE POSSO STAMPARE SEGNACOLLO -------------------*
042300910207     C     CONTR         BEGSR
042400000201     C                   CLEAR                   DSBLDE
042500171012     C***                CLEAR                   WPRMA
042600910207     C* PRENDO CODICE TRATTAMENTO MERCE
042700971219     C                   MOVEL(P)  BLPCTM        KEY
042800910207     C                   MOVEL     '1B'          COD
042900910207     C     KTAB          CHAIN     TABEL                              30
043000910207     C  N30              MOVEL     TBLUNI        DS1B
043100971219     C   30              CLEAR                   DS1B
043200971219     C**
043300971219     C* CHAIN CON LINEA DI PARTENZA
043400971219     C     BLPLNP        CHAIN     AZORG                              30
043500991201     C                   MOVEL     ORGDE3        OG143
043600020214     c                   eval      lnpntw = §ogntw
043700971219     C**
043800971219     C* IMPOSTO LA LINEA DI ARRIVO
043900971219     C                   MOVEL     BLPLNA        §LDLNA
044000971219     C     BLPLNA        CHAIN     AZORG                              30
044100971219     C  N30ORGDE5        IFNE      *BLANKS
044200971219     C                   MOVEL     ORGDE5        §LDDLA
044300971219     C                   ELSE
044400971219     C                   MOVEL     ORGDES        §LDDLA
044500971219     C                   END
044600971219     C   30              MOVEL     *BLANKS       §LDDLA
044700020214     c                   eval      a_og143 = orgde3
044800020214     c                   eval      lnantw = a_§ogntw
044900171011     c**                 movel     orgtel        dpdtel           15
045000171011     c**                 movel     orgfax        dpdfax           15
045100040525     c                   movel     orgdf0        og150
045200991206      *
045300991206      * RIFERIMENTO MITTENTE ALFABETICO, SE LNP DPD SCRIVO NUMERICO
045400171011     c***                if        lnpntw <> 'DPD'
045500971219     C                   MOVEL     BLPRMA        §LDCDP
045600060523     C***                ELSE
045700060523     C***                MOVEL     BLPRMN        DSRMN
045800060523     C***                CLEAR                   W15
045900060523     C***                MOVEA     DSRMN1        W15(1)
046000060523     C***                MOVEA     DSRMN2        W15(5)
046100060523     C***                MOVEA     DSRMN3        W15(14)
046200060523     C***                MOVEA     W15           §LDCDP
046300171011    1C***                ENDIF
046400991206      **
046500171020      **  CHAIN dato DPD import e export
046600171020      *  nel codice prodotto, per etichetta italia, sempre il parcel number
046700060523     c                   if        lnantw = 'DPD' or lnpntw='DPD'
046800171010        chain (blpaas:blplnp:blpnrs:blpnsp) fipnd01l ;
046900171010        if %found(fipnd01l)     ;
047000171011     C                   MOVEL     pndipn        §ldcdp
047100171020         else;
047200171020         clear pndipn ;
047300171010        endif;
047400171010
047500171010     C**                 MOVEL     'I'           WTRC
047600171010     C**   Kar4          CHAIN     Fiar401L                           39
047700171010     C**N39              MOVEL     ar4NOT        DSBL4I
047800171010     C** 39              CLEAR                   DSBL4I
047900171010    2C**   §B4ipn        IFNE      *BLANKS
048000171010     C**                 MOVEL     §B4ipn        DPDPAR
048100171010     C**                 MOVEL     DPD1          DPDS1
048200171010     C**                 MOVEL     DPD2          DPDS2
048300171010     C**                 MOVEL     §b4icd        DPDS3
048400171010     C**                 MOVEL(P)  DPDSTP        §LDCDP
048500171010     c**                 endif
048600060523     c                   endif
048700060523     c*
048800171010     c****               if        lnantw = 'DPD' and §b4ipn <> *blanks
048900060523     C* SE RMA DIVERSO DA BLANK MEMORIZZO CHE DEVO STAMPARLO PRIMA
049000060523     C* DELLA RAGIONE SOCIALE MITENTE
049100171012     C**   BLPRMA        IFNE      *BLANKS
049200171012     C**                 MOVE      '1'           WPRMA             1
049300171012     C**                 ENDIF
049400171011     C***  BLPPKF        IFLE      3
049500171011     C***                MOVEL     'X'           §DESSE
049600171011     C***                ENDIF
049700060523     c*
049800171011     C**                 MOVEL     'K'           WTRC
049900171011     C**   Kar4          CHAIN     Fiar401L                           39
050000171011     C**N39              MOVEL     ar4NOT        DSBL4K
050100171011     C** 39              CLEAR                   DSBL4K
050200000201     C*
050300171011     C****               MOVEL     blprsd        §DESIA
050400171011     C****               MOVEL     §B4krt        §DEAWB
050500171010     C****               MOVEL     §B4ipn        §DEEUR
050600171010     c****               eval      %subst(§deeur:12:1) = §b4icd
050700171011     C****               MOVEL     DPDTEL        §DEXX2
050800171011     C****               MOVE      DPDFAX        §DEXX2
050900171011     c****               eval      w005a = %subst(§b4ida:2:3) + §b4iza
051000171011     C****               MOVE      W005A         §DEXX1
051100171012    2C***                ENDIF
051200991206      **
051300000201     C** IMPOSTO CAMPI FISSI SERVONO SOLO PER EXPORT
051400020214
051500000207     C** IMPOSTO EUR IN TS PER EUROEXPRESS
051600020214
051700020214      * Se arrivo EuroExpress
051800040507      * imposta EEX
051900020214     c                   if        lnantw = 'EEX'
052000040507     C                   MOVEL     'EEX'         §DETSE
052100000207     C                   ENDIF
052200921209     C*
052300910207     C* FLAG6 DI STAMPA BOLLA
052400950419    1C     §1BFG6        IFNE      'D'
052500971219     C     §1BFG9        ANDNE     'S'
052600971219     C**
052700971219    2C     BLPNCD        IFGT      0
052800910207     C                   EXSR      STASCI
052900921209     C*
053000971219   X2C                   ELSE
053100921209     C*
053200921209     C* STAMPO ANCHE SE NON SONO SEQUENZIALI
053300971219    3C     BLPFNS        IFEQ      'S'
053400921209     C                   EXSR      STASCN
053500971219    3C                   ENDIF
053600971219    2C                   ENDIF
053700921209     C*
053800950419   X1C                   ELSE
053900910207     C*
054000941228     C     KBLD          SETLL     FNBLD000
054100921209     C                   SETOFF                                       32
054200921209     C*
054300971219    2C     *IN32         DOWEQ     *OFF
054400941228     C     KBLD          READE     FNBLD000                               32
054500971219    3C  N32BLDFST        IFEQ      'S'
054600971219    3C     BLDFST        OREQ      'Z'
054700910207     C                   EXSR      STASCD
054800950420     C* SE SUPERATE LE 6000 PAGINE (PARFLG='E'), INTERROMPO IL CICLO
054900971219    3C     PARFLG        IFEQ      'E'
055000950419     C                   SETON                                        32
055100971219    3C                   ENDIF
055200971219    3C                   ENDIF
055300971219    2C                   ENDDO
055400910207     C*
055500971219    1C                   ENDIF
055600910207     C*
055700910207     C                   ENDSR
055800910207     C*
055900921209     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI SEQUENZIALI ---------*
056000910207     C     STASCI        BEGSR
056100910207     C                   Z-ADD     BLPNCD        COMSCD            7 0
056200910207     C     BLPNCA        IFEQ      0
056300910207     C                   Z-ADD     BLPNCD        BLPNCA
056400910207     C                   END
056500921209     C* IMPOSTO CAMPI DI STAMPA
056600921209     C                   EXSR      IMPO
056700910208     C*
056800910208     C     COMSCD        DOWLE     BLPNCA
056900990318     C     COMSCD        ANDGT     0
057000910208     C                   ADD       1             §LDNC1
057100910208     C                   SETOFF                                       21
057200910208     C* SE SELEZIONE SEGNACOLLI CONTROLLO CHE SIA NEL RANGE
057300910403     C   07
057400941228     COR 08COMSCD        IFLT      VSCSCD
057500910208     C                   SETON                                        21
057600910208     C                   END
057700910403     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      DAL / AL
057800941228     C   08
057900941228     CANN21COMSCD        IFGT      VSCSCA
058000910208     C                   Z-ADD     BLPNCA        COMSCD
058100910208     C                   SETON                                        21
058200910208     C                   END
058300910403     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      SOLO DAL
058400941228     C   07
058500941228     CANN21COMSCD        IFGT      VSCSCD
058600910403     C                   Z-ADD     BLPNCA        COMSCD
058700910403     C                   SETON                                        21
058800910403     C                   END
058900910208     C* S T A M P A
059000910208     C  N21              MOVEL     COMSCD        §LDNCD
059100020215
059200020215     c                   if        not *in21
059300020215     c                   select
059400020215      * per segnacolli FedEx imposta §LDCDP
059500020215     c                   when      lnantw = 'FED'
059600020215     c                   exsr      sr_fedex
059700020215      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
059800040507     c                   when      lnpntw = 'EEX'
059900020215     C                   EXSR      EUREXP
060000020215     c                   endsl
060100020215     c                   endif
060200020215
060300910208     C*
060400941228     C  N21              CALL      PSS
060500910207     C                   PARM                    DSBLD
060600971219     C                   PARM                    DSBLDE
060700910207     C*
060800910207     C                   ADD       1             COMSCD
060900950419     C*
061000950419     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
061100950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
061200041012     C  n21              ADD       1             WPAGIN
061300080220     c                   IF        NOT  *in21
061400080220     c                             and  wPagin > §VPOmaxLdV
061500950419     C                   MOVEL     'E'           PARFLG
061600950419     C* IMPOSTO 9999999 IN COMSCD PER INTERROMPERE IL CICLO
061700950419     C                   Z-ADD     *ALL'9'       COMSCD
061800950419     C                   ENDIF
061900950419     C*
062000910207     C                   END
062100910208     C*
062200910207     C                   ENDSR
062300921209     C*
062400921209     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI NON SEQUENZIALI -----*
062500921209     C     STASCN        BEGSR
062600921209     C*
062700921209     C                   EXSR      IMPO
062800921209     C*
062900941228     C     KBLD          SETLL     FNBLT01L
063000941228     C     KBLD          READE     FNBLT01L                               33
063100921209     C*
063200941228     C     *IN33         DOWEQ     *OFF
063300921209     C*
063400941228     C                   Z-ADD     BLTNSC        COMSCD
063500921209     C                   ADD       1             §LDNC1
063600921209     C                   SETOFF                                       21
063700921209     C* SE SELEZIONE SEGNACOLLI CONTROLLO CHE SIA NEL RANGE
063800921209     C   07
063900941228     COR 08COMSCD        IFLT      VSCSCD
064000921209     C                   SETON                                        21
064100921209     C                   END
064200921209     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      DAL / AL
064300941228     C   08
064400941228     CANN21COMSCD        IFGT      VSCSCA
064500921209     C                   SETON                                        2133
064600921209     C                   END
064700921209     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      SOLO DAL
064800941228     C   07
064900941228     CANN21COMSCD        IFGT      VSCSCD
065000921209     C                   SETON                                        2133
065100921209     C                   END
065200921209     C* S T A M P A
065300921209     C  N21              MOVEL     COMSCD        §LDNCD
065400020215
065500020215     c                   if        not *in21
065600020215     c                   select
065700020215      * per segnacolli FedEx imposta §LDCDP
065800020215     c                   when      lnantw = 'FED'
065900020215     c                   exsr      sr_fedex
066000020215      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
066100040507     c                   when      lnpntw = 'EEX'
066200020215     C                   EXSR      EUREXP
066300020215     c                   endsl
066400020215     c                   endif
066500020215
066600921209     C*
066700941228     C  N21              CALL      PSS
066800921209     C                   PARM                    DSBLD
066900971219     C                   PARM                    DSBLDE
067000950418     C*
067100950418     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
067200950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
067300041012     C  n21              ADD       1             WPAGIN
067400080220     c                   IF        NOT  *in21
067500080220     c                             and  wPagin > §VPOmaxLdV
067600950419     C                   MOVEL     'E'           PARFLG
067700950419     C* ACCENDO *IN33  PER INTERROMPERE IL CICLO
067800950419     C                   SETON                                        33
067900950419     C                   ENDIF
068000921209     C*
068100941228     C  N33KBLD          READE     FNBLT01L                               33
068200921209     C                   END
068300921209     C*
068400921209     C                   ENDSR
068500921209     C*
068600921209     C* IMPOSTO CAMPI PER STAMPA IN IMMEDIATO ------------------------*
068700921209     C     IMPO          BEGSR
068800050418      *
068900050418      * Verifica se spedizione "di valore"
069000050418     c                   exsr      CtrSpeVal
069100921209     C*
069200921209     C                   Z-ADD     0             §LDNC1
069300921209     C                   MOVEL     BLPFLS        §LDLNP
069400921209     C     BLPFLS        CHAIN     AZORG                              30
069500921209     C  N30              MOVEL     ORGDES        §LDDLP
069600921209     C   30              MOVEL     *BLANKS       §LDDLP
069700921209     C                   MOVEL     BLPNRS        §LDNRS
069800921209     C                   MOVEL     BLPTFA        §LDTNA
069900921209     C                   MOVEL     BLPTSP        §LDTSP
070000921209     C                   MOVEL     BLPZNC        §LDZSC
070100921209     C                   MOVE      BLPNCL        §LDNC2
070200010302     C* SE DEVO STAMPARE ETICHETTA EXPORT DPD PRIMA DELLA RAGIONE SOC.
070300010302     C* MITTENTE DEVO STAMPARE IL RMA SENZA I BLANK NON SIGNIFICATIVI
070400171012     C**   WPRMA         IFEQ      '1'
070500171012     C**                 CLEAR                   W35
070600171012     C**                 MOVEA     BLPRMA        W35
070700171012     C**   ' '           CHECKR    BLPRMA        §Z                2 0
070800171012     C**                 ADD       2             §Z
070900171012     C**                 MOVEA     BLPRSM        W35(§Z)
071000171012     C**                 MOVEA     W35           §LDRSM
071100171012     C**                 ELSE
071200921209     C                   MOVEL     BLPRSM        §LDRSM
071300171012     C**                 ENDIF
071400921209     C                   MOVEL     BLPPRM        §LDPRM
071500921209     C                   MOVEL     BLPRSD        §LDRSD
071600921209     C                   MOVEL     BLPCAD        §LDCAD
071700921209     C                   MOVEL     BLPLOD        §LDLOD
071800921209     C                   MOVEL     BLPIND        §LDIND
071900921209     C                   MOVEL     BLPPRD        §LDPRD
072000941228     C                   MOVE      BLPPDR        §LDCPD
072100921209     C                   MOVEL     *ZEROS        §LDNCP
072200921209     C                   MOVEL     *ZEROS        §LDNCU
072300050418if  1c                   if        §AR5bva = 'V'
072400050418     c                   movel     'X'           §LDrst
072500050418x   1c                   else
072600921209     C                   MOVEL     'R'           §LDRST
072700050418e   1c                   endif
072800941228     C                   Z-ADD     BLPPKF        §LDPKG
072900941228     C                   Z-ADD     BLPVLF        §LDVOL
073000941228     C                   MOVEL     BLPFVF        §LDFVR
073100950116     C                   MOVEL     BLPAAS        §LDAAS
073200950116     C                   MOVEL     BLPMGS        §LDMGS
073300971219     C*
073400971219     C                   EXSR      ETIEST
073500980402     C*
073600921209     C                   ENDSR
073700921209     C*
073800910207     C*--- STAMPA BOLLA IN IMMEDIATO  COME RISTAMPA DEL "D" ----------*
073900910207     C     STASCD        BEGSR
074000910207     C* RICHIAMO DIRETTAMENTE IL PROGRAMMA DI STAMPA SEGNACOLLO
074100910207     C                   Z-ADD     BLDNCD        COMSCD
074200910207     C     BLDNCA        IFEQ      0
074300910207     C                   Z-ADD     BLDNCD        BLDNCA
074400910207     C                   END
074500050418      *
074600050418      * Verifica se spedizione "di valore"
074700050418     c                   exsr      CtrSpeVal
074800910207     C*
074900910219     C                   Z-ADD     BLDNC1        §LDNC1
075000910207     C                   MOVEL     BLPFLS        §LDLNP
075100910207     C     BLPFLS        CHAIN     AZORG                              30
075200910207     C  N30              MOVEL     ORGDES        §LDDLP
075300910207     C   30              MOVEL     *BLANKS       §LDDLP
075400991214     C     BLDCDP        IFNE      *BLANKS
075500910207     C                   MOVEL     BLDCDP        §LDCDP
075600991214     C                   ENDIF
075700171020
075800171020     c* etichetta ITalia export DPD --> imposto parcel number
075900171012     c                   if        lnpntw='DPD' or lnantw='DPD'
076000171012     c                   movel     pndipn        §ldcdp
076100171012     c                   endif
076200171012     c*
076300910207     C                   MOVEL     BLDNRS        §LDNRS
076400910313     C                   MOVEL     BLDTFA        §LDTNA
076500910207     C                   MOVEL     BLPTSP        §LDTSP
076600910207     C                   MOVEL     BLPZNC        §LDZSC
076700910313     C                   MOVE      BLPNCL        §LDNC2
076800010302     C* SE DEVO STAMPARE ETICHETTA EXPORT DPD PRIMA DELLA RAGIONE SOC.
076900010302     C* MITTENTE DEVO STAMPARE IL RMA SENZA I BLANK NON SIGNIFICATIVI
077000171012     C**   WPRMA         IFEQ      '1'
077100171012     C**                 CLEAR                   W35
077200171012     C**                 MOVEA     BLPRMA        W35
077300171012     C**   ' '           CHECKR    BLPRMA        §Z                2 0
077400171012     C**                 ADD       2             §Z
077500171012     C**                 MOVEA     BLPRSM        W35(§Z)
077600171012     C**                 MOVEA     W35           §LDRSM
077700171012     C**                 ELSE
077800910207     C                   MOVEL     BLPRSM        §LDRSM
077900171012     C**                 ENDIF
078000910207     C                   MOVEL     BLPPRM        §LDPRM
078100910207     C                   MOVEL     BLPRSD        §LDRSD
078200910207     C                   MOVEL     BLPCAD        §LDCAD
078300910207     C                   MOVEL     BLPLOD        §LDLOD
078400910404     C                   MOVEL     BLPIND        §LDIND
078500910207     C                   MOVEL     BLPPRD        §LDPRD
078600950116     C                   MOVE      BLPAAS        §LDAAS
078700950116     C                   MOVE      BLPMGS        §LDMGS
078800950116     C                   MOVE      BLDPDR        §LDCPD
078900910207     C                   MOVEL     *ZEROS        §LDNCP
079000910207     C                   MOVEL     *ZEROS        §LDNCU
079100050418if  1c                   if        §AR5bva = 'V'
079200050418     c                   movel     'X'           §LDrst
079300050418x   1c                   else
079400910415     C                   MOVEL     'R'           §LDRST
079500050418e   1c                   endif
079600941228     C                   Z-ADD     BLPPKF        §LDPKG
079700941228     C                   Z-ADD     BLPVLF        §LDVOL
079800941228     C                   MOVEL     BLPFVF        §LDFVR
079900971219     C*
080000971219     C                   EXSR      ETIEST
080100910208     C*
080200910208     C     COMSCD        DOWLE     BLDNCA
080300990318     C     COMSCD        ANDGT     0
080400910208     C                   SETOFF                                       21
080500910208     C* SE SELEZIONE SEGNACOLLI CONTROLLO CHE SIA NEL RANGE
080600910408     C   07
080700941228     COR 08COMSCD        IFLT      VSCSCD
080800910208     C                   SETON                                        21
080900910208     C                   END
081000910208     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE
081100941228     C   08
081200941228     CANN21COMSCD        IFGT      VSCSCA
081300910208     C                   Z-ADD     BLDNCA        COMSCD
081400910208     C                   SETON                                        21
081500910208     C                   END
081600910408     C* SE > ESCO NON CE NE SONO PIU' DA STAMPARE      SOLO DAL
081700941228     C   07
081800941228     CANN21COMSCD        IFGT      VSCSCD
081900910409     C                   Z-ADD     BLDNCA        COMSCD
082000910408     C                   SETON                                        21
082100910408     C                   END
082200910208     C* S T A M P A
082300910208     C  N21              MOVEL     COMSCD        §LDNCD
082400020215
082500020215     c                   if        not *in21
082600020215     c                   select
082700020215      * per segnacolli FedEx imposta §LDCDP
082800020215     c                   when      lnantw = 'FED'
082900020215     c                   exsr      sr_fedex
083000020215      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
083100040507     c                   when      lnpntw = 'EEX'
083200020215     C                   EXSR      EUREXP
083300020215     c                   endsl
083400020215     c                   endif
083500020215
083600910208     C*
083700941228     C  N21              CALL      PSS
083800910207     C                   PARM                    DSBLD
083900971219     C                   PARM                    DSBLDE
084000910207     C*
084100910207     C                   ADD       1             COMSCD
084200910219     C                   ADD       1             §LDNC1
084300950419     C*
084400950419     C* CONTO IL NUMERO DI CHIAMATE PER SAPERE IL NUMERO DI PAGINE
084500950420     C* STAMPATE: SE > 6000 INTERROMPO LA STAMPA
084600041012     C  n21              ADD       1             WPAGIN
084700080220     c                   IF        NOT  *in21
084800080220     c                             and  wPagin > §VPOmaxLdV
084900950419     C                   MOVEL     'E'           PARFLG
085000950419     C* IMPOSTO 9999999 IN COMSCD PER INTERROMPERE IL CICLO
085100950419     C                   Z-ADD     *ALL'9'       COMSCD
085200950419     C                   ENDIF
085300950419     C*
085400910207     C                   END
085500910208     C*
085600910207     C                   ENDSR
085700910207     C*
085800910207     C*--- CONFRONTO PARZIALIZZAZIONI FATTE -------------------------*
085900910207     C     CONFR         BEGSR
086000910207     C                   SETOFF                                       90
086100941228     C* SIGLA OPERATORE
086200941228     C   03BLPSOP        IFNE      VSCSOP
086300941228     C                   SETON                                        90
086400941228     C                   GOTO      ENDCON
086500941228     C                   END
086600941228     C* LINEA ARRIVO
086700941228     C   06BLPLNA        IFNE      VSCLNA
086800941228     C                   SETON                                        90
086900941228     C                   GOTO      ENDCON
087000941228     C                   END
087100941228     C* FILIALE SEGNACOLLO
087200941228     C   17BLPFLS        IFNE      VSCFLS
087300941228     C                   SETON                                        90
087400941228     C                   GOTO      ENDCON
087500941228     C                   END
087600941228     C*
087700941228     C* CODICE CLIENTE MITTENTE
087800941228     C   09BLPCCM        IFNE      0
087900941228     C     BLPCCM        IFNE      VSCKSC
088000941228     C                   SETON                                            90
088100941228     C                   GOTO      ENDCON
088200941228     C                   END
088300941228     C                   ELSE
088400941228     C     BLPKSC        IFNE      VSCKSC
088500941228     C                   SETON                                            90
088600941228     C                   GOTO      ENDCON
088700941228     C                   END
088800941228     C                   END
088900941228     C* CODICE PADRONCINO
089000941228     C   10BLPPDR        IFNE      VSCCPR
089100941228     C                   SETON                                            90
089200941228     C                   GOTO      ENDCON
089300941228     C                   END
089400950116     C* RITIRO MATTINA/POMERIGGIO
089500950116     C     VSCFPP        IFNE      ' '
089600950116     C     BLPFPP        ANDNE     VSCFPP
089700950116     C                   SETON                                            90
089800950116     C                   GOTO      ENDCON
089900950116     C                   END
090000950116     C* NUMERO RITIRO
090100950116     C     VSCNRT        IFNE      ' '
090200950116     C                   MOVEL     BLPNRT        W001A             1
090300950116     C     W001A         IFNE      VSCNRT
090400950116     C                   SETON                                            90
090500950116     C                   GOTO      ENDCON
090600950116     C                   END
090700950116     C                   END
090800970701     C* SI NO DDT
090900970701     C     VSCBAM        IFNE      ' '
091000970701     C     VSCBAM        IFEQ      'S'
091100970701     C     BLPFST        IFEQ      'N'
091200970701     C     BLPFST        OREQ      'Y'
091300970701     C     BLPFST        OREQ      'K'
091400970701     C                   SETON                                            90
091500970701     C                   GOTO      ENDCON
091600970701     C                   END
091700970701     C                   END
091800970701     C     VSCBAM        IFEQ      'N'
091900970701     C     BLPFST        IFEQ      'S'
092000970701     C     BLPFST        OREQ      'C'
092100970701     C                   SETON                                            90
092200970701     C                   GOTO      ENDCON
092300970701     C                   END
092400970701     C                   END
092500970701     C                   END
092600000614      * SI/NO/SOLO POSTE
092700000614     C                   CLEAR                   OG143
092800000614     C     BLPLNP        CHAIN     AZORG                              30
092900000614     C  N30              MOVEL     ORGDE3        OG143
093000000614      *
093100000614     C     VSCSNP        IFEQ      'N'
093200020215     c     §ogntw        andeq     'PPT'
093300000614     C                   SETON                                            90
093400000614     C                   GOTO      ENDCON
093500000614     C                   ENDIF
093600000614      *
093700000614     C     VSCSNP        IFEQ      'P'
093800020220     c     §ogntw        andne     'PPT'
093900000614     C                   SETON                                            90
094000000614     C                   GOTO      ENDCON
094100000614     C                   ENDIF
094200910207     C*
094300910207     C     ENDCON        ENDSR
094400971217     C**
094500020215      *---------------------------------------------------------------
094600971217     C* STAMPA ETICHETTA ESTERA SDI O BARTOLINI ----------------------*
094700020215      *---------------------------------------------------------------
094800971217     C     ETIEST        BEGSR
094900971217     C*
095000081007     c     §detse        ifeq      'EEX'
095100971219     C* IMPOSTO DSBLD
095200081007     C                   MOVEL     'ITA'         §DETST
095300971219     C**
095400971217     C* CHAIN SULLE NAZIONI
095500971217     C                   MOVEL     '15'          COD
095600971217     C                   MOVEL(P)  BLPNZD        KEY
095700971219     C     KTAB          CHAIN     TABEL                              30
095800971217     C  N30TBLFLG        IFNE      ' '
095900971217     C                   SETON                                        30
096000971217     C                   END
096100971217     C**
096200971217     C  N30              MOVEL     TBLUNI        DS15
096300971217     C   30              CLEAR                   DS15
096400971217     C                   MOVEL     §15COD        §DEISA
096500971217     C**
096600971217     C                   MOVEL     '3A'          COD
096700971217     C                   MOVEL(P)  BLPCBO        KEY
096800971219     C     KTAB          CHAIN     TABEL                              30
096900971217     C  N30TBLFLG        IFNE      ' '
097000971217     C                   SETON                                        30
097100971217     C                   END
097200971217     C**
097300971217     C                   MOVEL     TBLUNI        DS3A
097400971217     C                   MOVEL     §3ATB1        FLGTB1            1
097500971217     C**
097600971217    3C                   SELECT
097700971217     C     FLGTB1        WHENEQ    'A'
097800971217     C     §3AFCA        ANDEQ     0
097900971217     C                   MOVEL     'A'           §DEFCA
098000971217     C     FLGTB1        WHENEQ    'A'
098100971217     C     §3AFCA        ANDNE     0
098200971217     C                   MOVEL     'X'           §DEFCA
098300971217     C     §3AFCA        WHENNE    0
098400971217     C                   MOVEL     'S'           §DEFCA
098500971217    3C                   ENDSL
098600971219     C* ETICHETTA ITALIA
098700971219   X1C                   ELSE
098800971219     C                   MOVEL     'ITA'         §DETST
098900971217    1C                   ENDIF
099000971217     C**
099100971217     C                   ENDSR
099200020215      *----------------------------------------------------------------
099300020215      *--- IMPOSTO CAMPI SEGNACOLLO DA PASSARE A PGM DI STAMPA     ---*
099400020215      *----------------------------------------------------------------
099500020215     C     EUREXP        BEGSR
099600020215
099700020215     C                   MOVEL     'E'           WTRC
099800051012     C     KarS01        CHAIN     FIarS01L                           30
099900980402    2C     *IN30         IFEQ      *OFF
100000980402     C* PASSO AL PGM DI STAMPA ETICHETTE IL N. SEGNACOLLO EUROEXPRESS
100100051012     C                   MOVEL     arSNOT        §DEEUR
100200980402     C* SE IMPORT BARTOLINI METTO NEL CODICE PRODOTTO DELL'ETICHETTA
100300980402     C* ITALIA IL N.SEGNACOLLO EUROEXPRESS
100400020215   3 c                   if        lnpntw = 'EEX'
100500051012     C                   MOVEA     arSNOT        W35
100600980402     C                   Z-ADD     1             BB                2 0
100700980402     C     ' '           LOOKUP    W35(BB)                                30
100800980402     C   30              MOVEA     *BLANKS       W35(BB)
100900980402     C                   MOVEA     W35(6)        §LDCDP
101000980402    3C                   ENDIF
101100980402    2C                   ENDIF
101200980402     C                   ENDSR
101300020215
101400020215      *----------------------------------------------------------------
101500020215      *--- IMPOSTO CAMPI SEGNACOLLO DA PASSARE A PGM DI STAMPA     ---*
101600020215      *---------------------------------------------------------------*
101700020215     c     sr_fedex      begsr
101800020215
101900020215     c                   eval      wtrc = 'F'
102000051012     c     kars01        chain     fIars01l
102100051012     c                   if        %found(fIars01l) and arsnot <> *blanks
102200051012     c                   movel     arsnot        §ldcdp
102300020215     c                   endif
102400020215
102500020215     c                   endsr
102600050418      *---------------------------------------------------------------*
102700050418      *  Verifica se spedizione "DI VALORE"                           *
102800050418      *---------------------------------------------------------------*
102900050418     c     CtrSpeVal     BEGSR
103000050418      *
103100050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
103200050418if  1c                   if            BLPaas = AR5aas
103300050418     c                             and BLPlnp = AR5lnp
103400050418     c                             and BLPnrs = AR5nrs
103500050418     c                             and BLPnsp = AR5nsp
103600050418     c                   leavesr
103700050418e   1c                   endif
103800050418      *
103900050418      * Reperisce i dati da FIAR5
104000050418     c                   clear                   dAR5gen
104100050418     c     K05AR501      setgt     FIAR5000
104200050418do  1c                   dou            %eof(FIAR501L)
104300050418     c                             or   AR5atb =  *blanks
104400050418     c     K05AR501      readpe    FIAR5000
104500050418e   1c                   enddo
104600050418if  1c                   if             NOT %eof(FIAR501L)
104700050418     c                             and  AR5atb =  *blanks
104800050418     c                   movel     AR5uni        dAR5gen
104900050418e   1c                   endif
105000050418      *
105100050418     c                   ENDSR
105200020215
105300941228     OFNLV22P   T    LRNLR
