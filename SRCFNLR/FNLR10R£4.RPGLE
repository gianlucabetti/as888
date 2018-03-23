000100021105     H DECEDIT('0,') DATEDIT(*ymd.)
000200020906      * FNLR10R *-----------------------------------------------------*
000300020906      *           PULIZIA SPUNTE E ANOMALIE                           *
000400020906      *---------------------------------------------------------------*
000500070115     FFNBRV07L  UF   E           K DISK
000600070115     F                                     RENAME(FNBRV000:FNBRV007)
000700070115     FFNBRVE1L  UF   E           K DISK    USROPN
000800070115     F                                     RENAME(FNBRV000:FNBRVE)
000900990510     FFNANM02L  UF   E           K DISK
001000990510     F                                     RENAME(FNANM000:FNANM002)
001100970611     FFNEVS02L  UF   E           K DISK
001200941214     FFNBLP01L  IF   E           K DISK
001300960613     FFNBLT27L  IF   E           K DISK
001400050331     FTigcp01L  IF   E           K DISK
001500941214     FFNARB01L  IF   E           K DISK
001600941214     FFNART27L  IF   E           K DISK
001700050331     FTigcp21L  IF   E           K DISK
001800050331     F                                     RENAME(TIGCP000:TIGCP21)
001900990804     FFNEVB01L  IF   E           K DISK
002000941214     FFNLBL01L  IF   E           K DISK
002100941214     FFNLBL02L  IF   E           K DISK
002200941214     F                                     RENAME(FNLBL000:FNLBL002)
002300930111     FTABEL00F  IF   E           K DISK
002400020906     FAZORG01L  IF   E           K DISK
002500051007     Ffndct01l  IF   E           K DISK
002600020906
002700960506     D*
002800960506     D** DEFINIZIONE SCHIERE
002900020808     D*
003000080903     D EFS             S              3    DIM(999)
003100061023     D anom            S              1    DIM(50)
003200960506     D SAN             S              3  0 DIM(30)                              SOGGETTO ANOMALIA
003300961106     D CHA             S              3  0 DIM(30)                              AN.ESTERNE AUTOCHIUS
003400020906
003500960506     D*
003600020906     D** DEFINIZIONE STRUTTURE DATI ESTERNE
003700020808     D*
003800930519     D KPJBA         E DS
003900100504      * - Parametri x Controllo profilo utenti
004000100504     d TIBS34ds      e ds                  inz
004100100504      * - Ds di riferimento al file esterno AZUTE00F
004200100504     d AZUTEds       e ds                  inz extname(AZUTE00F)
004300100504      * - Ds per dati organigramma
004400100504     d dDATIUTE      e ds                  inz
004500100504      *
004600100618     D xgiolavds     E DS
004700100618     D dvpopuladd    E DS
004800100618     D FNLV53DS      E DS
004900020808     D DS2Z          E DS
005000061023     D DS3E          E DS
005100020808     D DS7C          E DS
005200100504     D DS5A          E DS
005300040806      * ds per il controllo della presenza di CA per la spedizione richiesta
005400040806     d FIDN12DS      e ds
005500051007     d  skKey                 26    305    dim(20)
005600051007     d  skDca                326    485  0 dim(20)
005700051007     d
005800051007     d dsKey           ds
005900051007     d  dctaac                        4  0
006000051007     d  dctfil                        3  0
006100051007     d  dctnca                        7  0
006200020808     D*
006300950303     D                 DS
006400950303     D  BRVLNP                 1      3  0
006500950303     D  BRVLNA                 4      6  0
006600950303     D  BRVNRS                 7      8  0
006700950303     D  BRVNSC                 9     15  0
006800950303     D  BRVSE                  1     15  0
006900970611     D                 DS
007000970611     D  WRVLNP                 1      3  0
007100970611     D  WRVLNA                 4      6  0
007200970611     D  WRVNRS                 7      8  0
007300970611     D  WRVNSC                 9     15  0
007400970611     D  WBRVSE                 1     15  0
007500950303     D                 DS
007600950303     D  BLTAAS                 1      4  0
007700950303     D  BLTLNP                 5      7  0
007800950303     D  BLTNRS                 8      9  0
007900950303     D  BLTNSP                10     16  0
008000950303     D  BLTSPE                 1     16  0
008100950303     D                 DS
008200950303     D  ARTAAS                 1      4  0
008300950303     D  ARTLNP                 5      7  0
008400950303     D  ARTNRS                 8      9  0
008500950303     D  ARTNSP                10     16  0
008600950303     D  ARTSPE                 1     16  0
008700950403     D                 DS
008800950404     D  BRVNPG                 1      1  0
008900020916     d  BrvFgs                 2      4  0
009000000125     D  BRVNFV                 5     10  0
009100000125     D  WFVV                   1     10  0
009200960506     D*
009300100504     D WLBDAT          DS                  inz
009400100504     D  G02DAT                 1      8  0 inz
009500100504     D  G02INV                 9     16  0 inz
009600100504     D  G02ERR                17     17    inz
009700100504     D  G02TGI                18     22  0 inz
009800100504     D WGIDAT          DS                  inz
009900100504     D  GIODAT                 1      8  0 inz
010000100504     D  GIOINV                 9     16  0 inz
010100100504     D  GIOTGI                17     21  0 inz
010200020906
010300020906     D*
010400020906     D** DEFINIZIONE CAMPI
010500020906     D*
010600100504     d CODUT           s                   like(TBLkut) inz(1)
010700100504     d KEY             s                   like(TBLKEY) inz
010800020906     D COD             S                   LIKE(TBLCOD) inz
010900020906     D PUDCS           S                   LIKE(BRVDCS) inz
011000020906     D PULNP           S                   LIKE(BRVLNP) inz
011100020906     D WAAS            S                   LIKE(EVBAAS) inz
011200020906     D WDAT            S                   LIKE(DATSPP) inz
011300020906     D SAVFVV          S                   LIKE(WFVV)   inz
011400020906     D SAVBRC          S                   LIKE(DATBRC) inz
011500020906     D WMAX            S                   LIKE(TBLKEY) inz
011600020906     D SESAV           S                   LIKE(BRVSE)  inz
011700070821     D FVSAV           S                   LIKE(WFVV)   inz
011800020906     D XI              S              2  0
011900020906     D XX              S              3  0
012000070821     D WFILEERR        S              1    inz
012100070821     D WOK             S              1    inz
012200020906     D WGIR            S              1    inz
012300020906     D W0080           S              8  0 inz
012400020906     D WCABLP          S              1    inz
012500020906     D SAVse           S             15  0 inz
012600020925     D SAVpar          S             15  0 inz
012700020906     D SAVspe          S             16  0 inz
012800020906     D SAVsp2          S             16  0 inz
012900020906      * Flag "Trovata bolla"
013000020906     d $Found          s              1    inz(*off)
013100090416     d DataPuliz       s              8  0 inz
013200100504     d COMBRV          s                   like(§5ABRV) inz
013300100504     d COMBRC          s                   like(§5ABRC) inz
013400100504     d datbrv          s              8  0 inz
013500100504     d datbrc          s              8  0 inz
013600100504     d datSPP          s              8  0 inz
013700100504     d datSca          s              8  0 inz
013800100504     d datcan          s              8  0 inz
013900020906
014000960506     I*
014100990510     I** RIDENOMINAZIONE CAMPI FNANM
014200990510     IFNANM002
014300960430     I              ANMLNA                      BRVLNA
014400960430     I              ANMNRS                      BRVNRS
014500960430     I              ANMSCN                      BRVNSC
014600020906
014700941214     C*---------------------------------------------------------------*
014800941214     C* INDICATORI USATI                                              *
014900941214     C*---------------------------------------------------------------*
015000960506     C* 21    - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE OPPURE CON
015100960506     C*         TIPO ANOMALIA "T"
015200961106     C* 22    - ANOMALIA ESTERNA AUTOCHIUSA
015300020926     C* 31 33 - uso esclusivo della RICPAR
015400020926     C* 32 34 - uso esclusivo della RICARR
015500061023     C* 36    - controllo se brvcan ha anomalia x danni
015600020926     C* 30/39 - CHAIN SUI FILES
015700930519     C*---------------------------------------------------------------*
015800930519     C*
015900930519     C* DEFINIZIONE VARIABILI, KLIST E PLIST
016000930520     C                   EXSR      DEFVAR
016100100513     c*
016200100513   -1c                   if        datbrv>0 and datbrc>0 and datspp>0
016300930520     C*
016400930521     C* CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE
016500930521     C                   EXSR      CARTAB
016600950303     C*
016700970611     C                   CLEAR                   WBRVSE
016800970611     C*
016900960506     C* ESEGUO 2 VOLTE IL CICLO DEL PGM: -PER LA PULIZIA DELLE ANOMALIE
017000960506     C*                                  -PER LA PULIZIA DELLE SPUNTE
017100020906     C                   MOVEL     'A'           WGIR
017200960506     C*
017300960515    0C                   DO        2
017400950303     C                   CLEAR                   SAVSE
017500020925     C                   CLEAR                   SAVpar
017600970611     C                   CLEAR                   SESAV
017700950303     C                   CLEAR                   SAVSPE
017800950303     C                   CLEAR                   SAVSP2
017900950403     C                   CLEAR                   SAVFVV
018000070821     C                   CLEAR                   WfileERR
018100941214     C*
018200960430     C* LETTURA  A N O M A L I E
018300980217     C*  PER LE ANOMALIE PULIAMO TUTTO CON LA DATA PIU' ALTA
018400960430    1C     WGIR          IFEQ      'A'
018500960515     C                   MOVEL     DATBRC        SAVBRC
018600960515     C                   MOVEL     DATBRV        DATBRC
018700960515     C*
018800990510     C     *LOVAL        SETLL     FNANM002
018900990510     C                   READ      FNANM002                             9930
019000990510     C   99              READ(N)   FNANM002                               30
019100960515     C*
019200960430   X1C                   ELSE
019300960515     C                   MOVEL     SAVBRC        DATBRC
019400960430     C* LETTURA  S P U N T E
019500070115     C     *LOVAL        SETLL     FNBRV007
019600070115     C                   READ      FNBRV007                             9930
019700070115     C   99              READ(N)   FNBRV007                               30
019800960430    1C                   ENDIF
019900941214     C*
020000941214    1C     *IN30         DOWEQ     *OFF
020100960730     C*
020200960730     C* SOLO SE SONO RIUSCITO AD ALLOCARE IL RECORD
020300960730   1AC     *IN99         IFEQ      *OFF
020400960430     C*
020500990510     C* SOLO PER  A N O M A L I E :
020600960506    2C     WGIR          IFEQ      'A'
020700960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
020800960903     C*                           CANCELLARE L'ANOMALIA
020900980217    3C     ANMDAO        IFEQ      0
021000990510     C                   Z-ADD     19000101      PUDCS
021100980217   X3C                   ELSE
021200990510     C                   Z-ADD     ANMDAO        PUDCS
021300980217    3C                   ENDIF
021400980217     C**
021500980217   X2C                   ELSE
021600021105      * Prendo la data minore tra caricamento e immissione se tutte e 2 sono
021700021105      * maggiori di udate
021800070821     c                   EXSR      CerDATbrv
021900070821     c
022000980217    2C                   ENDIF
022100960430     C*
022200980217     C* SE LA DATA APERTURA ANOMALIA O
022300980217     C*    LA DATA DI CARICAMENTO SPUNTA E' > DELLA DATA PIU' RECENTE
022400950403     C*  DI PULIZIA --> NON LA CONSIDERO NEMMENO
022500980217    2C     PUDCS         IFLE      WDAT
022600020906     C                   MOVEL     'S'           WOK
022700941214     C*
022800960430     C* SOLO PER  A N O M A L I E
022900960430    3C     WGIR          IFEQ      'A'
023000960506     C*
023100960506     C* 21 ON  - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE
023200960506     C     ANMCAA        LOOKUP    SAN                                    21
023300960506     C*
023400960430     C* LE ANOMALIE "E/R" SI PULISCONO SOLO SE HANNO LA DATA CHIUSURA
023500960506    4C  N21ANMAIE        IFEQ      'E'
023600960430     C     ANMAIE        OREQ      'R'
023700961106     C                   SETOFF                                       22
023800961106     C*
023900961106     C* 22 ON  - SI TRATTA DI ANOMALIA ESTERNA AUTOCHIUSA
024000961106    5C     ANMAIE        IFEQ      'E'
024100961106     C     ANMCAA        ANDNE     70
024200961106     C     ANMCAA        ANDNE     71
024300961106     C     ANMCAA        LOOKUP    CHA                                    22
024400961106    5C                   ENDIF
024500961106     C*
024600961106    5C     ANMAIE        IFEQ      'E'
024700961106     C     *IN22         ANDEQ     *OFF
024800980217     C**
024900980217     C* VERIFICO SE LNA FA O MENO L'IDD
025000020906     c                   eval      XX = 1
025100050805     c**   BRVlna        lookup    SKpo(xx)                               37
025200050805     c** 37              movel     NIDD(xx)      LNANID
025300050805     c**N37              movel     'N'           LNANID
025400980217     C**
025500050805    6C**   LNANID        IFNE      'N'
025600980217    7C     ANMDCH        IFEQ      0
025700980217     C     ANMFT1        OREQ      ' '
025800980217     C     ANMFL1        ANDGT     0
025900980217     C     ANMFT2        OREQ      ' '
026000980217     C     ANMFL2        ANDGT     0
026100980217     C                   CLEAR                   WOK
026200980217    7C                   ENDIF
026300050805    6C**                 ENDIF
026400050805    5C                   ENDIF
026500961106     C*
026600960430   X4C                   ELSE
026700960506     C* 21 ON  - ANOMALIA CON TIPO ANOMALIA "T"
026800960506     C     ANMAIE        IFEQ      'T'
026900960506     C                   SETON                                        21
027000960430     C                   ENDIF
027100960430    4C                   ENDIF
027200960430    3C                   ENDIF
027300960430     C*
027400960430     C* WOK = "S" ---> RECORD DA ELABORARE
027500960506    3C     WOK           IFEQ      'S'
027600960430     C*
027700960506    4C     *IN21         IFEQ      *OFF
027800960530     C*
027900960506    5C     WGIR          IFEQ      'A'
028000960506     C     ANMLNP        IFGT      0
028100960506     C                   MOVEL     ANMLNP        BRVLNP
028200960506     C                   ELSE
028300960506     C                   MOVEL     ANMFLS        BRVLNP
028400960506     C                   ENDIF
028500960506    5C                   ENDIF
028600960530     C*
028700970611     C*
028800970611     C* A CAMBIO SEGNACOLLO:
028900970611     C     WGIR          IFEQ      'S'
029000970611     C     BRVSE         ANDNE     SESAV
029100970611     C* Verifico se per il collo precedente devo cancellare gli eventi
029200970611     C     WBRVSE        IFGT      *ZEROS
029300970611     C                   EXSR      DELEVS
029400970611     C                   CLEAR                   WBRVSE
029500970611     C                   END
029600970611     C                   MOVE      BRVSE         SESAV
029700970611     C                   END
029800941214     C*
029900020906     c                   eval      $Found = *off
030000941214     C* CERCO NELLE BOLLE IN ARRIVO
030100941214     C                   EXSR      RICARR
030200020906     c*
030300020906     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
030400020906     c                   if        $Found = *off
030500020906     c                   exsr      RICPAR
030600020906     c                   endif
030700020925     c                   else
030800020925     c* se anomalia senza spedizione o di tipo "T" eseguo solo il ciclo
030900020925     c*  NO BOLLE
031000020925     c                   eval      $Found = *off
031100020925    4c                   endif
031200020906     C*
031300020906     c                   if        $Found = *off
031400020906     C     WGIR          IFEQ      'A'
031500020906     C                   EXSR      NOBOL2
031600020906     C                   ELSE
031700020906     C                   EXSR      NOBOLL
031800020906     C                   ENDIF
031900020906     c                   endif
032000950403     C*
032100960506    3C                   ENDIF
032200960506    2C                   ENDIF
032300960730     C*
032400960730   1AC                   ENDIF
032500941214     C*
032600960506    2C     WGIR          IFEQ      'A'                                          *ANOMALIE
032700990510     C                   READ      FNANM002                             9930
032800990510     C   99              READ(N)   FNANM002                               30
032900960730   X2C                   ELSE                                                   *SPUNTE
033000960730     C*
033100070115     C                   READ      FNBRV007                             9930
033200070115     C   99              READ(N)   FNBRV007                               30
033300960506    2C                   ENDIF
033400960506    1C                   ENDDO
033500941214     C*
033600960506     C* IMPOSTO CAMPI PER PULIZIA SPUNTE
033700960506     C                   MOVEL     'S'           WGIR
033800960506     C                   SETOFF                                       21
033900960506    0C                   ENDDO
034000970611     C*
034100070821     C* Verifico se x l'ultimo collo devo cancellare gli eventi
034200970611     C     WBRVSE        IFGT      *ZEROS
034300970611     C                   EXSR      DELEVS
034400970611     C                   CLEAR                   WBRVSE
034500970611     C                   END
034600980210     C****
034700980210     C* PULIZIA SPUNTE ERRATE SEGNACOLLI
034800980210     C****
034900070821     c                   eval      wfileerr='S'
035000070115     C                   OPEN      FNBRVE1L
035100070115     C     *LOVAL        SETLL     FNBRVE1L
035200070115     C                   READ      FNBRVE1L                               30
035300980210    1C     *IN30         DOWEQ     *OFF
035400980210     C* Prendo la data foglio a cambio di numero
035500070821     c                   if        wfvv<>FVSAV
035600020808     C                   CLEAR                   FNLV53DS
035700070821     C                   Z-ADD     BRvNPG        D53NPG
035800070821     C                   Z-ADD     BRvNFV        D53NFV
035900070821     C                   Z-ADD     BRvFGS        D53FGS
036000980210     C                   CALL      'FNLV53R'
036100020808     C                   PARM                    FNLV53DS
036200070821     C                   MOVE      wfvv          FVSAV
036300090416     c* se errore, imposto data caricammento spunte in data foglio
036400090416     c                   if        d53err<>' '
036500090416     c                   movel     PUDCS         D53DFV
036600090416     c                   clear                   FVSAV
036700090416     c                   endif
036800090416     c
036900980210    2C                   ENDIF
037000070821     c
037100070821    2C     D53DFV        IFLE      DATSPP
037200070821     c
037300070821     c* Per categorie 4 / 8 / 1 partenza, deleto
037400070821    3c                   if        brvnpg=4 or brvnpg = 8 or
037500070821     c                             (brvnpg=1 and brvftr='L')
037600070115     C                   DELETE    FNBRVE
037700070821   x3c                   else
037800070821     c
037900070823     c* Per le altre categorie mi legGo alla bolla
038000070823     c                   EXSR      CerDATbrv
038100070823     C
038200070821     c* Per le altre categorie mi lego alla bolla
038300070821     c                   eval      $Found = *off
038400070821     C* CERCO NELLE BOLLE IN ARRIVO
038500070821     C                   EXSR      RICARR
038600070821     c*
038700070821     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
038800070821    4c                   if        $Found = *off
038900070821     c                   exsr      RICPAR
039000070823     C                   ENDIF
039100070823     C
039200070823    4c                   if        $Found = *off
039300070821     C                   DELETE    FNBRVE
039400070821    4c                   endif
039500070821    3c                   endif
039600070821     c
039700070821    2C                   END
039800070115     C                   READ      FNBRVE1L                               30
039900980210    1C                   ENDDO
040000100513     c
040100100513   -1c                   endif
040200040806     C*
040300040806     C                   MOVEL     'C'           I12TLA
040400040806     C                   CALL      'FIDN12R'
040500040806     C                   PARM                    FIDN12DS
040600040806      *
040700020906     C                   MOVEL     *ON           *INLR
040800941214     C*
040900941214     C*---------------------------------------------------------------*
041000941214     C*                    RICPAR                                     *
041100941214     C* CERCO NELLE BOLLE IN PARTENZA                                 *
041200020926     c* indicatori 31 e 33 ad uso esclusivo della routine ricpar
041300941214     C*---------------------------------------------------------------*
041400941214     C     RICPAR        BEGSR
041500020926     c* Se anomlie senza segnacollo ma solo con la spedizione (70 e 71)
041600020926     c*  devo comunque entrare nell'if per chainare fnarb
041700020926     c                   if        wgir='A' and brvnsc=0
041800020926     c                   eval      savpar=*hival
041900020926     c                   endif
042000950303     C*
042100941214     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
042200020925    0C     BRVSE         IFNE      SAVpar
042300950303     C*
042400960508    1C     WGIR          IFEQ      'S'
042500960508     C     WGIR          OREQ      'A'
042600960508     C     ANMNSP        ANDEQ     0
042700941214     C     KBLT          CHAIN     FNBLT000                           31
042800960508     C*
042900960508   X1C                   ELSE
043000960514     C                   SETOFF                                       31
043100960508     C                   Z-ADD     ANMAAS        BLTAAS
043200960514     C                   MOVEL     ANMLNP        BLTLNP
043300960508     C                   MOVEL     BRVNRS        BLTNRS
043400960508     C                   MOVEL     ANMNSP        BLTNSP
043500960508    1C                   ENDIF
043600941214     C*
043700941214     C* AGGANCIO LA BOLLA
043800950303    1C  N31BLTSPE        IFNE      SAVSPE
043900950303     C     KBLP          CHAIN     FNBLP000                           31
044000990126     C* SE C'E' UNA C.A. NON CANCELLO LE SPUNTE
044100051006     c* 07 ott 05 --> addesso puliamo se c.a. in gestione alla sede
044200051006     c*               passati xxx giorni dall'apertura c.a.
044300040806      *
044400051007    1c                   If        not *in31
044500040806     c                   clear                   fidn12ds
044600040806     c                   eval      i12aas = bltaas
044700040806     c                   eval      i12lnp = bltlnp
044800040806     c                   eval      i12nrs = bltnrs
044900040806     c                   eval      i12nsp = bltnsp
045000040806     c                   eval      i12fel = 'E'
045100040806     c                   eval      i12fan = 'E'
045200040806      *
045300051007     c                   EXSR      CERCA
045400040806      *
045500950303    1C                   ENDIF
045600051007    1C                   ENDIF
045700950303     C*
045800020925     C                   MOVEL     BRVSE         SAVpar
045900950303    0C                   ENDIF
046000950120     C*
046100950120    1C     *IN31         IFEQ      *OFF
046200020906     c*
046300020906     c                   eval      $Found = *on
046400941214     C*
046500051007     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA e senza c.a.
046600051007     c
046700950303    2C     BLPDCM        IFGT      0
046800990126     C     WCABLP        ANDEQ     ' '
046900051007     c*   o con data c.a. < data gg pulizia bolle con c.a.
047000051007     c*   in questo caso pulisco anche se non è consegnata
047100051007     c     wcablp        oreq      'O'
047200950303     C*
047300960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
047400960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
047500980218   2AC     PUDCS         IFGT      BLPDCM
047600051007     c     blpdcm        andgt     0
047700960509     C*
047800090416     c                   select
047900090416     c                   when      wfileErr='S'
048000090416     c                   if        d53dfv<=datcan
048100090416     c                   delete    fnbrve
048200090416     c                   endif
048300090416     c                   when      WGIR='A'
048400960509     C                   EXSR      NOBOL2
048500090416     C                   other
048600960509     C                   EXSR      NOBOLL
048700090416     C                   ENDSL
048800960509     C*
048900960509  X2AC                   ELSE
049000090416     c*
049100090416     c* se spunta con "*" controlo direttamente la data consegna
049200090416     c*  con la data pulizia spunte con C.A.
049300090416   2bc                   if        brvnvr='*'
049400090416     c
049500090416    3c                   if        blpdcm<=datcan
049600090416     c                   exsr      BRVDEL
049700090416    3c                   endif
049800090416     c
049900090416  x2bc                   else
050000960509     C*
050100950303    3C     BLTSPE        IFNE      SAVSPE
050200941214     C*
050300941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
050400941214     C*                  - DATA CONSEGNA PARZIALE
050500950303    4C     BLPCCA        IFEQ      *BLANKS
050600941214     C     BLPDCP        ANDEQ     *ZEROS
050700950120     C                   SETOFF                                       33
050800950303   X4C                   ELSE
050900950120     C                   SETON                                        33
051000950303    4C                   ENDIF
051100941214     C*
051200981008     C* NON HA EVENTI (O SOLO QUELLI FASULLI)
051300090416    4C     *IN33         IFEQ      *OFF
051400981008     C                   Z-ADD     BLTAAS        WAAS
051500990804     C     KEVB          SETLL     FNEVB000
051600990804     C     KEVB          READE     FNEVB000                               98
051700090416    5C     *IN98         DOWEQ     *OFF
051800981008     C     EVBCEV        LOOKUP    EFS                                    97
051900981008     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
052000090416    6C     *IN97         IFEQ      *OFF
052100981008     C                   SETON                                        9833
052200981008     C                   ELSE
052300981008     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
052400990804     C     KEVB          READE     FNEVB000                               98
052500090416    6C                   ENDIF
052600090416    5C                   ENDDO
052700090416    4C                   ENDIF
052800941214     C*
052900941214     C* NON HA GIACENZA
053000050331     C  N33KBLP          SETLL     Tigcp01l                               33
053100941214     C*
053200941214     C* NON HA LEGAMI BOLLA (MAMMA)
053300950120     C  N33KBLP          SETLL     FNLBL000                               33
053400941214     C*
053500941214     C* NON HA LEGAMI BOLLA (FIGLIA)
053600950120     C  N33KBLP          SETLL     FNLBL002                               33
053700950303     C*
053800020906     C                   MOVEL     BLTSPE        SAVSPE
053900950303    3C                   ENDIF
054000090416     c
054100090416     c* Imposto la data di pulizia :
054200090416    3c                   select
054300090416     c* 1) Spunte nel file degli errori
054400090416     c                   when      wfileErr='S'
054500090416     c                   eval      datapuliz=DATcan
054600090416     C* 2) 33 OFF -    BOLLA SENZA EVENTI ANOMALI
054700090416     c                   when      *in33 =*off
054800090416     c                   eval      datapuliz=DATBRC
054900090416     c                   other
055000090416     C* 2)  altrimenti BOLLA CON   EVENTI ANOMALI
055100090416     c                   eval      datapuliz=DATBRV
055200090416    3c                   ENDSL
055300950303     C*
055400941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
055500090416    3C     BLPDCM        IFLE      DataPuliz
055600051007     c     blpdcm        andgt     0
055700051007     c* pulisco sempre se non vonsegna con c.a. perchè pulisco in base
055800051007     c*  alla data pulizia spunte con c.a.
055900051007     c     blpdcm        oreq      0
056000051007     c     wcablp        andeq     'O'
056100051007     c
056200090416    4C     WGIR          IFEQ      'A'
056300990510     C                   DELETE    FNANM002
056400090416   X4C                   ELSE
056500061019     c
056600090416     c* Se ha anomalia di spunta x DANNI, deleto solo se data <=data pulizia
056700061019     c*  spunte con anomalia
056800061023     c*
056900061023     c                   setoff                                       36
057000090416    5c                   if        brvcan<>' '
057100061023     c     brvcan        lookup    anom                                   36
057200090416    5c                   endif
057300061023     c
057400090416    5c                   if        not *in36  or wcablp='O'
057500070821     c                   EXSR      BRVDEL
057600090416   x5c                   else
057700061019     c
057800090416    6c                   if        blpdcm<=datcan
057900070821     c                   EXSR      BRVDEL
058000090416   x6c                   else
058100061019     c* altrimenti flaggo la spunta come non valida ai fini
058200061019     c*  del calcolo della responsabilità
058300090416    7c                   if        wfileerr=' '
058400070115     c                   eval      brvnvr='*'
058500070115     c                   update    fNbrv007
058600090416    7c                   endif
058700070821     c
058800090416    6c                   endif
058900090416    5c                   endif
059000090416    4C                   ENDIF
059100090416    3C                   ENDIF
059200090416   2bC                   ENDIF
059300090416   2AC                   ENDIF
059400941214     C*
059500950303   X2C                   ELSE
059600020906     C                   MOVEL     BLTSPE        SAVSPE
059700950120    2C                   ENDIF
059800941214     C*
059900950120    1C                   ENDIF
060000941214     C*
060100941214     C                   ENDSR
060200941214     C*
060300941214     C*---------------------------------------------------------------*
060400051007     C* cerco se bolla con c.a.
060500051007     C*---------------------------------------------------------------*
060600051007     c     CERCA         BEGSR
060700090417     C                   CLEAR                   WCABLP
060800051007     c                   call      'FIDN12R'
060900051007     c                   parm                    fidn12ds
061000051007      *
061100051007      * se non ci sono errori
061200051007    2c                   if        o12err = ' '
061300051007      * se numero di CA trovate maggiore di zero
061400051007    3c                   if        o12nca > 0
061500051007     c                   z-add     1             ii                3 0
061600051007    4c                   dow       skdca(II)>0 and wcablp=' '
061700051007    5c                   if        skdca(II)>datsca
061800051007     c                   movel     'S'           Wcablp
061900051007    5c                   endif
062000051007     c                   add       1             ii
062100051007    4c                   enddo
062200051007     c
062300051007     c* Se tutte le c.a. con data apertura < chain su dtc per controllare
062400070822     c*  che siano tutte LE APERTE in fase gestione SEDE
062500051007    4c                   if        wcablp=' '
062600051007     c                   z-add     1             ii
062700051007    5c                   dow       skdca(II)>0  and wcablp=' '
062800051007     c                   movel     skkey(II)     dskey
062900051007     c     kdct          chain     fndct01l
063000070822    6c                   if        %found(fndct01l) and dctgfc<>46  and
063100070822     c                             dctdch=0
063200051007     c                   movel     'S'           Wcablp
063300051007    6c                   endif
063400051007     c                   add       1             ii
063500051007    5c                   enddo
063600051007     c
063700051007     c                   if        wcablp=' '
063800051007     c                   eval      wcablp='O'
063900051007     c                   endif
064000051007     c
064100051007    4c                   endif
064200051007      *
064300051007    3c                   endif
064400051007      *
064500051007    2c                   endif
064600051007     c                   ENDSR
064700941214     C*---------------------------------------------------------------*
064800941214     C*                    RICARR                                     *
064900941214     C* CERCO NELLE BOLLE IN ARRIVO                                   *
065000020926     c* indicatori 32 e 34 ad uso esclusivo della routine ricarr
065100941214     C*---------------------------------------------------------------*
065200941214     C     RICARR        BEGSR
065300051007     c* Se anomalie  senza segnac.  ma solo con la spedizione (70 e 71)
065400020926     c*  devo comunque entrare nell'if per chainare fnarb
065500020926     c                   if        wgir='A' and brvnsc=0
065600020926     c                   eval      savse=*hival
065700020926     c                   endif
065800950303     C*
065900950303     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
066000950303    0C     BRVSE         IFNE      SAVSE
066100960508     C*
066200960508    1C     WGIR          IFEQ      'S'
066300960508     C     WGIR          OREQ      'A'
066400960508     C     ANMNSP        ANDEQ     0
066500950303     C     KBLT          CHAIN     FNART000                           32
066600960508     C*
066700960508   X1C                   ELSE
066800960514     C                   SETOFF                                       32
066900960508     C                   Z-ADD     ANMAAS        ARTAAS
067000960514     C                   MOVEL     ANMLNP        ARTLNP
067100960508     C                   MOVEL     BRVNRS        ARTNRS
067200960508     C                   MOVEL     ANMNSP        ARTNSP
067300960508    1C                   ENDIF
067400941214     C*
067500941214     C* AGGANCIO LA BOLLA
067600990126    1C  N32ARTSPE        IFNE      SAVSP2
067700950303     C     KARB          CHAIN     FNARB000                           32
067800051007     C* SE C'E' UNA C.A. cancello dalla data apertura c.a.
067900040806      *
068000090416    2c                   If        not *in32
068100040806     c                   clear                   fidn12ds
068200040806     c                   eval      i12aas = artaas
068300040806     c                   eval      i12lnp = artlnp
068400040806     c                   eval      i12nrs = artnrs
068500040806     c                   eval      i12nsp = artnsp
068600040806     c                   eval      i12fel = 'E'
068700040806     c                   eval      i12fan = 'E'
068800051007     c
068900051007     c                   EXSR      CERCA
069000040806      *
069100090416    2c                   endif
069200950303    1C                   ENDIF
069300950303     C*
069400950303     C                   MOVEL     BRVSE         SAVSE
069500950303    0C                   ENDIF
069600950120     C*
069700950303    1C     *IN32         IFEQ      *OFF
069800020906     c*
069900020906     c                   eval      $Found = *on
070000941214     C*
070100941214     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA
070200990126     C* E SE NON CI SONO C.A.
070300950120    2C     ARBDCM        IFGT      0
070400051007     C     WCAblp        ANDEQ     ' '
070500051007     c*  opure se ha c.a. con data < alla data di pulizia. cancello
070600051007     c*  anche le non consegnate
070700051007     C     WCAblp        oreq      'O'
070800960509     C*
070900960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
071000960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
071100061018     c*   se non ha c.a., altrimenti seguo la pulizia per data c.a.
071200980218   2AC     PUDCS         IFGT      ARBDCM
071300051007     c     arbdcm        andgt     0
071400061018     C     WCAblp        ANDEQ     ' '
071500960509     C*
071600090416     c                   select
071700090416     c                   when      wfileErr='S'
071800090416     c                   if        d53dfv<=datcan
071900090416     c                   delete    fnbrve
072000090416     c                   endif
072100090416     c                   when      WGIR='A'
072200090416     C                   EXSR      NOBOL2
072300090416     C                   other
072400090416     C                   EXSR      NOBOLL
072500090416     C                   ENDSL
072600960509     C*
072700960509  X2AC                   ELSE
072800090416     c*
072900090416     c* se spunta con "*" controllo direttamente la data consegna
073000090416     c*  con la data pulizia spunte con C.A.
073100090416   2bc                   if        brvnvr='*'
073200090416     c
073300090416    3c                   if        arbdcm<=datcan
073400090416     c                   exsr      BRVDEL
073500090416    3c                   endif
073600090416     c
073700090416  x2bc                   else
073800090416     C*
073900950303    3C     ARTSPE        IFNE      SAVSP2
074000941214     C*
074100941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
074200941214     C*                  - DATA CONSEGNA PARZIALE
074300950303    4C     ARBCCA        IFEQ      *BLANKS
074400941214     C     ARBDCP        ANDEQ     *ZEROS
074500950303     C                   SETOFF                                       34
074600950303   X4C                   ELSE
074700950303     C                   SETON                                        34
074800950303    4C                   ENDIF
074900941214     C*
075000000614     C* NON HA EVENTI O SOLO QUELLI FASULLI
075100950303     C  N34              Z-ADD     ARTAAS        WAAS
075200000614     C     *IN34         IFEQ      *OFF
075300000614     C     KEVB2         SETLL     FNEVB000
075400000614     C     KEVB2         READE     FNEVB000                               98
075500000614     C     *IN98         DOWEQ     *OFF
075600000614     C     EVBCEV        LOOKUP    EFS                                    97
075700000614     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
075800000614     C     *IN97         IFEQ      *OFF
075900000614     C                   SETON                                        9834
076000000614     C                   ELSE
076100000614     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
076200000614     C     KEVB2         READE     FNEVB000                               98
076300000614     C                   ENDIF
076400000614     C                   ENDDO
076500000614     C                   ENDIF
076600941214     C*
076700941214     C* NON HA GIACENZA
076800050331     C  N34KARB          SETLL     Tigcp21l                               34
076900941214     C*
077000941214     C* NON HA LEGAMI BOLLA (MAMMA)
077100950303     C  N34KARB          SETLL     FNLBL000                               34
077200941214     C*
077300941214     C* NON HA LEGAMI BOLLA (FIGLIA)
077400950303     C  N34KARB          SETLL     FNLBL002                               34
077500950303     C*
077600020906     C                   MOVEL     ARTSPE        SAVSP2
077700950303    3C                   ENDIF
077800090416     c
077900090416     c* Imposto la data di pulizia :
078000090416    3c                   select
078100090416     c* 1) Spunte nel file degli errori
078200090416     c                   when      wfileErr='S'
078300090416     c                   eval      datapuliz=DATcan
078400090416     C* 2) 34 OFF -    BOLLA SENZA EVENTI ANOMALI
078500090416     c                   when      *in34 =*off
078600090416     c                   eval      datapuliz=DATBRC
078700090416     c                   other
078800090416     C* 2)  altrimenti BOLLA CON   EVENTI ANOMALI
078900090416     c                   eval      datapuliz=DATBRV
079000090416    3c                   ENDSL
079100950120     C*
079200941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
079300090416    3C     ARBDCM        IFLE      Datapuliz
079400051007     c     arbdcm        andgt     0
079500051007     c
079600051007     c     arbdcm        oreq      0
079700051007     c     wcablp        andeq     'O'
079800051007     c
079900090416    4C     WGIR          IFEQ      'A'
080000990510     C                   DELETE    FNANM002
080100090416   x4C                   ELSE
080200061023     c* Se ha anomalia di spunta x danni, deleto solo se data <=data pulizia
080300061019     c*  spunte con anomalia
080400061023     c*
080500061023     c                   setoff                                       36
080600090416    5c                   if        brvcan<>' '
080700061023     c     brvcan        lookup    anom                                   36
080800090416    5c                   endif
080900061023     c
081000090416    5c                   if        not *in36   or Wcablp='O'
081100070821     c                   EXSR      BRVDEL
081200090416   x5c                   else
081300061019     c
081400090416    6c                   if        arbdcm<=datcan
081500070821     c                   EXSR      BRVDEL
081600090416   x6c                   else
081700061019     c* altrimenti flaggo la spunta come non valida ai fini
081800061019     c*  del calcolo della responsabilità
081900090416    7c                   if        wfileerr=' '
082000070115     c                   eval      brvnvr='*'
082100070115     c                   update    fnbrv007
082200090416    7c                   endif
082300070821     c
082400090416    6c                   endif
082500090416    5c                   endif
082600980114     C**
082700090416    4c                   ENDIF
082800090416    3C                   ENDIF
082900090416   2bC                   ENDIF
083000090416   2AC                   ENDIF
083100950120     C*
083200090416   x2C                   ELSE
083300020906     C                   MOVEL     ARTSPE        SAVSP2
083400950120    2C                   ENDIF
083500941214     C*
083600950120    1C                   ENDIF
083700941214     C*
083800941214     C                   ENDSR
083900941214     C*---------------------------------------------------------------*
084000941214     C*                    NOBOLL                                     *
084100941214     C* CANCELLAZIONE SPUNTE SENZA BOLLA                              *
084200941214     C*---------------------------------------------------------------*
084300941214     C     NOBOLL        BEGSR
084400051007     c
084500051007    1C     wfvv          IFNE      savfvv
084600051007     C                   CLEAR                   FNLV53DS
084700051007     C                   Z-ADD     BRvNPG        D53NPG
084800051007     C                   Z-ADD     BRvNFV        D53NFV
084900051007     C                   Z-ADD     BRvFGS        D53FGS
085000051007     C                   CALL      'FNLV53R'
085100051007     C                   PARM                    FNLV53DS
085200051007     C                   MOVE      wfvv          savfvv
085300051007     c                   movel     d53err        werr              1
085400051007     c                   movel     d53dfv        wDFV              8 0
085500051007    1C                   ENDIF
085600061023     c
085700061023     c                   setoff                                       36
085800061023     c                   if        brvcan<>' '
085900061023     c     brvcan        lookup    anom                                   36
086000061023     c                   endif
086100051007     c
086200051007    1c                   if        Werr=' '
086300061019     c*
086400051007    2C     Wdfv          IFLE      DATSPP
086500061019     c
086600061019     C                   MOVE      BRVSE         WBRVSE
086700061023    3c                   if        not *in36
086800070821     c                   EXSR      BRVDEL
086900061019   x3c                   else
087000061019    4c                   if        wdfv<=datcan
087100070821     c                   EXSR      BRVDEL
087200061019   x4c                   else
087300070821    5c                   if        wfileerr=' '
087400070115     c                   eval      brvnvr='*'
087500070115     c                   update    fnbrv007
087600070821    5c                   endif
087700070821     c
087800070821    4c                   endif
087900061019    3c                   endif
088000061019     c
088100051007    2C                   ENDIF
088200051007     C*
088300051007     c                   else
088400051007     c* se non trovato foglio pulisco in base alla data spunta
088500061019     c
088600061019    2C     PUDCS         IFLE      DATSPP
088700061019     C                   MOVE      BRVSE         WBRVSE
088800061019     c
088900061023    3c                   if        not *in36
089000070821     C****               DELETE    FNBRV007
089100070821     c                   EXSR      BRVDEL
089200061019   x3c                   else
089300061019    4c                   if        pudcs<=datcan
089400070821     C****               DELETE    FNBRV007
089500070821     c                   EXSR      BRVDEL
089600061019   x4c                   else
089700070821    5c                   if        wfileerr=' '
089800070115     c                   eval      brvnvr='*'
089900070115     c                   update    fnbrv007
090000070821    5c                   endif
090100070821     c
090200070821    4c                   endif
090300061019    3c                   endif
090400061019     c
090500051007    2C                   ENDIF
090600051007    1C                   ENDIF
090700941214     C*
090800941214     C                   ENDSR
090900970611     C*---------------------------------------------------------------*
091000970611     C* CANCELLAZIONE EVENTI SEGNACOLLI                               *
091100970611     C*---------------------------------------------------------------*
091200970611     C     DELEVS        BEGSR
091300970611     C* VERIFICO SE PER IL SEGNACOLLO ESISTONO ANCORA DELLE SPUNTE E
091400970611     C* SE E' PRESENTE ALMENO UNA SPUNTA NON CANCELLO GLI EVENTI
091500070115     C     KBRV          SETLL     FNBRV07L                               35
091600970611     C     *IN35         IFEQ      *OFF
091700970611     C     *IN30         DOUEQ     *ON
091800970611     C     KEVS          DELETE    FNEVS000                           30
091900970611     C                   ENDDO
092000970611     C                   END
092100970611     C* MI RIPOSIZIONO SULLA SPUNTA CHE STAVO LEGGENDO
092200070115     C     KBRV7         SETLL     FNBRV07L
092300070115     C     KBRV7         READE     FNBRV07L                               30
092400970611     C                   ENDSR
092500960506     C*
092600960506     C*---------------------------------------------------------------*
092700960506     C*                    NOBOL2                                     *
092800960506     C* CANCELLAZIONE:  - ANOMALIE SENZA BOLLA                        *
092900960506     C*                 - ANOMALIE SENZA SEGNACOLLO E SPEDIZIONE      *
093000960506     C*                 - ANOMALIE CON TIPO ANOMALIA "T"              *
093100960506     C*---------------------------------------------------------------*
093200960506     C     NOBOL2        BEGSR
093300960506     C* PRENDO LA MAGGIORE TRA DATA APERTURA E DATA CHIUSURA
093400960506     C     ANMDCH        IFGT      ANMDAO
093500020906     C                   Z-ADD     ANMDCH        W0080
093600960506     C                   ELSE
093700990510     C                   Z-ADD     ANMDAO        W0080
093800960506     C                   ENDIF
093900960506     C*
094000960508     C*
094100960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
094200960903     C*                           CANCELLARE L'ANOMALIA
094300960903     C     ANMDAO        IFEQ      0
094400990510     C                   Z-ADD     19000101      W0080
094500960903     C                   ENDIF
094600960903     C*
094700990510    1C     W0080         IFLE      DATSPP
094800990510     C                   DELETE    FNANM002
094900960506    1C                   ENDIF
095000960506     C*
095100960506     C                   ENDSR
095200960506     C*
095300070821     C*---------------------------------------------------------------*
095400070821     c* Cancella record di FNBRV o FNBRVE
095500070821     C*---------------------------------------------------------------*
095600070821     c     BRVDEL        BEGSR
095700070821     c                   if        wfileerr=' '
095800070821     C                   DELETE    FNBRV007
095900070821     c                   else
096000070821     c                   delete    FNBRVE
096100070821     c                   endif
096200070821     C                   ENDSR
096300930519     C*---------------------------------------------------------------*
096400930521     C*                    CARTAB                                     *
096500930521     C* ROUTINE DI CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE  *
096600930519     C*---------------------------------------------------------------*
096700930521     C     CARTAB        BEGSR
096800981008     C***
096900981008     C* EVENTI CHE SONO FASULLI DA NON CONSIDERARE PER LA PULIZIA
097000981008     C***
097100981008     C                   MOVEL     '2Z'          COD
097200020906     C                   Z-ADD     0             XI
097300981008     C     KTAB          SETLL     TABEL
097400981008     C     KTAB          READE     TABEL                                  31
097500981008     C*
097600981008DO  1C     *IN31         DOWEQ     '0'
097700981008IF  2C     TBLFLG        IFEQ      ' '
097800981008     C                   MOVEL     TBLUNI        DS2Z
097900981008     C*
098000981008IF  3C     §2ZEFS        IFEQ      'S'
098100020906     C                   ADD       1             XI
098200020906     C                   MOVEL     TBLKEY        EFS(XI)
098300981008E   3C                   ENDIF
098400981008E   2C                   ENDIF
098500981008     C*
098600981008     C     KTAB          READE     TABEL                                  31
098700981008E   1C                   ENDDO
098800930111     C*
098900960506     C***
099000960506     C* ANOMALIE  IDD
099100960506     C***
099200960506     C                   MOVEL     '7C'          COD
099300020906     C                   Z-ADD     0             XI
099400020906     C                   Z-ADD     0             XX
099500960506     C     KTAB          SETLL     TABEL
099600960506     C     KTAB          READE     TABEL                                  31
099700960506     C*
099800960506DO  1C     *IN31         DOWEQ     '0'
099900960506IF  2C     TBLFLG        IFEQ      ' '
100000960506     C                   MOVEL     TBLUNI        DS7C
100100960506     C*
100200960506IF  3C     §7CSAN        IFEQ      ' '
100300020906     C                   ADD       1             XI
100400020906     C                   MOVEL     TBLKEY        SAN(XI)
100500960506E   3C                   ENDIF
100600961106     C*
100700961106     C* CARICO LE ANOMALIE ESTERNE AUTOCHIUSE
100800961106IF  3C     §7CAIE        IFEQ      'E'
100900961106     C     §7CCHA        ANDEQ     'S'
101000020906     C                   ADD       1             XX
101100020906     C                   MOVEL     TBLKEY        CHA(XX)
101200961106E   3C                   ENDIF
101300960506E   2C                   ENDIF
101400960506     C*
101500960506     C     KTAB          READE     TABEL                                  31
101600960506E   1C                   ENDDO
101700061023     C***
101800061023     C* carico le anomalie di spunte per DANNI
101900061023     C***
102000061023     C                   MOVEL     '3E'          COD
102100061023     C                   Z-ADD     0             XI
102200061023     C     KTAB          SETLL     TABEL
102300061023     C     KTAB          READE     TABEL                                  31
102400061023     C*
102500061023DO  1C     *IN31         DOWEQ     '0'
102600061023IF  2C     TBLFLG        IFEQ      ' '
102700061023     C                   MOVEL     TBLUNI        DS3E
102800061023     C*
102900061023     c* Escludo le anomlie di rientro che servono per la
103000061023     c*  chiusura distinta
103100061023IF  3C     §3eFTA        IFne      'R'
103200061023     C                   ADD       1             XI
103300061023     C                   MOVEL     TBLKEY        ANOM(XI)
103400061023E   3C                   ENDIF
103500061023E   2C                   ENDIF
103600061023     C*
103700061023     C     KTAB          READE     TABEL                                  31
103800061023E   1C                   ENDDO
103900960506     C*
104000930524     C                   SETOFF                                       31
104100950403     C***
104200950403     C* DETERMINO LA DATA DI PULIZIA PIU' RECENTE
104300950403     C***
104400950403     C                   SELECT
104500950403     C     DATSPP        WHENGE    DATBRV
104600950403     C     DATSPP        ANDGE     DATBRC
104700950403     C                   Z-ADD     DATSPP        WDAT
104800950403     C*
104900950403     C     DATBRV        WHENGE    DATSPP
105000950403     C     DATBRV        ANDGE     DATBRC
105100950403     C                   Z-ADD     DATBRV        WDAT
105200950403     C*
105300950403     C                   OTHER
105400950403     C                   Z-ADD     DATBRC        WDAT
105500950403     C                   ENDSL
105600950403     C*
105700930111     C                   ENDSR
105800070821     C*---------------------------------------------------------------*
105900070821     C*                    CerDATbrv                                  *
106000070821     C* Cerco la data più recente della spunta letta                  *
106100070821     C*---------------------------------------------------------------*
106200070821     c     CerDATbrv     BEGSR
106300070821    3c                   Select
106400070821     c                   When      BrvDfs > *Date and BrvDcs > *Date
106500070821     c                   If        BrvDfs > BrvDcs
106600070821     c                   Eval      Pudcs = BrvDcs
106700070821     c                   Else
106800070821     c                   Eval      Pudcs = BrvDfs
106900070821     c                   EndIf
107000070821      * Prendo la data caricamento se data immissione maggiore di udate
107100070821     c                   When      BrvDfs > *Date
107200070821     c                   Eval      Pudcs = BrvDcs
107300070821      * Prendo la data immissione se data caricamento maggiore di udate
107400070821     c                   When      BrvDcs > *Date
107500070821     c                   Eval      Pudcs = BrvDfs
107600070821
107700070821     c                   Other
107800070821     C* PRENDO LA DATA PIÙ ALTA FRA QUELLA DI CARICAMENTO E QUELLA
107900070821     C* DI IMMISSIONE/VARIAZIONE
108000070821     C     BRVDFS        IFGE      BRVDCS
108100070821     C                   Z-ADD     BRVDFS        PUDCS
108200070821     C                   ELSE
108300070821     C                   Z-ADD     BRVDCS        PUDCS
108400070821     C                   ENDIF
108500070821    3c                   EndSl
108600070821     c                   ENDSR
108700930519     C*---------------------------------------------------------------*
108800930519     C*                    DEFVAR                                     *
108900930519     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
109000930519     C*---------------------------------------------------------------*
109100930519     C     DEFVAR        BEGSR
109200930519     C*
109300930519     C     *ENTRY        PLIST
109400930519     C                   PARM                    KPJBA
109500100504      *
109600100504     c     *dtaara       define    §azute        AZUTEds
109700100504     c     *dtaara       define    §datiute      dDATIUTE
109800100504     c                   in(E)     *dtaara
109900100504     c                   if        %ERROR or RSUT = *blanks
110000100504     c                   clear                   Tibs34Ds
110100100504     c                   call      'TIBS34R'
110200100504     c                   parm                    Tibs34Ds
110300100504     c                   in        *dtaara
110400100504     c                   endif
110500100504     C*
110600100504     c
110700100504     C                   CALL      'FNLV61R'
110800100504     C                   PARM                    O61DRF            8 0
110900100618     C                   PARM      ' '           i61Rep            1
111000100618     C                   PARM                    dvpopuladd
111100100618     c
111200100504     C                   z-add     o61drf        G02inv
111300100504     C                   MOVEL     '3'           G02ERR
111400100504     C                   CALL      'XSRDA8'
111500100504     C                   PARM                    WLBDAT
111600100618     c
111700100618     c* Se ci sono aggiuntivi di pulizia da sottrarre --> tolti anche questi
111800100504     c
111900100504     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A
112000100504     C                   MOVEL     '5A'          COD
112100100504     C                   MOVEL     '1       '    KEY
112200100504     C     KTAB2         CHAIN     TABEL                              30
112300100504     C  N30              MOVEL     TBLUNI        DS5A
112400100504     C   30              MOVEL     *ZEROS        DS5A
112500100504     C*
112600100504     C* CALCOLO DATE PULIZIA SPUNTE CON BOLLE CONSEGNATE SENZA EVENTI
112700100504     C*   ANOMALI E CON EVENTI ANOMALI
112800100504     C                   EXSR      CALDAT
112900100504     C*
113000100504     C* PULIZIA SPUNTE :  SOTTRAGGO §5ASPP AI GIORNI
113100100504     C*   PULIZIA SPUNTE DI DISGUIDI O BOLLE TRANSITO
113200100504     C     G02TGI        SUB       §5ASPP        GIOTGI
113300100618     c                   sub       §vpogg        GIOTGI
113400100504     C*
113500100504     C                   CALL      'XSRGI8'
113600100504     C                   PARM                    WGIDAT
113700100504     C                   Z-ADD     GIOINV        datSPP
113800100504      *
113900100504      * pulizia spunte con c.a.: sottraggo §5ASCA ai giorni
114000100504     c     G02tgi        sub       §5ASca        GIOtgi
114100100504     c                   call      'XSRGI8'
114200100504     c                   parm                    WGIdat
114300100504     c                   z-add     GIOinv        datSCA
114400100504      *
114500100504      * pulizia spunte con BRVCAN impostato: sottatto §5ABLP ai giorni
114600100504     c*  tengo infatti le spunte come le bolle partenza
114700100504     c     G02tgi        sub       §5Ablp        GIOtgi
114800100618     c                   sub       §vpogg        GIOTGI
114900100504     c                   call      'XSRGI8'
115000100504     c                   parm                    WGIdat
115100100504     c                   z-add     GIOinv        datcan
115200100504     c*
115300930519     C*
115400941214     C* LETTURA TABEL00F
115500941214     C     KTAB          KLIST
115600051007     C                   KFLD                    CODUT             1 0
115700941214     C                   KFLD                    COD
115800100504     C     KTAB2         KLIST
115900100504     C                   KFLD                    CODUT
116000100504     C                   KFLD                    COD
116100100504     C                   KFLD                    KEY
116200100504     C*
116300941214     C* LETTURA FNBLT27L
116400941214     C     KBLT          KLIST
116500941214     C                   KFLD                    BRVLNP
116600941214     C                   KFLD                    BRVLNA
116700941214     C                   KFLD                    BRVNRS
116800941214     C                   KFLD                    BRVNSC
116900941214     C* LETTURA FNBLP01L
117000941214     C     KBLP          KLIST
117100941214     C                   KFLD                    BLTAAS
117200941214     C                   KFLD                    BLTLNP
117300941214     C                   KFLD                    BLTNRS
117400941214     C                   KFLD                    BLTNSP
117500941214     C* LETTURA FNEVB01L
117600941214     C     KEVB          KLIST
117700941214     C                   KFLD                    WAAS
117800941214     C                   KFLD                    BLTLNP
117900941214     C                   KFLD                    BLTNRS
118000941214     C                   KFLD                    BLTNSP
118100941214     C     KEVB2         KLIST
118200941214     C                   KFLD                    WAAS
118300941214     C                   KFLD                    ARTLNP
118400941214     C                   KFLD                    ARTNRS
118500941214     C                   KFLD                    ARTNSP
118600941214     C* LETTURA FNARB01L
118700941214     C     KARB          KLIST
118800941214     C                   KFLD                    ARTAAS
118900941214     C                   KFLD                    ARTLNP
119000941214     C                   KFLD                    ARTNRS
119100941214     C                   KFLD                    ARTNSP
119200970611     C* LETTURA FNEVS02L
119300970611     C     KEVS          KLIST
119400040324     C                   KFLD                    WRVLNP
119500040324     C                   KFLD                    WRVLNA
119600040324     C                   KFLD                    WRVNRS
119700040324     C                   KFLD                    WRVNSC
119800070115     C* POSIZIONAMENTO FNBRV07L
119900970611     C     KBRV          KLIST
120000970611     C                   KFLD                    WRVLNP
120100970611     C                   KFLD                    WRVLNA
120200970611     C                   KFLD                    WRVNRS
120300970611     C                   KFLD                    WRVNSC
120400970611     C     KBRV7         KLIST
120500970611     C                   KFLD                    BRVLNP
120600970611     C                   KFLD                    BRVLNA
120700970611     C                   KFLD                    BRVNRS
120800970611     C                   KFLD                    BRVNSC
120900051007     C     Kdct          KLIST
121000051007     C                   KFLD                    dctaac
121100051007     C                   KFLD                    dctfil
121200051007     C                   KFLD                    dctnca
121300930521     C*
121400930519     C                   ENDSR
121500100618     c
121600100504     C*--- CALCOLO DATE DI PULIZIA SPUNTE ----------------------------*
121700100504     C     CALDAT        BEGSR
121800100504     C*
121900100504     C* GIORNI PULIZIA
122000100504     C                   MOVEL     §5ABRV        COMBRV
122100100504     C                   MOVEL     §5ABRC        COMBRC
122200100618     c* addiziono eventuali gg lavorativi aggiuntivi
122300100618     c                   add       §vpogglav     COMBRV
122400100618     c                   add       §vpogglav     COMBRC
122500100618     c
122600100618     C****
122700100618     C** GIORNI DATA CONSEGNA PER BOLLE CON   EVENTI ANOMALI
122800100618     C****
122900100618     c                   clear                   xgiolavds
123000100618     c                   eval      ixgldata=o61drf
123100100618     c                   eval      ixglfil =simfel
123200100618     c                   eval      ixglpa  ='A'
123300100618     c                   eval      ixglsub ='S'
123400100618     C* AGGIUNGO 1 AI GIORNI DI PULIZIA CONTENUTI IN TABELLA
123500100618     c                   eval      ixglgg  =combrv+1
123600100618     c                   eval      ixgllav ='S'
123700100618     c                   call      'XGIOLAV'
123800100618     c                   parm                    xgiolavds
123900100618     c                   Move      oxgldata      datbrv
124000100618
124100100618     C****
124200100618     C** GIORNI DATA CONSEGNA PER BOLLE SENZA EVENTI ANOMALI
124300100618     C****
124400100618     c                   clear                   xgiolavds
124500100618     c                   eval      ixgldata=o61drf
124600100618     c                   eval      ixglfil =simfel
124700100618     c                   eval      ixglpa  ='A'
124800100618     c                   eval      ixglsub ='S'
124900100618     C* AGGIUNGO 1 AI GIORNI DI PULIZIA CONTENUTI IN TABELLA
125000100618     c                   eval      ixglgg  =combrc+1
125100100618     c                   eval      ixgllav ='S'
125200100618     c                   call      'XGIOLAV'
125300100618     c                   parm                    xgiolavds
125400100618     c                   Move      oxgldata      datbrc
125500100618
125600100504     C                   ENDSR
