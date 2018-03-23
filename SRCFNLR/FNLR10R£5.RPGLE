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
009200100706     D                 DS
009300100706     D  blpaas                 1      4  0
009400100706     D  blpmgs                 5      8  0
009500100706     D  BLPSPE                 1      8  0
009600100706     D                 DS
009700100706     D  arbaas                 1      4  0
009800100706     D  arbmgs                 5      8  0
009900100706     D  ARBSPE                 1      8  0
010000960506     D*
010100100504     D WLBDAT          DS                  inz
010200100504     D  G02DAT                 1      8  0 inz
010300100504     D  G02INV                 9     16  0 inz
010400100504     D  G02ERR                17     17    inz
010500100504     D  G02TGI                18     22  0 inz
010600100504     D WGIDAT          DS                  inz
010700100504     D  GIODAT                 1      8  0 inz
010800100504     D  GIOINV                 9     16  0 inz
010900100504     D  GIOTGI                17     21  0 inz
011000020906
011100020906     D*
011200020906     D** DEFINIZIONE CAMPI
011300020906     D*
011400100504     d CODUT           s                   like(TBLkut) inz(1)
011500100504     d KEY             s                   like(TBLKEY) inz
011600020906     D COD             S                   LIKE(TBLCOD) inz
011700020906     D PUDCS           S                   LIKE(BRVDCS) inz
011800020906     D PULNP           S                   LIKE(BRVLNP) inz
011900020906     D WAAS            S                   LIKE(EVBAAS) inz
012000020906     D WDAT            S                   LIKE(DATSPP) inz
012100020906     D SAVFVV          S                   LIKE(WFVV)   inz
012200020906     D SAVBRC          S                   LIKE(DATBRC) inz
012300020906     D WMAX            S                   LIKE(TBLKEY) inz
012400020906     D SESAV           S                   LIKE(BRVSE)  inz
012500070821     D FVSAV           S                   LIKE(WFVV)   inz
012600020906     D XI              S              2  0
012700020906     D XX              S              3  0
012800070821     D WFILEERR        S              1    inz
012900070821     D WOK             S              1    inz
013000020906     D WGIR            S              1    inz
013100020906     D W0080           S              8  0 inz
013200020906     D WCABLP          S              1    inz
013300020906     D SAVse           S             15  0 inz
013400020925     D SAVpar          S             15  0 inz
013500020906     D SAVspe          S             16  0 inz
013600020906     D SAVsp2          S             16  0 inz
013700020906      * Flag "Trovata bolla"
013800020906     d $Found          s              1    inz(*off)
013900090416     d DataPuliz       s              8  0 inz
014000100504     d COMBRV          s                   like(§5ABRV) inz
014100100504     d COMBRC          s                   like(§5ABRC) inz
014200100504     d datbrv          s              8  0 inz
014300100504     d datbrc          s              8  0 inz
014400100504     d datSPP          s              8  0 inz
014500100504     d datSca          s              8  0 inz
014600100504     d datcan          s              8  0 inz
014700020906
014800960506     I*
014900990510     I** RIDENOMINAZIONE CAMPI FNANM
015000990510     IFNANM002
015100960430     I              ANMLNA                      BRVLNA
015200960430     I              ANMNRS                      BRVNRS
015300960430     I              ANMSCN                      BRVNSC
015400020906
015500941214     C*---------------------------------------------------------------*
015600941214     C* INDICATORI USATI                                              *
015700941214     C*---------------------------------------------------------------*
015800960506     C* 21    - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE OPPURE CON
015900960506     C*         TIPO ANOMALIA "T"
016000961106     C* 22    - ANOMALIA ESTERNA AUTOCHIUSA
016100020926     C* 31 33 - uso esclusivo della RICPAR
016200020926     C* 32 34 - uso esclusivo della RICARR
016300061023     C* 36    - controllo se brvcan ha anomalia x danni
016400020926     C* 30/39 - CHAIN SUI FILES
016500930519     C*---------------------------------------------------------------*
016600930519     C*
016700930519     C* DEFINIZIONE VARIABILI, KLIST E PLIST
016800930520     C                   EXSR      DEFVAR
016900100513     c*
017000100513   -1c                   if        datbrv>0 and datbrc>0 and datspp>0
017100930520     C*
017200930521     C* CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE
017300930521     C                   EXSR      CARTAB
017400950303     C*
017500970611     C                   CLEAR                   WBRVSE
017600970611     C*
017700960506     C* ESEGUO 2 VOLTE IL CICLO DEL PGM: -PER LA PULIZIA DELLE ANOMALIE
017800960506     C*                                  -PER LA PULIZIA DELLE SPUNTE
017900020906     C                   MOVEL     'A'           WGIR
018000960506     C*
018100960515    0C                   DO        2
018200950303     C                   CLEAR                   SAVSE
018300020925     C                   CLEAR                   SAVpar
018400970611     C                   CLEAR                   SESAV
018500950303     C                   CLEAR                   SAVSPE
018600950303     C                   CLEAR                   SAVSP2
018700950403     C                   CLEAR                   SAVFVV
018800070821     C                   CLEAR                   WfileERR
018900941214     C*
019000960430     C* LETTURA  A N O M A L I E
019100980217     C*  PER LE ANOMALIE PULIAMO TUTTO CON LA DATA PIU' ALTA
019200960430    1C     WGIR          IFEQ      'A'
019300960515     C                   MOVEL     DATBRC        SAVBRC
019400960515     C                   MOVEL     DATBRV        DATBRC
019500960515     C*
019600990510     C     *LOVAL        SETLL     FNANM002
019700990510     C                   READ      FNANM002                             9930
019800990510     C   99              READ(N)   FNANM002                               30
019900960515     C*
020000960430   X1C                   ELSE
020100960515     C                   MOVEL     SAVBRC        DATBRC
020200960430     C* LETTURA  S P U N T E
020300070115     C     *LOVAL        SETLL     FNBRV007
020400070115     C                   READ      FNBRV007                             9930
020500070115     C   99              READ(N)   FNBRV007                               30
020600960430    1C                   ENDIF
020700941214     C*
020800941214    1C     *IN30         DOWEQ     *OFF
020900960730     C*
021000960730     C* SOLO SE SONO RIUSCITO AD ALLOCARE IL RECORD
021100960730   1AC     *IN99         IFEQ      *OFF
021200960430     C*
021300990510     C* SOLO PER  A N O M A L I E :
021400960506    2C     WGIR          IFEQ      'A'
021500960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
021600960903     C*                           CANCELLARE L'ANOMALIA
021700980217    3C     ANMDAO        IFEQ      0
021800990510     C                   Z-ADD     19000101      PUDCS
021900980217   X3C                   ELSE
022000990510     C                   Z-ADD     ANMDAO        PUDCS
022100980217    3C                   ENDIF
022200980217     C**
022300980217   X2C                   ELSE
022400021105      * Prendo la data minore tra caricamento e immissione se tutte e 2 sono
022500021105      * maggiori di udate
022600070821     c                   EXSR      CerDATbrv
022700070821     c
022800980217    2C                   ENDIF
022900960430     C*
023000980217     C* SE LA DATA APERTURA ANOMALIA O
023100980217     C*    LA DATA DI CARICAMENTO SPUNTA E' > DELLA DATA PIU' RECENTE
023200950403     C*  DI PULIZIA --> NON LA CONSIDERO NEMMENO
023300980217    2C     PUDCS         IFLE      WDAT
023400020906     C                   MOVEL     'S'           WOK
023500941214     C*
023600960430     C* SOLO PER  A N O M A L I E
023700960430    3C     WGIR          IFEQ      'A'
023800960506     C*
023900960506     C* 21 ON  - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE
024000960506     C     ANMCAA        LOOKUP    SAN                                    21
024100960506     C*
024200960430     C* LE ANOMALIE "E/R" SI PULISCONO SOLO SE HANNO LA DATA CHIUSURA
024300960506    4C  N21ANMAIE        IFEQ      'E'
024400960430     C     ANMAIE        OREQ      'R'
024500961106     C                   SETOFF                                       22
024600961106     C*
024700961106     C* 22 ON  - SI TRATTA DI ANOMALIA ESTERNA AUTOCHIUSA
024800961106    5C     ANMAIE        IFEQ      'E'
024900961106     C     ANMCAA        ANDNE     70
025000961106     C     ANMCAA        ANDNE     71
025100961106     C     ANMCAA        LOOKUP    CHA                                    22
025200961106    5C                   ENDIF
025300961106     C*
025400961106    5C     ANMAIE        IFEQ      'E'
025500961106     C     *IN22         ANDEQ     *OFF
025600980217     C**
025700980217     C* VERIFICO SE LNA FA O MENO L'IDD
025800020906     c                   eval      XX = 1
025900050805     c**   BRVlna        lookup    SKpo(xx)                               37
026000050805     c** 37              movel     NIDD(xx)      LNANID
026100050805     c**N37              movel     'N'           LNANID
026200980217     C**
026300050805    6C**   LNANID        IFNE      'N'
026400980217    7C     ANMDCH        IFEQ      0
026500980217     C     ANMFT1        OREQ      ' '
026600980217     C     ANMFL1        ANDGT     0
026700980217     C     ANMFT2        OREQ      ' '
026800980217     C     ANMFL2        ANDGT     0
026900980217     C                   CLEAR                   WOK
027000980217    7C                   ENDIF
027100050805    6C**                 ENDIF
027200050805    5C                   ENDIF
027300961106     C*
027400960430   X4C                   ELSE
027500960506     C* 21 ON  - ANOMALIA CON TIPO ANOMALIA "T"
027600960506     C     ANMAIE        IFEQ      'T'
027700960506     C                   SETON                                        21
027800960430     C                   ENDIF
027900960430    4C                   ENDIF
028000960430    3C                   ENDIF
028100960430     C*
028200960430     C* WOK = "S" ---> RECORD DA ELABORARE
028300960506    3C     WOK           IFEQ      'S'
028400960430     C*
028500960506    4C     *IN21         IFEQ      *OFF
028600960530     C*
028700960506    5C     WGIR          IFEQ      'A'
028800960506     C     ANMLNP        IFGT      0
028900960506     C                   MOVEL     ANMLNP        BRVLNP
029000960506     C                   ELSE
029100960506     C                   MOVEL     ANMFLS        BRVLNP
029200960506     C                   ENDIF
029300960506    5C                   ENDIF
029400960530     C*
029500970611     C*
029600970611     C* A CAMBIO SEGNACOLLO:
029700970611     C     WGIR          IFEQ      'S'
029800970611     C     BRVSE         ANDNE     SESAV
029900970611     C* Verifico se per il collo precedente devo cancellare gli eventi
030000970611     C     WBRVSE        IFGT      *ZEROS
030100970611     C                   EXSR      DELEVS
030200970611     C                   CLEAR                   WBRVSE
030300970611     C                   END
030400970611     C                   MOVE      BRVSE         SESAV
030500970611     C                   END
030600941214     C*
030700020906     c                   eval      $Found = *off
030800941214     C* CERCO NELLE BOLLE IN ARRIVO
030900941214     C                   EXSR      RICARR
031000020906     c*
031100020906     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
031200020906     c                   if        $Found = *off
031300020906     c                   exsr      RICPAR
031400020906     c                   endif
031500020925     c                   else
031600020925     c* se anomalia senza spedizione o di tipo "T" eseguo solo il ciclo
031700020925     c*  NO BOLLE
031800020925     c                   eval      $Found = *off
031900020925    4c                   endif
032000020906     C*
032100020906     c                   if        $Found = *off
032200020906     C     WGIR          IFEQ      'A'
032300020906     C                   EXSR      NOBOL2
032400020906     C                   ELSE
032500020906     C                   EXSR      NOBOLL
032600020906     C                   ENDIF
032700020906     c                   endif
032800950403     C*
032900960506    3C                   ENDIF
033000960506    2C                   ENDIF
033100960730     C*
033200960730   1AC                   ENDIF
033300941214     C*
033400960506    2C     WGIR          IFEQ      'A'                                          *ANOMALIE
033500990510     C                   READ      FNANM002                             9930
033600990510     C   99              READ(N)   FNANM002                               30
033700960730   X2C                   ELSE                                                   *SPUNTE
033800960730     C*
033900070115     C                   READ      FNBRV007                             9930
034000070115     C   99              READ(N)   FNBRV007                               30
034100960506    2C                   ENDIF
034200960506    1C                   ENDDO
034300941214     C*
034400960506     C* IMPOSTO CAMPI PER PULIZIA SPUNTE
034500960506     C                   MOVEL     'S'           WGIR
034600960506     C                   SETOFF                                       21
034700960506    0C                   ENDDO
034800970611     C*
034900070821     C* Verifico se x l'ultimo collo devo cancellare gli eventi
035000970611     C     WBRVSE        IFGT      *ZEROS
035100970611     C                   EXSR      DELEVS
035200970611     C                   CLEAR                   WBRVSE
035300970611     C                   END
035400980210     C****
035500980210     C* PULIZIA SPUNTE ERRATE SEGNACOLLI
035600980210     C****
035700070821     c                   eval      wfileerr='S'
035800070115     C                   OPEN      FNBRVE1L
035900070115     C     *LOVAL        SETLL     FNBRVE1L
036000070115     C                   READ      FNBRVE1L                               30
036100980210    1C     *IN30         DOWEQ     *OFF
036200980210     C* Prendo la data foglio a cambio di numero
036300070821     c                   if        wfvv<>FVSAV
036400020808     C                   CLEAR                   FNLV53DS
036500070821     C                   Z-ADD     BRvNPG        D53NPG
036600070821     C                   Z-ADD     BRvNFV        D53NFV
036700070821     C                   Z-ADD     BRvFGS        D53FGS
036800980210     C                   CALL      'FNLV53R'
036900020808     C                   PARM                    FNLV53DS
037000070821     C                   MOVE      wfvv          FVSAV
037100090416     c* se errore, imposto data caricammento spunte in data foglio
037200090416     c                   if        d53err<>' '
037300090416     c                   movel     PUDCS         D53DFV
037400090416     c                   clear                   FVSAV
037500090416     c                   endif
037600090416     c
037700980210    2C                   ENDIF
037800070821     c
037900070821    2C     D53DFV        IFLE      DATSPP
038000070821     c
038100070821     c* Per categorie 4 / 8 / 1 partenza, deleto
038200070821    3c                   if        brvnpg=4 or brvnpg = 8 or
038300070821     c                             (brvnpg=1 and brvftr='L')
038400070115     C                   DELETE    FNBRVE
038500070821   x3c                   else
038600070821     c
038700070823     c* Per le altre categorie mi legGo alla bolla
038800070823     c                   EXSR      CerDATbrv
038900070823     C
039000070821     c* Per le altre categorie mi lego alla bolla
039100070821     c                   eval      $Found = *off
039200070821     C* CERCO NELLE BOLLE IN ARRIVO
039300070821     C                   EXSR      RICARR
039400070821     c*
039500070821     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
039600070821    4c                   if        $Found = *off
039700070821     c                   exsr      RICPAR
039800070823     C                   ENDIF
039900070823     C
040000070823    4c                   if        $Found = *off
040100070821     C                   DELETE    FNBRVE
040200070821    4c                   endif
040300070821    3c                   endif
040400070821     c
040500070821    2C                   END
040600070115     C                   READ      FNBRVE1L                               30
040700980210    1C                   ENDDO
040800100513     c
040900100513   -1c                   endif
041000040806     C*
041100040806     C                   MOVEL     'C'           I12TLA
041200040806     C                   CALL      'FIDN12R'
041300040806     C                   PARM                    FIDN12DS
041400040806      *
041500020906     C                   MOVEL     *ON           *INLR
041600941214     C*
041700941214     C*---------------------------------------------------------------*
041800941214     C*                    RICPAR                                     *
041900941214     C* CERCO NELLE BOLLE IN PARTENZA                                 *
042000020926     c* indicatori 31 e 33 ad uso esclusivo della routine ricpar
042100941214     C*---------------------------------------------------------------*
042200941214     C     RICPAR        BEGSR
042300020926     c* Se anomlie senza segnacollo ma solo con la spedizione (70 e 71)
042400020926     c*  devo comunque entrare nell'if per chainare fnarb
042500020926     c                   if        wgir='A' and brvnsc=0
042600020926     c                   eval      savpar=*hival
042700020926     c                   endif
042800950303     C*
042900941214     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
043000020925    0C     BRVSE         IFNE      SAVpar
043100950303     C*
043200960508    1C     WGIR          IFEQ      'S'
043300960508     C     WGIR          OREQ      'A'
043400960508     C     ANMNSP        ANDEQ     0
043500941214     C     KBLT          CHAIN     FNBLT000                           31
043600960508     C*
043700960508   X1C                   ELSE
043800960514     C                   SETOFF                                       31
043900960508     C                   Z-ADD     ANMAAS        BLTAAS
044000960514     C                   MOVEL     ANMLNP        BLTLNP
044100960508     C                   MOVEL     BRVNRS        BLTNRS
044200960508     C                   MOVEL     ANMNSP        BLTNSP
044300960508    1C                   ENDIF
044400941214     C*
044500941214     C* AGGANCIO LA BOLLA
044600950303    1C  N31BLTSPE        IFNE      SAVSPE
044700950303     C     KBLP          CHAIN     FNBLP000                           31
044800990126     C* SE C'E' UNA C.A. NON CANCELLO LE SPUNTE
044900051006     c* 07 ott 05 --> addesso puliamo se c.a. in gestione alla sede
045000051006     c*               passati xxx giorni dall'apertura c.a.
045100040806      *
045200051007    1c                   If        not *in31
045300040806     c                   clear                   fidn12ds
045400040806     c                   eval      i12aas = bltaas
045500040806     c                   eval      i12lnp = bltlnp
045600040806     c                   eval      i12nrs = bltnrs
045700040806     c                   eval      i12nsp = bltnsp
045800040806     c                   eval      i12fel = 'E'
045900040806     c                   eval      i12fan = 'E'
046000040806      *
046100051007     c                   EXSR      CERCA
046200040806      *
046300950303    1C                   ENDIF
046400051007    1C                   ENDIF
046500950303     C*
046600020925     C                   MOVEL     BRVSE         SAVpar
046700950303    0C                   ENDIF
046800950120     C*
046900950120    1C     *IN31         IFEQ      *OFF
047000020906     c*
047100020906     c                   eval      $Found = *on
047200941214     C*
047300051007     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA e senza c.a.
047400051007     c
047500950303    2C     BLPDCM        IFGT      0
047600990126     C     WCABLP        ANDEQ     ' '
047700100706     c
047800100706     C* Elaboro anche se non consegnata senza C.A. solo SPUNTE
047900100706    2C     BLPDCM        oreq      0
048000100706     C     WCABLP        ANDEQ     ' '
048100100706     C     WGIR          ANDEQ     'S'
048200100706     c
048300051007     c*   o con data c.a. < data gg pulizia bolle con c.a.
048400100706     c*   in questo caso pulisco sempre, anche se non è consegnata
048500100706    2c     wcablp        oreq      'O'
048600950303     C*
048700100706     c                   select
048800960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
048900960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
049000100706   2AC     PUDCS         whengt    BLPDCM
049100051007     c     blpdcm        andgt     0
049200960509     C*
049300090416     c                   select
049400090416     c                   when      wfileErr='S'
049500090416     c                   if        d53dfv<=datcan
049600090416     c                   delete    fnbrve
049700090416     c                   endif
049800090416     c                   when      WGIR='A'
049900960509     C                   EXSR      NOBOL2
050000090416     C                   other
050100960509     C                   EXSR      NOBOLL
050200090416     C                   ENDSL
050300090416     c*
050400090416     c* se spunta con "*" controlo direttamente la data consegna
050500090416     c*  con la data pulizia spunte con C.A.
050600100706   2ac                   when      brvnvr='*'  and blpdcm>0
050700090416     c
050800090416    3c                   if        blpdcm<=datcan
050900090416     c                   exsr      BRVDEL
051000090416    3c                   endif
051100100706     c
051200100706     c* Bolla NON consegnata --> tengo almeno un anno la spunta poi cancello
051300100706   2ac                   when      blpdcm= 0  and wcablp=' '
051400100706    6c                   if        blpSPE<=datSCA and PUDCS<=datSCA
051500100706     c                   EXSR      BRVDEL
051600100706     c                   endif
051700090416     c
051800100706  x2ac                   other
051900960509     C*
052000100706     c* bolla consegnata
052100950303    3C     BLTSPE        IFNE      SAVSPE
052200941214     C*
052300941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
052400941214     C*                  - DATA CONSEGNA PARZIALE
052500950303    4C     BLPCCA        IFEQ      *BLANKS
052600941214     C     BLPDCP        ANDEQ     *ZEROS
052700950120     C                   SETOFF                                       33
052800950303   X4C                   ELSE
052900950120     C                   SETON                                        33
053000950303    4C                   ENDIF
053100941214     C*
053200981008     C* NON HA EVENTI (O SOLO QUELLI FASULLI)
053300090416    4C     *IN33         IFEQ      *OFF
053400981008     C                   Z-ADD     BLTAAS        WAAS
053500990804     C     KEVB          SETLL     FNEVB000
053600990804     C     KEVB          READE     FNEVB000                               98
053700090416    5C     *IN98         DOWEQ     *OFF
053800981008     C     EVBCEV        LOOKUP    EFS                                    97
053900981008     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
054000090416    6C     *IN97         IFEQ      *OFF
054100981008     C                   SETON                                        9833
054200981008     C                   ELSE
054300981008     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
054400990804     C     KEVB          READE     FNEVB000                               98
054500090416    6C                   ENDIF
054600090416    5C                   ENDDO
054700090416    4C                   ENDIF
054800941214     C*
054900941214     C* NON HA GIACENZA
055000050331     C  N33KBLP          SETLL     Tigcp01l                               33
055100941214     C*
055200941214     C* NON HA LEGAMI BOLLA (MAMMA)
055300950120     C  N33KBLP          SETLL     FNLBL000                               33
055400941214     C*
055500941214     C* NON HA LEGAMI BOLLA (FIGLIA)
055600950120     C  N33KBLP          SETLL     FNLBL002                               33
055700950303     C*
055800020906     C                   MOVEL     BLTSPE        SAVSPE
055900950303    3C                   ENDIF
056000090416     c
056100090416     c* Imposto la data di pulizia :
056200090416    3c                   select
056300090416     c* 1) Spunte nel file degli errori
056400090416     c                   when      wfileErr='S'
056500090416     c                   eval      datapuliz=DATcan
056600090416     C* 2) 33 OFF -    BOLLA SENZA EVENTI ANOMALI
056700090416     c                   when      *in33 =*off
056800090416     c                   eval      datapuliz=DATBRC
056900090416     c                   other
057000090416     C* 2)  altrimenti BOLLA CON   EVENTI ANOMALI
057100090416     c                   eval      datapuliz=DATBRV
057200090416    3c                   ENDSL
057300950303     C*
057400941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
057500090416    3C     BLPDCM        IFLE      DataPuliz
057600051007     c     blpdcm        andgt     0
057700051007     c* pulisco sempre se non vonsegna con c.a. perchè pulisco in base
057800051007     c*  alla data pulizia spunte con c.a.
057900051007     c     blpdcm        oreq      0
058000051007     c     wcablp        andeq     'O'
058100051007     c
058200090416    4C     WGIR          IFEQ      'A'
058300990510     C                   DELETE    FNANM002
058400090416   X4C                   ELSE
058500061019     c
058600090416     c* Se ha anomalia di spunta x DANNI, deleto solo se data <=data pulizia
058700061019     c*  spunte con anomalia
058800061023     c*
058900061023     c                   setoff                                       36
059000090416    5c                   if        brvcan<>' '
059100061023     c     brvcan        lookup    anom                                   36
059200090416    5c                   endif
059300061023     c
059400090416    5c                   if        not *in36  or wcablp='O'
059500070821     c                   EXSR      BRVDEL
059600090416   x5c                   else
059700061019     c
059800090416    6c                   if        blpdcm<=datcan
059900070821     c                   EXSR      BRVDEL
060000090416   x6c                   else
060100061019     c* altrimenti flaggo la spunta come non valida ai fini
060200061019     c*  del calcolo della responsabilità
060300090416    7c                   if        wfileerr=' '
060400070115     c                   eval      brvnvr='*'
060500070115     c                   update    fNbrv007
060600090416    7c                   endif
060700070821     c
060800090416    6c                   endif
060900090416    5c                   endif
061000090416    4C                   ENDIF
061100090416    3C                   ENDIF
061200100706   2AC                   endsl
061300941214     C*
061400950303   X2C                   ELSE
061500020906     C                   MOVEL     BLTSPE        SAVSPE
061600950120    2C                   ENDIF
061700941214     C*
061800950120    1C                   ENDIF
061900941214     C*
062000941214     C                   ENDSR
062100941214     C*
062200941214     C*---------------------------------------------------------------*
062300051007     C* cerco se bolla con c.a.
062400051007     C*---------------------------------------------------------------*
062500051007     c     CERCA         BEGSR
062600090417     C                   CLEAR                   WCABLP
062700051007     c                   call      'FIDN12R'
062800051007     c                   parm                    fidn12ds
062900051007      *
063000051007      * se non ci sono errori
063100051007    2c                   if        o12err = ' '
063200051007      * se numero di CA trovate maggiore di zero
063300051007    3c                   if        o12nca > 0
063400051007     c                   z-add     1             ii                3 0
063500051007    4c                   dow       skdca(II)>0 and wcablp=' '
063600051007    5c                   if        skdca(II)>datsca
063700051007     c                   movel     'S'           Wcablp
063800051007    5c                   endif
063900051007     c                   add       1             ii
064000051007    4c                   enddo
064100051007     c
064200051007     c* Se tutte le c.a. con data apertura < chain su dtc per controllare
064300070822     c*  che siano tutte LE APERTE in fase gestione SEDE
064400051007    4c                   if        wcablp=' '
064500051007     c                   z-add     1             ii
064600051007    5c                   dow       skdca(II)>0  and wcablp=' '
064700051007     c                   movel     skkey(II)     dskey
064800051007     c     kdct          chain     fndct01l
064900070822    6c                   if        %found(fndct01l) and dctgfc<>46  and
065000070822     c                             dctdch=0
065100051007     c                   movel     'S'           Wcablp
065200051007    6c                   endif
065300051007     c                   add       1             ii
065400051007    5c                   enddo
065500051007     c
065600051007     c                   if        wcablp=' '
065700051007     c                   eval      wcablp='O'
065800051007     c                   endif
065900051007     c
066000051007    4c                   endif
066100051007      *
066200051007    3c                   endif
066300051007      *
066400051007    2c                   endif
066500051007     c                   ENDSR
066600941214     C*---------------------------------------------------------------*
066700941214     C*                    RICARR                                     *
066800941214     C* CERCO NELLE BOLLE IN ARRIVO                                   *
066900020926     c* indicatori 32 e 34 ad uso esclusivo della routine ricarr
067000941214     C*---------------------------------------------------------------*
067100941214     C     RICARR        BEGSR
067200051007     c* Se anomalie  senza segnac.  ma solo con la spedizione (70 e 71)
067300020926     c*  devo comunque entrare nell'if per chainare fnarb
067400020926     c                   if        wgir='A' and brvnsc=0
067500020926     c                   eval      savse=*hival
067600020926     c                   endif
067700950303     C*
067800950303     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
067900950303    0C     BRVSE         IFNE      SAVSE
068000960508     C*
068100960508    1C     WGIR          IFEQ      'S'
068200960508     C     WGIR          OREQ      'A'
068300960508     C     ANMNSP        ANDEQ     0
068400950303     C     KBLT          CHAIN     FNART000                           32
068500960508     C*
068600960508   X1C                   ELSE
068700960514     C                   SETOFF                                       32
068800960508     C                   Z-ADD     ANMAAS        ARTAAS
068900960514     C                   MOVEL     ANMLNP        ARTLNP
069000960508     C                   MOVEL     BRVNRS        ARTNRS
069100960508     C                   MOVEL     ANMNSP        ARTNSP
069200960508    1C                   ENDIF
069300941214     C*
069400941214     C* AGGANCIO LA BOLLA
069500990126    1C  N32ARTSPE        IFNE      SAVSP2
069600950303     C     KARB          CHAIN     FNARB000                           32
069700051007     C* SE C'E' UNA C.A. cancello dalla data apertura c.a.
069800040806      *
069900090416    2c                   If        not *in32
070000040806     c                   clear                   fidn12ds
070100040806     c                   eval      i12aas = artaas
070200040806     c                   eval      i12lnp = artlnp
070300040806     c                   eval      i12nrs = artnrs
070400040806     c                   eval      i12nsp = artnsp
070500040806     c                   eval      i12fel = 'E'
070600040806     c                   eval      i12fan = 'E'
070700051007     c
070800051007     c                   EXSR      CERCA
070900040806      *
071000090416    2c                   endif
071100950303    1C                   ENDIF
071200950303     C*
071300950303     C                   MOVEL     BRVSE         SAVSE
071400950303    0C                   ENDIF
071500950120     C*
071600950303    1C     *IN32         IFEQ      *OFF
071700020906     c*
071800020906     c                   eval      $Found = *on
071900941214     C*
072000941214     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA
072100990126     C* E SE NON CI SONO C.A.
072200950120    2C     ARBDCM        IFGT      0
072300051007     C     WCAblp        ANDEQ     ' '
072400100706     c
072500100706     C* Elaboro anche se non consegnata senza C.A. ssolo spunte
072600100706    2C     ARBDCM        oreq      0
072700100706     C     WCABLP        ANDEQ     ' '
072800100706     C     WGIR          ANDEQ     'S'
072900100706     c
073000051007     c*  opure se ha c.a. con data < alla data di pulizia. cancello
073100051007     c*  anche le non consegnate
073200100706    2C     WCAblp        oreq      'O'
073300960509     C*
073400100706     c                   select
073500960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
073600960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
073700061018     c*   se non ha c.a., altrimenti seguo la pulizia per data c.a.
073800100706   2AC     PUDCS         whengt    ARBDCM
073900051007     c     arbdcm        andgt     0
074000061018     C     WCAblp        ANDEQ     ' '
074100960509     C*
074200090416     c                   select
074300090416     c                   when      wfileErr='S'
074400090416     c                   if        d53dfv<=datcan
074500090416     c                   delete    fnbrve
074600090416     c                   endif
074700090416     c                   when      WGIR='A'
074800090416     C                   EXSR      NOBOL2
074900090416     C                   other
075000090416     C                   EXSR      NOBOLL
075100090416     C                   ENDSL
075200090416     c*
075300090416     c* se spunta con "*" controllo direttamente la data consegna
075400090416     c*  con la data pulizia spunte con C.A.
075500100706   2ac                   when      brvnvr='*'   and arbdcm>0
075600090416     c
075700090416    3c                   if        arbdcm<=datcan
075800090416     c                   exsr      BRVDEL
075900090416    3c                   endif
076000090416     c
076100100706     c
076200100706     c* Bolla NON consegnata --> tengo almeno un anno la spunta poi cancello
076300100706   2ac                   when      arbdcm= 0  and wcablp=' '
076400100706    3c                   if        arbSPE<=datSCA  and PUDCS<=datSCA
076500100706     c                   EXSR      BRVDEL
076600100706    3c                   endif
076700100706     c
076800100706  x2ac                   other
076900090416     C*
077000950303    3C     ARTSPE        IFNE      SAVSP2
077100941214     C*
077200941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
077300941214     C*                  - DATA CONSEGNA PARZIALE
077400950303    4C     ARBCCA        IFEQ      *BLANKS
077500941214     C     ARBDCP        ANDEQ     *ZEROS
077600950303     C                   SETOFF                                       34
077700950303   X4C                   ELSE
077800950303     C                   SETON                                        34
077900950303    4C                   ENDIF
078000941214     C*
078100000614     C* NON HA EVENTI O SOLO QUELLI FASULLI
078200950303     C  N34              Z-ADD     ARTAAS        WAAS
078300000614     C     *IN34         IFEQ      *OFF
078400000614     C     KEVB2         SETLL     FNEVB000
078500000614     C     KEVB2         READE     FNEVB000                               98
078600000614     C     *IN98         DOWEQ     *OFF
078700000614     C     EVBCEV        LOOKUP    EFS                                    97
078800000614     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
078900000614     C     *IN97         IFEQ      *OFF
079000000614     C                   SETON                                        9834
079100000614     C                   ELSE
079200000614     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
079300000614     C     KEVB2         READE     FNEVB000                               98
079400000614     C                   ENDIF
079500000614     C                   ENDDO
079600000614     C                   ENDIF
079700941214     C*
079800941214     C* NON HA GIACENZA
079900050331     C  N34KARB          SETLL     Tigcp21l                               34
080000941214     C*
080100941214     C* NON HA LEGAMI BOLLA (MAMMA)
080200950303     C  N34KARB          SETLL     FNLBL000                               34
080300941214     C*
080400941214     C* NON HA LEGAMI BOLLA (FIGLIA)
080500950303     C  N34KARB          SETLL     FNLBL002                               34
080600950303     C*
080700020906     C                   MOVEL     ARTSPE        SAVSP2
080800950303    3C                   ENDIF
080900090416     c
081000090416     c* Imposto la data di pulizia :
081100090416    3c                   select
081200090416     c* 1) Spunte nel file degli errori
081300090416     c                   when      wfileErr='S'
081400090416     c                   eval      datapuliz=DATcan
081500090416     C* 2) 34 OFF -    BOLLA SENZA EVENTI ANOMALI
081600090416     c                   when      *in34 =*off
081700090416     c                   eval      datapuliz=DATBRC
081800090416     c                   other
081900090416     C* 2)  altrimenti BOLLA CON   EVENTI ANOMALI
082000090416     c                   eval      datapuliz=DATBRV
082100090416    3c                   ENDSL
082200950120     C*
082300941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
082400090416    3C     ARBDCM        IFLE      Datapuliz
082500051007     c     arbdcm        andgt     0
082600051007     c
082700051007     c     arbdcm        oreq      0
082800051007     c     wcablp        andeq     'O'
082900051007     c
083000090416    4C     WGIR          IFEQ      'A'
083100990510     C                   DELETE    FNANM002
083200090416   x4C                   ELSE
083300061023     c* Se ha anomalia di spunta x danni, deleto solo se data <=data pulizia
083400061019     c*  spunte con anomalia
083500061023     c*
083600061023     c                   setoff                                       36
083700090416    5c                   if        brvcan<>' '
083800061023     c     brvcan        lookup    anom                                   36
083900090416    5c                   endif
084000061023     c
084100090416    5c                   if        not *in36   or Wcablp='O'
084200070821     c                   EXSR      BRVDEL
084300090416   x5c                   else
084400061019     c
084500090416    6c                   if        arbdcm<=datcan
084600070821     c                   EXSR      BRVDEL
084700090416   x6c                   else
084800061019     c* altrimenti flaggo la spunta come non valida ai fini
084900061019     c*  del calcolo della responsabilità
085000090416    7c                   if        wfileerr=' '
085100070115     c                   eval      brvnvr='*'
085200070115     c                   update    fnbrv007
085300090416    7c                   endif
085400070821     c
085500090416    6c                   endif
085600090416    5c                   endif
085700980114     C**
085800090416    4c                   ENDIF
085900090416    3C                   ENDIF
086000100706   2AC                   endsl
086100950120     C*
086200090416   x2C                   ELSE
086300020906     C                   MOVEL     ARTSPE        SAVSP2
086400950120    2C                   ENDIF
086500941214     C*
086600950120    1C                   ENDIF
086700941214     C*
086800941214     C                   ENDSR
086900941214     C*---------------------------------------------------------------*
087000941214     C*                    NOBOLL                                     *
087100941214     C* CANCELLAZIONE SPUNTE SENZA BOLLA                              *
087200941214     C*---------------------------------------------------------------*
087300941214     C     NOBOLL        BEGSR
087400051007     c
087500051007    1C     wfvv          IFNE      savfvv
087600051007     C                   CLEAR                   FNLV53DS
087700051007     C                   Z-ADD     BRvNPG        D53NPG
087800051007     C                   Z-ADD     BRvNFV        D53NFV
087900051007     C                   Z-ADD     BRvFGS        D53FGS
088000051007     C                   CALL      'FNLV53R'
088100051007     C                   PARM                    FNLV53DS
088200051007     C                   MOVE      wfvv          savfvv
088300051007     c                   movel     d53err        werr              1
088400051007     c                   movel     d53dfv        wDFV              8 0
088500051007    1C                   ENDIF
088600061023     c
088700061023     c                   setoff                                       36
088800061023     c                   if        brvcan<>' '
088900061023     c     brvcan        lookup    anom                                   36
089000061023     c                   endif
089100051007     c
089200051007    1c                   if        Werr=' '
089300061019     c*
089400051007    2C     Wdfv          IFLE      DATSPP
089500061019     c
089600061019     C                   MOVE      BRVSE         WBRVSE
089700061023    3c                   if        not *in36
089800070821     c                   EXSR      BRVDEL
089900061019   x3c                   else
090000061019    4c                   if        wdfv<=datcan
090100070821     c                   EXSR      BRVDEL
090200061019   x4c                   else
090300070821    5c                   if        wfileerr=' '
090400070115     c                   eval      brvnvr='*'
090500070115     c                   update    fnbrv007
090600070821    5c                   endif
090700070821     c
090800070821    4c                   endif
090900061019    3c                   endif
091000061019     c
091100051007    2C                   ENDIF
091200051007     C*
091300051007     c                   else
091400051007     c* se non trovato foglio pulisco in base alla data spunta
091500061019     c
091600061019    2C     PUDCS         IFLE      DATSPP
091700061019     C                   MOVE      BRVSE         WBRVSE
091800061019     c
091900061023    3c                   if        not *in36
092000070821     C****               DELETE    FNBRV007
092100070821     c                   EXSR      BRVDEL
092200061019   x3c                   else
092300061019    4c                   if        pudcs<=datcan
092400070821     C****               DELETE    FNBRV007
092500070821     c                   EXSR      BRVDEL
092600061019   x4c                   else
092700070821    5c                   if        wfileerr=' '
092800070115     c                   eval      brvnvr='*'
092900070115     c                   update    fnbrv007
093000070821    5c                   endif
093100070821     c
093200070821    4c                   endif
093300061019    3c                   endif
093400061019     c
093500051007    2C                   ENDIF
093600051007    1C                   ENDIF
093700941214     C*
093800941214     C                   ENDSR
093900970611     C*---------------------------------------------------------------*
094000970611     C* CANCELLAZIONE EVENTI SEGNACOLLI                               *
094100970611     C*---------------------------------------------------------------*
094200970611     C     DELEVS        BEGSR
094300970611     C* VERIFICO SE PER IL SEGNACOLLO ESISTONO ANCORA DELLE SPUNTE E
094400970611     C* SE E' PRESENTE ALMENO UNA SPUNTA NON CANCELLO GLI EVENTI
094500070115     C     KBRV          SETLL     FNBRV07L                               35
094600970611     C     *IN35         IFEQ      *OFF
094700970611     C     *IN30         DOUEQ     *ON
094800970611     C     KEVS          DELETE    FNEVS000                           30
094900970611     C                   ENDDO
095000970611     C                   END
095100970611     C* MI RIPOSIZIONO SULLA SPUNTA CHE STAVO LEGGENDO
095200070115     C     KBRV7         SETLL     FNBRV07L
095300070115     C     KBRV7         READE     FNBRV07L                               30
095400970611     C                   ENDSR
095500960506     C*
095600960506     C*---------------------------------------------------------------*
095700960506     C*                    NOBOL2                                     *
095800960506     C* CANCELLAZIONE:  - ANOMALIE SENZA BOLLA                        *
095900960506     C*                 - ANOMALIE SENZA SEGNACOLLO E SPEDIZIONE      *
096000960506     C*                 - ANOMALIE CON TIPO ANOMALIA "T"              *
096100960506     C*---------------------------------------------------------------*
096200960506     C     NOBOL2        BEGSR
096300960506     C* PRENDO LA MAGGIORE TRA DATA APERTURA E DATA CHIUSURA
096400960506     C     ANMDCH        IFGT      ANMDAO
096500020906     C                   Z-ADD     ANMDCH        W0080
096600960506     C                   ELSE
096700990510     C                   Z-ADD     ANMDAO        W0080
096800960506     C                   ENDIF
096900960506     C*
097000960508     C*
097100960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
097200960903     C*                           CANCELLARE L'ANOMALIA
097300960903     C     ANMDAO        IFEQ      0
097400990510     C                   Z-ADD     19000101      W0080
097500960903     C                   ENDIF
097600960903     C*
097700990510    1C     W0080         IFLE      DATSPP
097800990510     C                   DELETE    FNANM002
097900960506    1C                   ENDIF
098000960506     C*
098100960506     C                   ENDSR
098200960506     C*
098300070821     C*---------------------------------------------------------------*
098400070821     c* Cancella record di FNBRV o FNBRVE
098500070821     C*---------------------------------------------------------------*
098600070821     c     BRVDEL        BEGSR
098700070821     c                   if        wfileerr=' '
098800070821     C                   DELETE    FNBRV007
098900070821     c                   else
099000070821     c                   delete    FNBRVE
099100070821     c                   endif
099200070821     C                   ENDSR
099300930519     C*---------------------------------------------------------------*
099400930521     C*                    CARTAB                                     *
099500930521     C* ROUTINE DI CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE  *
099600930519     C*---------------------------------------------------------------*
099700930521     C     CARTAB        BEGSR
099800981008     C***
099900981008     C* EVENTI CHE SONO FASULLI DA NON CONSIDERARE PER LA PULIZIA
100000981008     C***
100100981008     C                   MOVEL     '2Z'          COD
100200020906     C                   Z-ADD     0             XI
100300981008     C     KTAB          SETLL     TABEL
100400981008     C     KTAB          READE     TABEL                                  31
100500981008     C*
100600981008DO  1C     *IN31         DOWEQ     '0'
100700981008IF  2C     TBLFLG        IFEQ      ' '
100800981008     C                   MOVEL     TBLUNI        DS2Z
100900981008     C*
101000981008IF  3C     §2ZEFS        IFEQ      'S'
101100020906     C                   ADD       1             XI
101200020906     C                   MOVEL     TBLKEY        EFS(XI)
101300981008E   3C                   ENDIF
101400981008E   2C                   ENDIF
101500981008     C*
101600981008     C     KTAB          READE     TABEL                                  31
101700981008E   1C                   ENDDO
101800930111     C*
101900960506     C***
102000960506     C* ANOMALIE  IDD
102100960506     C***
102200960506     C                   MOVEL     '7C'          COD
102300020906     C                   Z-ADD     0             XI
102400020906     C                   Z-ADD     0             XX
102500960506     C     KTAB          SETLL     TABEL
102600960506     C     KTAB          READE     TABEL                                  31
102700960506     C*
102800960506DO  1C     *IN31         DOWEQ     '0'
102900960506IF  2C     TBLFLG        IFEQ      ' '
103000960506     C                   MOVEL     TBLUNI        DS7C
103100960506     C*
103200960506IF  3C     §7CSAN        IFEQ      ' '
103300020906     C                   ADD       1             XI
103400020906     C                   MOVEL     TBLKEY        SAN(XI)
103500960506E   3C                   ENDIF
103600961106     C*
103700961106     C* CARICO LE ANOMALIE ESTERNE AUTOCHIUSE
103800961106IF  3C     §7CAIE        IFEQ      'E'
103900961106     C     §7CCHA        ANDEQ     'S'
104000020906     C                   ADD       1             XX
104100020906     C                   MOVEL     TBLKEY        CHA(XX)
104200961106E   3C                   ENDIF
104300960506E   2C                   ENDIF
104400960506     C*
104500960506     C     KTAB          READE     TABEL                                  31
104600960506E   1C                   ENDDO
104700061023     C***
104800061023     C* carico le anomalie di spunte per DANNI
104900061023     C***
105000061023     C                   MOVEL     '3E'          COD
105100061023     C                   Z-ADD     0             XI
105200061023     C     KTAB          SETLL     TABEL
105300061023     C     KTAB          READE     TABEL                                  31
105400061023     C*
105500061023DO  1C     *IN31         DOWEQ     '0'
105600061023IF  2C     TBLFLG        IFEQ      ' '
105700061023     C                   MOVEL     TBLUNI        DS3E
105800061023     C*
105900061023     c* Escludo le anomlie di rientro che servono per la
106000061023     c*  chiusura distinta
106100061023IF  3C     §3eFTA        IFne      'R'
106200061023     C                   ADD       1             XI
106300061023     C                   MOVEL     TBLKEY        ANOM(XI)
106400061023E   3C                   ENDIF
106500061023E   2C                   ENDIF
106600061023     C*
106700061023     C     KTAB          READE     TABEL                                  31
106800061023E   1C                   ENDDO
106900960506     C*
107000930524     C                   SETOFF                                       31
107100950403     C***
107200950403     C* DETERMINO LA DATA DI PULIZIA PIU' RECENTE
107300950403     C***
107400950403     C                   SELECT
107500950403     C     DATSPP        WHENGE    DATBRV
107600950403     C     DATSPP        ANDGE     DATBRC
107700950403     C                   Z-ADD     DATSPP        WDAT
107800950403     C*
107900950403     C     DATBRV        WHENGE    DATSPP
108000950403     C     DATBRV        ANDGE     DATBRC
108100950403     C                   Z-ADD     DATBRV        WDAT
108200950403     C*
108300950403     C                   OTHER
108400950403     C                   Z-ADD     DATBRC        WDAT
108500950403     C                   ENDSL
108600950403     C*
108700930111     C                   ENDSR
108800070821     C*---------------------------------------------------------------*
108900070821     C*                    CerDATbrv                                  *
109000070821     C* Cerco la data più recente della spunta letta                  *
109100070821     C*---------------------------------------------------------------*
109200070821     c     CerDATbrv     BEGSR
109300070821    3c                   Select
109400070821     c                   When      BrvDfs > *Date and BrvDcs > *Date
109500070821     c                   If        BrvDfs > BrvDcs
109600070821     c                   Eval      Pudcs = BrvDcs
109700070821     c                   Else
109800070821     c                   Eval      Pudcs = BrvDfs
109900070821     c                   EndIf
110000070821      * Prendo la data caricamento se data immissione maggiore di udate
110100070821     c                   When      BrvDfs > *Date
110200070821     c                   Eval      Pudcs = BrvDcs
110300070821      * Prendo la data immissione se data caricamento maggiore di udate
110400070821     c                   When      BrvDcs > *Date
110500070821     c                   Eval      Pudcs = BrvDfs
110600070821
110700070821     c                   Other
110800070821     C* PRENDO LA DATA PIÙ ALTA FRA QUELLA DI CARICAMENTO E QUELLA
110900070821     C* DI IMMISSIONE/VARIAZIONE
111000070821     C     BRVDFS        IFGE      BRVDCS
111100070821     C                   Z-ADD     BRVDFS        PUDCS
111200070821     C                   ELSE
111300070821     C                   Z-ADD     BRVDCS        PUDCS
111400070821     C                   ENDIF
111500070821    3c                   EndSl
111600070821     c                   ENDSR
111700930519     C*---------------------------------------------------------------*
111800930519     C*                    DEFVAR                                     *
111900930519     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
112000930519     C*---------------------------------------------------------------*
112100930519     C     DEFVAR        BEGSR
112200930519     C*
112300930519     C     *ENTRY        PLIST
112400930519     C                   PARM                    KPJBA
112500100504      *
112600100504     c     *dtaara       define    §azute        AZUTEds
112700100504     c     *dtaara       define    §datiute      dDATIUTE
112800100504     c                   in(E)     *dtaara
112900100504     c                   if        %ERROR or RSUT = *blanks
113000100504     c                   clear                   Tibs34Ds
113100100504     c                   call      'TIBS34R'
113200100504     c                   parm                    Tibs34Ds
113300100504     c                   in        *dtaara
113400100504     c                   endif
113500100504     C*
113600100504     c
113700100504     C                   CALL      'FNLV61R'
113800100504     C                   PARM                    O61DRF            8 0
113900100618     C                   PARM      ' '           i61Rep            1
114000100618     C                   PARM                    dvpopuladd
114100100618     c
114200100504     C                   z-add     o61drf        G02inv
114300100504     C                   MOVEL     '3'           G02ERR
114400100504     C                   CALL      'XSRDA8'
114500100504     C                   PARM                    WLBDAT
114600100618     c
114700100618     c* Se ci sono aggiuntivi di pulizia da sottrarre --> tolti anche questi
114800100504     c
114900100504     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A
115000100504     C                   MOVEL     '5A'          COD
115100100504     C                   MOVEL     '1       '    KEY
115200100504     C     KTAB2         CHAIN     TABEL                              30
115300100504     C  N30              MOVEL     TBLUNI        DS5A
115400100504     C   30              MOVEL     *ZEROS        DS5A
115500100504     C*
115600100504     C* CALCOLO DATE PULIZIA SPUNTE CON BOLLE CONSEGNATE SENZA EVENTI
115700100504     C*   ANOMALI E CON EVENTI ANOMALI
115800100504     C                   EXSR      CALDAT
115900100504     C*
116000100504     C* PULIZIA SPUNTE :  SOTTRAGGO §5ASPP AI GIORNI
116100100504     C*   PULIZIA SPUNTE DI DISGUIDI O BOLLE TRANSITO
116200100504     C     G02TGI        SUB       §5ASPP        GIOTGI
116300100618     c                   sub       §vpogg        GIOTGI
116400100504     C*
116500100504     C                   CALL      'XSRGI8'
116600100504     C                   PARM                    WGIDAT
116700100504     C                   Z-ADD     GIOINV        datSPP
116800100504      *
116900100504      * pulizia spunte con c.a.: sottraggo §5ASCA ai giorni
117000100504     c     G02tgi        sub       §5ASca        GIOtgi
117100100504     c                   call      'XSRGI8'
117200100504     c                   parm                    WGIdat
117300100504     c                   z-add     GIOinv        datSCA
117400100504      *
117500100504      * pulizia spunte con BRVCAN impostato: sottatto §5ABLP ai giorni
117600100504     c*  tengo infatti le spunte come le bolle partenza
117700100504     c     G02tgi        sub       §5Ablp        GIOtgi
117800100618     c                   sub       §vpogg        GIOTGI
117900100504     c                   call      'XSRGI8'
118000100504     c                   parm                    WGIdat
118100100504     c                   z-add     GIOinv        datcan
118200100504     c*
118300930519     C*
118400941214     C* LETTURA TABEL00F
118500941214     C     KTAB          KLIST
118600051007     C                   KFLD                    CODUT             1 0
118700941214     C                   KFLD                    COD
118800100504     C     KTAB2         KLIST
118900100504     C                   KFLD                    CODUT
119000100504     C                   KFLD                    COD
119100100504     C                   KFLD                    KEY
119200100504     C*
119300941214     C* LETTURA FNBLT27L
119400941214     C     KBLT          KLIST
119500941214     C                   KFLD                    BRVLNP
119600941214     C                   KFLD                    BRVLNA
119700941214     C                   KFLD                    BRVNRS
119800941214     C                   KFLD                    BRVNSC
119900941214     C* LETTURA FNBLP01L
120000941214     C     KBLP          KLIST
120100941214     C                   KFLD                    BLTAAS
120200941214     C                   KFLD                    BLTLNP
120300941214     C                   KFLD                    BLTNRS
120400941214     C                   KFLD                    BLTNSP
120500941214     C* LETTURA FNEVB01L
120600941214     C     KEVB          KLIST
120700941214     C                   KFLD                    WAAS
120800941214     C                   KFLD                    BLTLNP
120900941214     C                   KFLD                    BLTNRS
121000941214     C                   KFLD                    BLTNSP
121100941214     C     KEVB2         KLIST
121200941214     C                   KFLD                    WAAS
121300941214     C                   KFLD                    ARTLNP
121400941214     C                   KFLD                    ARTNRS
121500941214     C                   KFLD                    ARTNSP
121600941214     C* LETTURA FNARB01L
121700941214     C     KARB          KLIST
121800941214     C                   KFLD                    ARTAAS
121900941214     C                   KFLD                    ARTLNP
122000941214     C                   KFLD                    ARTNRS
122100941214     C                   KFLD                    ARTNSP
122200970611     C* LETTURA FNEVS02L
122300970611     C     KEVS          KLIST
122400040324     C                   KFLD                    WRVLNP
122500040324     C                   KFLD                    WRVLNA
122600040324     C                   KFLD                    WRVNRS
122700040324     C                   KFLD                    WRVNSC
122800070115     C* POSIZIONAMENTO FNBRV07L
122900970611     C     KBRV          KLIST
123000970611     C                   KFLD                    WRVLNP
123100970611     C                   KFLD                    WRVLNA
123200970611     C                   KFLD                    WRVNRS
123300970611     C                   KFLD                    WRVNSC
123400970611     C     KBRV7         KLIST
123500970611     C                   KFLD                    BRVLNP
123600970611     C                   KFLD                    BRVLNA
123700970611     C                   KFLD                    BRVNRS
123800970611     C                   KFLD                    BRVNSC
123900051007     C     Kdct          KLIST
124000051007     C                   KFLD                    dctaac
124100051007     C                   KFLD                    dctfil
124200051007     C                   KFLD                    dctnca
124300930521     C*
124400930519     C                   ENDSR
124500100618     c
124600100504     C*--- CALCOLO DATE DI PULIZIA SPUNTE ----------------------------*
124700100504     C     CALDAT        BEGSR
124800100504     C*
124900100504     C* GIORNI PULIZIA
125000100504     C                   MOVEL     §5ABRV        COMBRV
125100100504     C                   MOVEL     §5ABRC        COMBRC
125200100618     c* addiziono eventuali gg lavorativi aggiuntivi
125300100618     c                   add       §vpogglav     COMBRV
125400100618     c                   add       §vpogglav     COMBRC
125500100618     c
125600100618     C****
125700100618     C** GIORNI DATA CONSEGNA PER BOLLE CON   EVENTI ANOMALI
125800100618     C****
125900100618     c                   clear                   xgiolavds
126000100618     c                   eval      ixgldata=o61drf
126100100618     c                   eval      ixglfil =simfel
126200100618     c                   eval      ixglpa  ='A'
126300100618     c                   eval      ixglsub ='S'
126400100618     C* AGGIUNGO 1 AI GIORNI DI PULIZIA CONTENUTI IN TABELLA
126500100618     c                   eval      ixglgg  =combrv+1
126600100618     c                   eval      ixgllav ='S'
126700100618     c                   call      'XGIOLAV'
126800100618     c                   parm                    xgiolavds
126900100618     c                   Move      oxgldata      datbrv
127000100618
127100100618     C****
127200100618     C** GIORNI DATA CONSEGNA PER BOLLE SENZA EVENTI ANOMALI
127300100618     C****
127400100618     c                   clear                   xgiolavds
127500100618     c                   eval      ixgldata=o61drf
127600100618     c                   eval      ixglfil =simfel
127700100618     c                   eval      ixglpa  ='A'
127800100618     c                   eval      ixglsub ='S'
127900100618     C* AGGIUNGO 1 AI GIORNI DI PULIZIA CONTENUTI IN TABELLA
128000100618     c                   eval      ixglgg  =combrc+1
128100100618     c                   eval      ixgllav ='S'
128200100618     c                   call      'XGIOLAV'
128300100618     c                   parm                    xgiolavds
128400100618     c                   Move      oxgldata      datbrc
128500100618
128600100504     C                   ENDSR
