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
003400160601     d
003500160601     d lnaR            s              3  0 dim(10) ctdata perrcd(10)
003600020906
003700960506     D*
003800020906     D** DEFINIZIONE STRUTTURE DATI ESTERNE
003900020808     D*
004000930519     D KPJBA         E DS
004100100504      * - Parametri x Controllo profilo utenti
004200100504     d TIBS34ds      e ds                  inz
004300100504      * - Ds di riferimento al file esterno AZUTE00F
004400100504     d AZUTEds       e ds                  inz extname(AZUTE00F)
004500100504      * - Ds per dati organigramma
004600100504     d dDATIUTE      e ds                  inz
004700100504      *
004800100618     D xgiolavds     E DS
004900100618     D dvpopuladd    E DS
005000100618     D FNLV53DS      E DS
005100020808     D DS2Z          E DS
005200061023     D DS3E          E DS
005300020808     D DS7C          E DS
005400100504     D DS5A          E DS
005500040806      * ds per il controllo della presenza di CA per la spedizione richiesta
005600040806     d FIDN12DS      e ds
005700051007     d  skKey                 26    305    dim(20)
005800051007     d  skDca                326    485  0 dim(20)
005900051007     d
006000051007     d dsKey           ds
006100051007     d  dctaac                        4  0
006200051007     d  dctfil                        3  0
006300051007     d  dctnca                        7  0
006400020808     D*
006500950303     D                 DS
006600950303     D  BRVLNP                 1      3  0
006700950303     D  BRVLNA                 4      6  0
006800950303     D  BRVNRS                 7      8  0
006900950303     D  BRVNSC                 9     15  0
007000950303     D  BRVSE                  1     15  0
007100970611     D                 DS
007200970611     D  WRVLNP                 1      3  0
007300970611     D  WRVLNA                 4      6  0
007400970611     D  WRVNRS                 7      8  0
007500970611     D  WRVNSC                 9     15  0
007600970611     D  WBRVSE                 1     15  0
007700950303     D                 DS
007800950303     D  BLTAAS                 1      4  0
007900950303     D  BLTLNP                 5      7  0
008000950303     D  BLTNRS                 8      9  0
008100950303     D  BLTNSP                10     16  0
008200950303     D  BLTSPE                 1     16  0
008300950303     D                 DS
008400950303     D  ARTAAS                 1      4  0
008500950303     D  ARTLNP                 5      7  0
008600950303     D  ARTNRS                 8      9  0
008700950303     D  ARTNSP                10     16  0
008800950303     D  ARTSPE                 1     16  0
008900950403     D                 DS
009000950404     D  BRVNPG                 1      1  0
009100020916     d  BrvFgs                 2      4  0
009200000125     D  BRVNFV                 5     10  0
009300000125     D  WFVV                   1     10  0
009400100706     D                 DS
009500100706     D  blpaas                 1      4  0
009600100706     D  blpmgs                 5      8  0
009700100706     D  BLPSPE                 1      8  0
009800100706     D                 DS
009900100706     D  arbaas                 1      4  0
010000100706     D  arbmgs                 5      8  0
010100100706     D  ARBSPE                 1      8  0
010200960506     D*
010300100504     D WLBDAT          DS                  inz
010400100504     D  G02DAT                 1      8  0 inz
010500100504     D  G02INV                 9     16  0 inz
010600100504     D  G02ERR                17     17    inz
010700100504     D  G02TGI                18     22  0 inz
010800100504     D WGIDAT          DS                  inz
010900100504     D  GIODAT                 1      8  0 inz
011000100504     D  GIOINV                 9     16  0 inz
011100100504     D  GIOTGI                17     21  0 inz
011200020906
011300020906     D*
011400020906     D** DEFINIZIONE CAMPI
011500020906     D*
011600100504     d CODUT           s                   like(TBLkut) inz(1)
011700100504     d KEY             s                   like(TBLKEY) inz
011800020906     D COD             S                   LIKE(TBLCOD) inz
011900020906     D PUDCS           S                   LIKE(BRVDCS) inz
012000020906     D PULNP           S                   LIKE(BRVLNP) inz
012100020906     D WAAS            S                   LIKE(EVBAAS) inz
012200020906     D WDAT            S                   LIKE(DATSPP) inz
012300020906     D SAVFVV          S                   LIKE(WFVV)   inz
012400020906     D SAVBRC          S                   LIKE(DATBRC) inz
012500020906     D WMAX            S                   LIKE(TBLKEY) inz
012600020906     D SESAV           S                   LIKE(BRVSE)  inz
012700070821     D FVSAV           S                   LIKE(WFVV)   inz
012800020906     D XI              S              2  0
012900020906     D XX              S              3  0
013000070821     D WFILEERR        S              1    inz
013100070821     D WOK             S              1    inz
013200020906     D WGIR            S              1    inz
013300020906     D W0080           S              8  0 inz
013400020906     D WCABLP          S              1    inz
013500020906     D SAVse           S             15  0 inz
013600020925     D SAVpar          S             15  0 inz
013700020906     D SAVspe          S             16  0 inz
013800020906     D SAVsp2          S             16  0 inz
013900020906      * Flag "Trovata bolla"
014000020906     d $Found          s              1    inz(*off)
014100090416     d DataPuliz       s              8  0 inz
014200100504     d COMBRV          s                   like(§5ABRV) inz
014300100504     d COMBRC          s                   like(§5ABRC) inz
014400100504     d datbrv          s              8  0 inz
014500100504     d datbrc          s              8  0 inz
014600100504     d datSPP          s              8  0 inz
014700100504     d datSca          s              8  0 inz
014800100504     d datcan          s              8  0 inz
014900160601
015000160601     d data_ISO        s               d   datfmt(*ISO)
015100020906
015200960506     I*
015300990510     I** RIDENOMINAZIONE CAMPI FNANM
015400990510     IFNANM002
015500960430     I              ANMLNA                      BRVLNA
015600960430     I              ANMNRS                      BRVNRS
015700960430     I              ANMSCN                      BRVNSC
015800020906
015900941214     C*---------------------------------------------------------------*
016000941214     C* INDICATORI USATI                                              *
016100941214     C*---------------------------------------------------------------*
016200960506     C* 21    - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE OPPURE CON
016300960506     C*         TIPO ANOMALIA "T"
016400961106     C* 22    - ANOMALIA ESTERNA AUTOCHIUSA
016500020926     C* 31 33 - uso esclusivo della RICPAR
016600020926     C* 32 34 - uso esclusivo della RICARR
016700061023     C* 36    - controllo se brvcan ha anomalia x danni
016800020926     C* 30/39 - CHAIN SUI FILES
016900930519     C*---------------------------------------------------------------*
017000930519     C*
017100930519     C* DEFINIZIONE VARIABILI, KLIST E PLIST
017200930520     C                   EXSR      DEFVAR
017300100513     c*
017400100513   -1c                   if        datbrv>0 and datbrc>0 and datspp>0
017500930520     C*
017600930521     C* CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE
017700930521     C                   EXSR      CARTAB
017800950303     C*
017900970611     C                   CLEAR                   WBRVSE
018000970611     C*
018100960506     C* ESEGUO 2 VOLTE IL CICLO DEL PGM: -PER LA PULIZIA DELLE ANOMALIE
018200960506     C*                                  -PER LA PULIZIA DELLE SPUNTE
018300020906     C                   MOVEL     'A'           WGIR
018400960506     C*
018500960515    0C                   DO        2
018600950303     C                   CLEAR                   SAVSE
018700020925     C                   CLEAR                   SAVpar
018800970611     C                   CLEAR                   SESAV
018900950303     C                   CLEAR                   SAVSPE
019000950303     C                   CLEAR                   SAVSP2
019100950403     C                   CLEAR                   SAVFVV
019200070821     C                   CLEAR                   WfileERR
019300941214     C*
019400960430     C* LETTURA  A N O M A L I E
019500980217     C*  PER LE ANOMALIE PULIAMO TUTTO CON LA DATA PIU' ALTA
019600960430    1C     WGIR          IFEQ      'A'
019700960515     C                   MOVEL     DATBRC        SAVBRC
019800960515     C                   MOVEL     DATBRV        DATBRC
019900960515     C*
020000990510     C     *LOVAL        SETLL     FNANM002
020100990510     C                   READ      FNANM002                             9930
020200990510     C   99              READ(N)   FNANM002                               30
020300960515     C*
020400960430   X1C                   ELSE
020500960515     C                   MOVEL     SAVBRC        DATBRC
020600960430     C* LETTURA  S P U N T E
020700070115     C     *LOVAL        SETLL     FNBRV007
020800070115     C                   READ      FNBRV007                             9930
020900070115     C   99              READ(N)   FNBRV007                               30
021000960430    1C                   ENDIF
021100941214     C*
021200941214    1C     *IN30         DOWEQ     *OFF
021300960730     C*
021400960730     C* SOLO SE SONO RIUSCITO AD ALLOCARE IL RECORD
021500960730   1AC     *IN99         IFEQ      *OFF
021600960430     C*
021700990510     C* SOLO PER  A N O M A L I E :
021800960506    2C     WGIR          IFEQ      'A'
021900960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
022000960903     C*                           CANCELLARE L'ANOMALIA
022100980217    3C     ANMDAO        IFEQ      0
022200990510     C                   Z-ADD     19000101      PUDCS
022300980217   X3C                   ELSE
022400990510     C                   Z-ADD     ANMDAO        PUDCS
022500980217    3C                   ENDIF
022600980217     C**
022700980217   X2C                   ELSE
022800021105      * Prendo la data minore tra caricamento e immissione se tutte e 2 sono
022900021105      * maggiori di udate
023000070821     c                   EXSR      CerDATbrv
023100070821     c
023200980217    2C                   ENDIF
023300960430     C*
023400980217     C* SE LA DATA APERTURA ANOMALIA O
023500980217     C*    LA DATA DI CARICAMENTO SPUNTA E' > DELLA DATA PIU' RECENTE
023600950403     C*  DI PULIZIA --> NON LA CONSIDERO NEMMENO
023700980217    2C     PUDCS         IFLE      WDAT
023800020906     C                   MOVEL     'S'           WOK
023900941214     C*
024000960430     C* SOLO PER  A N O M A L I E
024100960430    3C     WGIR          IFEQ      'A'
024200960506     C*
024300960506     C* 21 ON  - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE
024400960506     C     ANMCAA        LOOKUP    SAN                                    21
024500960506     C*
024600960430     C* LE ANOMALIE "E/R" SI PULISCONO SOLO SE HANNO LA DATA CHIUSURA
024700960506    4C  N21ANMAIE        IFEQ      'E'
024800960430     C     ANMAIE        OREQ      'R'
024900961106     C                   SETOFF                                       22
025000961106     C*
025100961106     C* 22 ON  - SI TRATTA DI ANOMALIA ESTERNA AUTOCHIUSA
025200961106    5C     ANMAIE        IFEQ      'E'
025300961106     C     ANMCAA        ANDNE     70
025400961106     C     ANMCAA        ANDNE     71
025500961106     C     ANMCAA        LOOKUP    CHA                                    22
025600961106    5C                   ENDIF
025700961106     C*
025800961106    5C     ANMAIE        IFEQ      'E'
025900961106     C     *IN22         ANDEQ     *OFF
026000980217     C**
026100980217     C* VERIFICO SE LNA FA O MENO L'IDD
026200020906     c                   eval      XX = 1
026300050805     c**   BRVlna        lookup    SKpo(xx)                               37
026400050805     c** 37              movel     NIDD(xx)      LNANID
026500050805     c**N37              movel     'N'           LNANID
026600980217     C**
026700050805    6C**   LNANID        IFNE      'N'
026800980217    7C     ANMDCH        IFEQ      0
026900980217     C     ANMFT1        OREQ      ' '
027000980217     C     ANMFL1        ANDGT     0
027100980217     C     ANMFT2        OREQ      ' '
027200980217     C     ANMFL2        ANDGT     0
027300980217     C                   CLEAR                   WOK
027400980217    7C                   ENDIF
027500050805    6C**                 ENDIF
027600050805    5C                   ENDIF
027700961106     C*
027800960430   X4C                   ELSE
027900960506     C* 21 ON  - ANOMALIA CON TIPO ANOMALIA "T"
028000960506     C     ANMAIE        IFEQ      'T'
028100960506     C                   SETON                                        21
028200960430     C                   ENDIF
028300960430    4C                   ENDIF
028400960430    3C                   ENDIF
028500960430     C*
028600960430     C* WOK = "S" ---> RECORD DA ELABORARE
028700960506    3C     WOK           IFEQ      'S'
028800960430     C*
028900960506    4C     *IN21         IFEQ      *OFF
029000960530     C*
029100960506    5C     WGIR          IFEQ      'A'
029200960506     C     ANMLNP        IFGT      0
029300960506     C                   MOVEL     ANMLNP        BRVLNP
029400960506     C                   ELSE
029500960506     C                   MOVEL     ANMFLS        BRVLNP
029600960506     C                   ENDIF
029700960506    5C                   ENDIF
029800960530     C*
029900970611     C*
030000970611     C* A CAMBIO SEGNACOLLO:
030100970611     C     WGIR          IFEQ      'S'
030200970611     C     BRVSE         ANDNE     SESAV
030300970611     C* Verifico se per il collo precedente devo cancellare gli eventi
030400970611     C     WBRVSE        IFGT      *ZEROS
030500970611     C                   EXSR      DELEVS
030600970611     C                   CLEAR                   WBRVSE
030700970611     C                   END
030800970611     C                   MOVE      BRVSE         SESAV
030900970611     C                   END
031000941214     C*
031100020906     c                   eval      $Found = *off
031200941214     C* CERCO NELLE BOLLE IN ARRIVO
031300941214     C                   EXSR      RICARR
031400020906     c*
031500020906     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
031600020906     c                   if        $Found = *off
031700020906     c                   exsr      RICPAR
031800020906     c                   endif
031900020925     c                   else
032000020925     c* se anomalia senza spedizione o di tipo "T" eseguo solo il ciclo
032100020925     c*  NO BOLLE
032200020925     c                   eval      $Found = *off
032300020925    4c                   endif
032400020906     C*
032500020906     c                   if        $Found = *off
032600020906     C     WGIR          IFEQ      'A'
032700020906     C                   EXSR      NOBOL2
032800020906     C                   ELSE
032900020906     C                   EXSR      NOBOLL
033000020906     C                   ENDIF
033100020906     c                   endif
033200950403     C*
033300960506    3C                   ENDIF
033400960506    2C                   ENDIF
033500960730     C*
033600960730   1AC                   ENDIF
033700941214     C*
033800960506    2C     WGIR          IFEQ      'A'                                          *ANOMALIE
033900990510     C                   READ      FNANM002                             9930
034000990510     C   99              READ(N)   FNANM002                               30
034100960730   X2C                   ELSE                                                   *SPUNTE
034200960730     C*
034300070115     C                   READ      FNBRV007                             9930
034400070115     C   99              READ(N)   FNBRV007                               30
034500960506    2C                   ENDIF
034600960506    1C                   ENDDO
034700941214     C*
034800960506     C* IMPOSTO CAMPI PER PULIZIA SPUNTE
034900960506     C                   MOVEL     'S'           WGIR
035000960506     C                   SETOFF                                       21
035100960506    0C                   ENDDO
035200970611     C*
035300070821     C* Verifico se x l'ultimo collo devo cancellare gli eventi
035400970611     C     WBRVSE        IFGT      *ZEROS
035500970611     C                   EXSR      DELEVS
035600970611     C                   CLEAR                   WBRVSE
035700970611     C                   END
035800980210     C****
035900980210     C* PULIZIA SPUNTE ERRATE SEGNACOLLI
036000980210     C****
036100070821     c                   eval      wfileerr='S'
036200070115     C                   OPEN      FNBRVE1L
036300070115     C     *LOVAL        SETLL     FNBRVE1L
036400070115     C                   READ      FNBRVE1L                               30
036500980210    1C     *IN30         DOWEQ     *OFF
036600980210     C* Prendo la data foglio a cambio di numero
036700070821     c                   if        wfvv<>FVSAV
036800020808     C                   CLEAR                   FNLV53DS
036900070821     C                   Z-ADD     BRvNPG        D53NPG
037000070821     C                   Z-ADD     BRvNFV        D53NFV
037100070821     C                   Z-ADD     BRvFGS        D53FGS
037200980210     C                   CALL      'FNLV53R'
037300020808     C                   PARM                    FNLV53DS
037400070821     C                   MOVE      wfvv          FVSAV
037500090416     c* se errore, imposto data caricammento spunte in data foglio
037600090416     c                   if        d53err<>' '
037700090416     c                   movel     PUDCS         D53DFV
037800090416     c                   clear                   FVSAV
037900090416     c                   endif
038000090416     c
038100980210    2C                   ENDIF
038200070821     c
038300070821    2C     D53DFV        IFLE      DATSPP
038400070821     c
038500070821     c* Per categorie 4 / 8 / 1 partenza, deleto
038600070821    3c                   if        brvnpg=4 or brvnpg = 8 or
038700070821     c                             (brvnpg=1 and brvftr='L')
038800070115     C                   DELETE    FNBRVE
038900070821   x3c                   else
039000070821     c
039100070823     c* Per le altre categorie mi legGo alla bolla
039200070823     c                   EXSR      CerDATbrv
039300070823     C
039400070821     c* Per le altre categorie mi lego alla bolla
039500070821     c                   eval      $Found = *off
039600070821     C* CERCO NELLE BOLLE IN ARRIVO
039700070821     C                   EXSR      RICARR
039800070821     c*
039900070821     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
040000070821    4c                   if        $Found = *off
040100070821     c                   exsr      RICPAR
040200070823     C                   ENDIF
040300070823     C
040400070823    4c                   if        $Found = *off
040500070821     C                   DELETE    FNBRVE
040600070821    4c                   endif
040700070821    3c                   endif
040800070821     c
040900070821    2C                   END
041000070115     C                   READ      FNBRVE1L                               30
041100980210    1C                   ENDDO
041200100513     c
041300100513   -1c                   endif
041400040806     C*
041500040806     C                   MOVEL     'C'           I12TLA
041600040806     C                   CALL      'FIDN12R'
041700040806     C                   PARM                    FIDN12DS
041800040806      *
041900020906     C                   MOVEL     *ON           *INLR
042000941214     C*
042100941214     C*---------------------------------------------------------------*
042200941214     C*                    RICPAR                                     *
042300941214     C* CERCO NELLE BOLLE IN PARTENZA                                 *
042400020926     c* indicatori 31 e 33 ad uso esclusivo della routine ricpar
042500941214     C*---------------------------------------------------------------*
042600941214     C     RICPAR        BEGSR
042700020926     c* Se anomlie senza segnacollo ma solo con la spedizione (70 e 71)
042800020926     c*  devo comunque entrare nell'if per chainare fnarb
042900020926     c                   if        wgir='A' and brvnsc=0
043000020926     c                   eval      savpar=*hival
043100020926     c                   endif
043200950303     C*
043300941214     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
043400020925    0C     BRVSE         IFNE      SAVpar
043500950303     C*
043600960508    1C     WGIR          IFEQ      'S'
043700960508     C     WGIR          OREQ      'A'
043800960508     C     ANMNSP        ANDEQ     0
043900941214     C     KBLT          CHAIN     FNBLT000                           31
044000960508     C*
044100960508   X1C                   ELSE
044200960514     C                   SETOFF                                       31
044300960508     C                   Z-ADD     ANMAAS        BLTAAS
044400960514     C                   MOVEL     ANMLNP        BLTLNP
044500960508     C                   MOVEL     BRVNRS        BLTNRS
044600960508     C                   MOVEL     ANMNSP        BLTNSP
044700960508    1C                   ENDIF
044800941214     C*
044900941214     C* AGGANCIO LA BOLLA
045000950303    1C  N31BLTSPE        IFNE      SAVSPE
045100950303     C     KBLP          CHAIN     FNBLP000                           31
045200990126     C* SE C'E' UNA C.A. NON CANCELLO LE SPUNTE
045300051006     c* 07 ott 05 --> addesso puliamo se c.a. in gestione alla sede
045400051006     c*               passati xxx giorni dall'apertura c.a.
045500040806      *
045600051007    1c                   If        not *in31
045700040806     c                   clear                   fidn12ds
045800040806     c                   eval      i12aas = bltaas
045900040806     c                   eval      i12lnp = bltlnp
046000040806     c                   eval      i12nrs = bltnrs
046100040806     c                   eval      i12nsp = bltnsp
046200040806     c                   eval      i12fel = 'E'
046300040806     c                   eval      i12fan = 'E'
046400040806      *
046500051007     c                   EXSR      CERCA
046600040806      *
046700950303    1C                   ENDIF
046800051007    1C                   ENDIF
046900950303     C*
047000020925     C                   MOVEL     BRVSE         SAVpar
047100950303    0C                   ENDIF
047200950120     C*
047300950120    1C     *IN31         IFEQ      *OFF
047400020906     c*
047500020906     c                   eval      $Found = *on
047600941214     C*
047700051007     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA e senza c.a.
047800051007     c
047900950303    2C     BLPDCM        IFGT      0
048000990126     C     WCABLP        ANDEQ     ' '
048100100706     c
048200100706     C* Elaboro anche se non consegnata senza C.A. solo SPUNTE
048300100706    2C     BLPDCM        oreq      0
048400100706     C     WCABLP        ANDEQ     ' '
048500100706     C     WGIR          ANDEQ     'S'
048600100706     c
048700051007     c*   o con data c.a. < data gg pulizia bolle con c.a.
048800100706     c*   in questo caso pulisco sempre, anche se non è consegnata
048900100706    2c     wcablp        oreq      'O'
049000950303     C*
049100100706     c                   select
049200960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
049300960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
049400100706   2AC     PUDCS         whengt    BLPDCM
049500051007     c     blpdcm        andgt     0
049600160603     C     WCAblp        ANDEQ     ' '
049700960509     C*
049800090416     c                   select
049900090416     c                   when      wfileErr='S'
050000090416     c                   if        d53dfv<=datcan
050100090416     c                   delete    fnbrve
050200090416     c                   endif
050300090416     c                   when      WGIR='A'
050400960509     C                   EXSR      NOBOL2
050500090416     C                   other
050600960509     C                   EXSR      NOBOLL
050700090416     C                   ENDSL
050800090416     c*
050900090416     c* se spunta con "*" controlo direttamente la data consegna
051000090416     c*  con la data pulizia spunte con C.A.
051100100706   2ac                   when      brvnvr='*'  and blpdcm>0
051200090416     c
051300090416    3c                   if        blpdcm<=datcan
051400090416     c                   exsr      BRVDEL
051500090416    3c                   endif
051600100706     c
051700100706     c* Bolla NON consegnata --> tengo almeno un anno la spunta poi cancello
051800100706   2ac                   when      blpdcm= 0  and wcablp=' '
051900100706    6c                   if        blpSPE<=datSCA and PUDCS<=datSCA
052000100706     c                   EXSR      BRVDEL
052100100706     c                   endif
052200090416     c
052300100706  x2ac                   other
052400160603     c
052500160603     c                   clear                   lna_r
052600160603     c* Se la linea di arrivo è a "rischio" devo tenere 4 mesi le spunte
052700160603     c                   if        wgir='S' and %lookup(bltlna:lnaR)>0
052800160603     c                   eval      lna_r='S'
052900160603     c                   endif
053000960509     C*
053100160603     c*
053200950303    3C     BLTSPE        IFNE      SAVSPE
053300160603     c     lna_r         andne     'S'
053400160603     c     wfileErr      andne     'S'
053500941214     C*
053600941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
053700941214     C*                  - DATA CONSEGNA PARZIALE
053800950303    4C     BLPCCA        IFEQ      *BLANKS
053900941214     C     BLPDCP        ANDEQ     *ZEROS
054000950120     C                   SETOFF                                       33
054100950303   X4C                   ELSE
054200950120     C                   SETON                                        33
054300950303    4C                   ENDIF
054400941214     C*
054500981008     C* NON HA EVENTI (O SOLO QUELLI FASULLI)
054600090416    4C     *IN33         IFEQ      *OFF
054700981008     C                   Z-ADD     BLTAAS        WAAS
054800990804     C     KEVB          SETLL     FNEVB000
054900990804     C     KEVB          READE     FNEVB000                               98
055000090416    5C     *IN98         DOWEQ     *OFF
055100981008     C     EVBCEV        LOOKUP    EFS                                    97
055200981008     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
055300090416    6C     *IN97         IFEQ      *OFF
055400981008     C                   SETON                                        9833
055500981008     C                   ELSE
055600981008     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
055700990804     C     KEVB          READE     FNEVB000                               98
055800090416    6C                   ENDIF
055900090416    5C                   ENDDO
056000090416    4C                   ENDIF
056100941214     C*
056200941214     C* NON HA GIACENZA
056300050331     C  N33KBLP          SETLL     Tigcp01l                               33
056400941214     C*
056500941214     C* NON HA LEGAMI BOLLA (MAMMA)
056600950120     C  N33KBLP          SETLL     FNLBL000                               33
056700941214     C*
056800941214     C* NON HA LEGAMI BOLLA (FIGLIA)
056900950120     C  N33KBLP          SETLL     FNLBL002                               33
057000950303     C*
057100020906     C                   MOVEL     BLTSPE        SAVSPE
057200950303    3C                   ENDIF
057300160603     c
057400090416     c* Imposto la data di pulizia :
057500090416    3c                   select
057600090416     c* 1) Spunte nel file degli errori
057700090416     c                   when      wfileErr='S'
057800090416     c                   eval      datapuliz=DATcan
057900160603     c* 2) linea a rischio: devo tenere 4 mesi
058000160603     c                   when      lna_r='S'
058100160603     c                   eval      datapuliz=DTPEXP_ymd
058200090416     C* 2) 33 OFF -    BOLLA SENZA EVENTI ANOMALI
058300090416     c                   when      *in33 =*off
058400090416     c                   eval      datapuliz=DATBRC
058500090416     c                   other
058600090416     C* 2)  altrimenti BOLLA CON   EVENTI ANOMALI
058700090416     c                   eval      datapuliz=DATBRV
058800090416    3c                   ENDSL
058900950303     C*
059000941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
059100090416    3C     BLPDCM        IFLE      DataPuliz
059200051007     c     blpdcm        andgt     0
059300051007     c* pulisco sempre se non vonsegna con c.a. perchè pulisco in base
059400051007     c*  alla data pulizia spunte con c.a.
059500051007     c     blpdcm        oreq      0
059600051007     c     wcablp        andeq     'O'
059700051007     c
059800090416    4C     WGIR          IFEQ      'A'
059900990510     C                   DELETE    FNANM002
060000090416   X4C                   ELSE
060100061019     c
060200090416     c* Se ha anomalia di spunta x DANNI, deleto solo se data <=data pulizia
060300061019     c*  spunte con anomalia
060400061023     c*
060500061023     c                   setoff                                       36
060600090416    5c                   if        brvcan<>' '
060700061023     c     brvcan        lookup    anom                                   36
060800090416    5c                   endif
060900061023     c
061000090416    5c                   if        not *in36  or wcablp='O'
061100070821     c                   EXSR      BRVDEL
061200090416   x5c                   else
061300061019     c
061400090416    6c                   if        blpdcm<=datcan
061500070821     c                   EXSR      BRVDEL
061600090416   x6c                   else
061700061019     c* altrimenti flaggo la spunta come non valida ai fini
061800061019     c*  del calcolo della responsabilità
061900090416    7c                   if        wfileerr=' '
062000070115     c                   eval      brvnvr='*'
062100070115     c                   update    fNbrv007
062200090416    7c                   endif
062300070821     c
062400090416    6c                   endif
062500090416    5c                   endif
062600090416    4C                   ENDIF
062700090416    3C                   ENDIF
062800100706   2AC                   endsl
062900941214     C*
063000950303   X2C                   ELSE
063100020906     C                   MOVEL     BLTSPE        SAVSPE
063200950120    2C                   ENDIF
063300941214     C*
063400950120    1C                   ENDIF
063500941214     C*
063600941214     C                   ENDSR
063700941214     C*
063800941214     C*---------------------------------------------------------------*
063900051007     C* cerco se bolla con c.a.
064000051007     C*---------------------------------------------------------------*
064100051007     c     CERCA         BEGSR
064200090417     C                   CLEAR                   WCABLP
064300051007     c                   call      'FIDN12R'
064400051007     c                   parm                    fidn12ds
064500051007      *
064600051007      * se non ci sono errori
064700051007    2c                   if        o12err = ' '
064800051007      * se numero di CA trovate maggiore di zero
064900051007    3c                   if        o12nca > 0
065000051007     c                   z-add     1             ii                3 0
065100051007    4c                   dow       skdca(II)>0 and wcablp=' '
065200051007    5c                   if        skdca(II)>datsca
065300051007     c                   movel     'S'           Wcablp
065400051007    5c                   endif
065500051007     c                   add       1             ii
065600051007    4c                   enddo
065700051007     c
065800051007     c* Se tutte le c.a. con data apertura < chain su dtc per controllare
065900070822     c*  che siano tutte LE APERTE in fase gestione SEDE
066000051007    4c                   if        wcablp=' '
066100051007     c                   z-add     1             ii
066200051007    5c                   dow       skdca(II)>0  and wcablp=' '
066300051007     c                   movel     skkey(II)     dskey
066400051007     c     kdct          chain     fndct01l
066500070822    6c                   if        %found(fndct01l) and dctgfc<>46  and
066600070822     c                             dctdch=0
066700051007     c                   movel     'S'           Wcablp
066800051007    6c                   endif
066900051007     c                   add       1             ii
067000051007    5c                   enddo
067100051007     c
067200051007     c                   if        wcablp=' '
067300051007     c                   eval      wcablp='O'
067400051007     c                   endif
067500051007     c
067600051007    4c                   endif
067700051007      *
067800051007    3c                   endif
067900051007      *
068000051007    2c                   endif
068100051007     c                   ENDSR
068200941214     C*---------------------------------------------------------------*
068300941214     C*                    RICARR                                     *
068400941214     C* CERCO NELLE BOLLE IN ARRIVO                                   *
068500020926     c* indicatori 32 e 34 ad uso esclusivo della routine ricarr
068600941214     C*---------------------------------------------------------------*
068700941214     C     RICARR        BEGSR
068800051007     c* Se anomalie  senza segnac.  ma solo con la spedizione (70 e 71)
068900020926     c*  devo comunque entrare nell'if per chainare fnarb
069000020926     c                   if        wgir='A' and brvnsc=0
069100020926     c                   eval      savse=*hival
069200020926     c                   endif
069300950303     C*
069400950303     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
069500950303    0C     BRVSE         IFNE      SAVSE
069600960508     C*
069700960508    1C     WGIR          IFEQ      'S'
069800960508     C     WGIR          OREQ      'A'
069900960508     C     ANMNSP        ANDEQ     0
070000950303     C     KBLT          CHAIN     FNART000                           32
070100960508     C*
070200960508   X1C                   ELSE
070300960514     C                   SETOFF                                       32
070400960508     C                   Z-ADD     ANMAAS        ARTAAS
070500960514     C                   MOVEL     ANMLNP        ARTLNP
070600960508     C                   MOVEL     BRVNRS        ARTNRS
070700960508     C                   MOVEL     ANMNSP        ARTNSP
070800960508    1C                   ENDIF
070900941214     C*
071000941214     C* AGGANCIO LA BOLLA
071100990126    1C  N32ARTSPE        IFNE      SAVSP2
071200950303     C     KARB          CHAIN     FNARB000                           32
071300051007     C* SE C'E' UNA C.A. cancello dalla data apertura c.a.
071400040806      *
071500090416    2c                   If        not *in32
071600040806     c                   clear                   fidn12ds
071700040806     c                   eval      i12aas = artaas
071800040806     c                   eval      i12lnp = artlnp
071900040806     c                   eval      i12nrs = artnrs
072000040806     c                   eval      i12nsp = artnsp
072100040806     c                   eval      i12fel = 'E'
072200040806     c                   eval      i12fan = 'E'
072300051007     c
072400051007     c                   EXSR      CERCA
072500040806      *
072600090416    2c                   endif
072700950303    1C                   ENDIF
072800950303     C*
072900950303     C                   MOVEL     BRVSE         SAVSE
073000950303    0C                   ENDIF
073100950120     C*
073200950303    1C     *IN32         IFEQ      *OFF
073300020906     c*
073400020906     c                   eval      $Found = *on
073500941214     C*
073600941214     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA
073700990126     C* E SE NON CI SONO C.A.
073800950120    2C     ARBDCM        IFGT      0
073900051007     C     WCAblp        ANDEQ     ' '
074000100706     c
074100100706     C* Elaboro anche se non consegnata senza C.A. ssolo spunte
074200100706    2C     ARBDCM        oreq      0
074300100706     C     WCABLP        ANDEQ     ' '
074400100706     C     WGIR          ANDEQ     'S'
074500100706     c
074600051007     c*  opure se ha c.a. con data < alla data di pulizia. cancello
074700051007     c*  anche le non consegnate
074800100706    2C     WCAblp        oreq      'O'
074900960509     C*
075000100706     c                   select
075100960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
075200960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
075300061018     c*   se non ha c.a., altrimenti seguo la pulizia per data c.a.
075400100706   2AC     PUDCS         whengt    ARBDCM
075500051007     c     arbdcm        andgt     0
075600061018     C     WCAblp        ANDEQ     ' '
075700960509     C*
075800090416     c                   select
075900090416     c                   when      wfileErr='S'
076000090416     c                   if        d53dfv<=datcan
076100090416     c                   delete    fnbrve
076200090416     c                   endif
076300090416     c                   when      WGIR='A'
076400090416     C                   EXSR      NOBOL2
076500090416     C                   other
076600090416     C                   EXSR      NOBOLL
076700090416     C                   ENDSL
076800090416     c*
076900090416     c* se spunta con "*" controllo direttamente la data consegna
077000090416     c*  con la data pulizia spunte con C.A.
077100100706   2ac                   when      brvnvr='*'   and arbdcm>0
077200090416     c
077300090416    3c                   if        arbdcm<=datcan
077400090416     c                   exsr      BRVDEL
077500090416    3c                   endif
077600090416     c
077700100706     c
077800100706     c* Bolla NON consegnata --> tengo almeno un anno la spunta poi cancello
077900100706   2ac                   when      arbdcm= 0  and wcablp=' '
078000100706    3c                   if        arbSPE<=datSCA  and PUDCS<=datSCA
078100100706     c                   EXSR      BRVDEL
078200100706    3c                   endif
078300100706     c
078400100706  x2ac                   other
078500160603     c
078600160603     c                   clear                   lna_r             1
078700160603     c* Se la linea di arrivo è a "rischio" devo tenere 4 mesi le spunte
078800160603     c                   if        wgir='S' and %lookup(bltlna:lnaR)>0
078900160603     c                   eval      lna_r='S'
079000160603     c                   endif
079100090416     C*
079200950303    3C     ARTSPE        IFNE      SAVSP2
079300160603     c     lna_r         andne     'S'
079400160603     c     wfileErr      andne     'S'
079500941214     C*
079600941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
079700941214     C*                  - DATA CONSEGNA PARZIALE
079800950303    4C     ARBCCA        IFEQ      *BLANKS
079900941214     C     ARBDCP        ANDEQ     *ZEROS
080000950303     C                   SETOFF                                       34
080100950303   X4C                   ELSE
080200950303     C                   SETON                                        34
080300950303    4C                   ENDIF
080400941214     C*
080500000614     C* NON HA EVENTI O SOLO QUELLI FASULLI
080600950303     C  N34              Z-ADD     ARTAAS        WAAS
080700000614     C     *IN34         IFEQ      *OFF
080800000614     C     KEVB2         SETLL     FNEVB000
080900000614     C     KEVB2         READE     FNEVB000                               98
081000000614     C     *IN98         DOWEQ     *OFF
081100000614     C     EVBCEV        LOOKUP    EFS                                    97
081200000614     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
081300000614     C     *IN97         IFEQ      *OFF
081400000614     C                   SETON                                        9834
081500000614     C                   ELSE
081600000614     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
081700000614     C     KEVB2         READE     FNEVB000                               98
081800000614     C                   ENDIF
081900000614     C                   ENDDO
082000000614     C                   ENDIF
082100941214     C*
082200941214     C* NON HA GIACENZA
082300050331     C  N34KARB          SETLL     Tigcp21l                               34
082400941214     C*
082500941214     C* NON HA LEGAMI BOLLA (MAMMA)
082600950303     C  N34KARB          SETLL     FNLBL000                               34
082700941214     C*
082800941214     C* NON HA LEGAMI BOLLA (FIGLIA)
082900950303     C  N34KARB          SETLL     FNLBL002                               34
083000950303     C*
083100020906     C                   MOVEL     ARTSPE        SAVSP2
083200950303    3C                   ENDIF
083300090416     c
083400090416     c* Imposto la data di pulizia :
083500090416    3c                   select
083600090416     c* 1) Spunte nel file degli errori
083700090416     c                   when      wfileErr='S'
083800090416     c                   eval      datapuliz=DATcan
083900160603     c* 2) linea a rischio: devo tenere 4 mesi
084000160603     c                   when      lna_r='S'
084100160603     c                   eval      datapuliz=DTPEXP_ymd
084200160603     C* 3) 34 OFF -    BOLLA SENZA EVENTI ANOMALI
084300090416     c                   when      *in34 =*off
084400090416     c                   eval      datapuliz=DATBRC
084500090416     c                   other
084600160603     C* 4)  altrimenti BOLLA CON   EVENTI ANOMALI
084700090416     c                   eval      datapuliz=DATBRV
084800090416    3c                   ENDSL
084900950120     C*
085000941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
085100090416    3C     ARBDCM        IFLE      Datapuliz
085200051007     c     arbdcm        andgt     0
085300051007     c
085400051007     c     arbdcm        oreq      0
085500051007     c     wcablp        andeq     'O'
085600051007     c
085700090416    4C     WGIR          IFEQ      'A'
085800990510     C                   DELETE    FNANM002
085900090416   x4C                   ELSE
086000061023     c* Se ha anomalia di spunta x danni, deleto solo se data <=data pulizia
086100061019     c*  spunte con anomalia
086200061023     c*
086300061023     c                   setoff                                       36
086400090416    5c                   if        brvcan<>' '
086500061023     c     brvcan        lookup    anom                                   36
086600090416    5c                   endif
086700061023     c
086800090416    5c                   if        not *in36   or Wcablp='O'
086900070821     c                   EXSR      BRVDEL
087000090416   x5c                   else
087100061019     c
087200090416    6c                   if        arbdcm<=datcan
087300070821     c                   EXSR      BRVDEL
087400090416   x6c                   else
087500061019     c* altrimenti flaggo la spunta come non valida ai fini
087600061019     c*  del calcolo della responsabilità
087700090416    7c                   if        wfileerr=' '
087800070115     c                   eval      brvnvr='*'
087900070115     c                   update    fnbrv007
088000090416    7c                   endif
088100070821     c
088200090416    6c                   endif
088300090416    5c                   endif
088400980114     C**
088500090416    4c                   ENDIF
088600090416    3C                   ENDIF
088700100706   2AC                   endsl
088800950120     C*
088900090416   x2C                   ELSE
089000020906     C                   MOVEL     ARTSPE        SAVSP2
089100950120    2C                   ENDIF
089200941214     C*
089300950120    1C                   ENDIF
089400941214     C*
089500941214     C                   ENDSR
089600941214     C*---------------------------------------------------------------*
089700941214     C*                    NOBOLL                                     *
089800941214     C* CANCELLAZIONE SPUNTE SENZA BOLLA                              *
089900941214     C*---------------------------------------------------------------*
090000941214     C     NOBOLL        BEGSR
090100051007     c
090200051007    1C     wfvv          IFNE      savfvv
090300051007     C                   CLEAR                   FNLV53DS
090400051007     C                   Z-ADD     BRvNPG        D53NPG
090500051007     C                   Z-ADD     BRvNFV        D53NFV
090600051007     C                   Z-ADD     BRvFGS        D53FGS
090700051007     C                   CALL      'FNLV53R'
090800051007     C                   PARM                    FNLV53DS
090900051007     C                   MOVE      wfvv          savfvv
091000051007     c                   movel     d53err        werr              1
091100051007     c                   movel     d53dfv        wDFV              8 0
091200051007    1C                   ENDIF
091300061023     c
091400061023     c                   setoff                                       36
091500061023     c                   if        brvcan<>' '
091600061023     c     brvcan        lookup    anom                                   36
091700061023     c                   endif
091800051007     c
091900051007    1c                   if        Werr=' '
092000061019     c*
092100051007    2C     Wdfv          IFLE      DATSPP
092200160601     c
092300160601     c* linea a rischio: tengo di più
092400160601   2ac                   if        %lookup(arblna:lnaR)=0   or
092500160601     c                             wdfv <= dtpexp_ymd
092600160601     c
092700061019     c
092800061019     C                   MOVE      BRVSE         WBRVSE
092900061023    3c                   if        not *in36
093000070821     c                   EXSR      BRVDEL
093100061019   x3c                   else
093200061019    4c                   if        wdfv<=datcan
093300070821     c                   EXSR      BRVDEL
093400061019   x4c                   else
093500070821    5c                   if        wfileerr=' '
093600070115     c                   eval      brvnvr='*'
093700070115     c                   update    fnbrv007
093800070821    5c                   endif
093900070821     c
094000070821    4c                   endif
094100061019    3c                   endif
094200061019     c
094300160601   2aC                   ENDIF
094400160601    2C                   ENDIF
094500051007     C*
094600051007     c                   else
094700051007     c* se non trovato foglio pulisco in base alla data spunta
094800061019     c
094900061019    2C     PUDCS         IFLE      DATSPP
095000160601
095100160601     c* linea a rischio: tengo di più
095200160601   2ac                   if        %lookup(arblna:lnaR)=0   or
095300160601     c                             pudcs <= dtpexp_ymd
095400160601     c
095500061019     C                   MOVE      BRVSE         WBRVSE
095600061019     c
095700061023    3c                   if        not *in36
095800070821     c                   EXSR      BRVDEL
095900061019   x3c                   else
096000061019    4c                   if        pudcs<=datcan
096100070821     c                   EXSR      BRVDEL
096200061019   x4c                   else
096300070821    5c                   if        wfileerr=' '
096400070115     c                   eval      brvnvr='*'
096500070115     c                   update    fnbrv007
096600070821    5c                   endif
096700070821     c
096800070821    4c                   endif
096900061019    3c                   endif
097000061019     c
097100160601   2aC                   ENDIF
097200160601    2C                   ENDIF
097300051007    1C                   ENDIF
097400941214     C*
097500941214     C                   ENDSR
097600970611     C*---------------------------------------------------------------*
097700970611     C* CANCELLAZIONE EVENTI SEGNACOLLI                               *
097800970611     C*---------------------------------------------------------------*
097900970611     C     DELEVS        BEGSR
098000970611     C* VERIFICO SE PER IL SEGNACOLLO ESISTONO ANCORA DELLE SPUNTE E
098100970611     C* SE E' PRESENTE ALMENO UNA SPUNTA NON CANCELLO GLI EVENTI
098200070115     C     KBRV          SETLL     FNBRV07L                               35
098300970611     C     *IN35         IFEQ      *OFF
098400970611     C     *IN30         DOUEQ     *ON
098500970611     C     KEVS          DELETE    FNEVS000                           30
098600970611     C                   ENDDO
098700970611     C                   END
098800970611     C* MI RIPOSIZIONO SULLA SPUNTA CHE STAVO LEGGENDO
098900070115     C     KBRV7         SETLL     FNBRV07L
099000070115     C     KBRV7         READE     FNBRV07L                               30
099100970611     C                   ENDSR
099200960506     C*
099300960506     C*---------------------------------------------------------------*
099400960506     C*                    NOBOL2                                     *
099500960506     C* CANCELLAZIONE:  - ANOMALIE SENZA BOLLA                        *
099600960506     C*                 - ANOMALIE SENZA SEGNACOLLO E SPEDIZIONE      *
099700960506     C*                 - ANOMALIE CON TIPO ANOMALIA "T"              *
099800960506     C*---------------------------------------------------------------*
099900960506     C     NOBOL2        BEGSR
100000960506     C* PRENDO LA MAGGIORE TRA DATA APERTURA E DATA CHIUSURA
100100960506     C     ANMDCH        IFGT      ANMDAO
100200020906     C                   Z-ADD     ANMDCH        W0080
100300960506     C                   ELSE
100400990510     C                   Z-ADD     ANMDAO        W0080
100500960506     C                   ENDIF
100600960506     C*
100700960508     C*
100800960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
100900960903     C*                           CANCELLARE L'ANOMALIA
101000960903     C     ANMDAO        IFEQ      0
101100990510     C                   Z-ADD     19000101      W0080
101200960903     C                   ENDIF
101300960903     C*
101400990510    1C     W0080         IFLE      DATSPP
101500990510     C                   DELETE    FNANM002
101600960506    1C                   ENDIF
101700960506     C*
101800960506     C                   ENDSR
101900960506     C*
102000070821     C*---------------------------------------------------------------*
102100070821     c* Cancella record di FNBRV o FNBRVE
102200070821     C*---------------------------------------------------------------*
102300070821     c     BRVDEL        BEGSR
102400070821     c                   if        wfileerr=' '
102500070821     C                   DELETE    FNBRV007
102600070821     c                   else
102700070821     c                   delete    FNBRVE
102800070821     c                   endif
102900070821     C                   ENDSR
103000930519     C*---------------------------------------------------------------*
103100930521     C*                    CARTAB                                     *
103200930521     C* ROUTINE DI CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE  *
103300930519     C*---------------------------------------------------------------*
103400930521     C     CARTAB        BEGSR
103500981008     C***
103600981008     C* EVENTI CHE SONO FASULLI DA NON CONSIDERARE PER LA PULIZIA
103700981008     C***
103800981008     C                   MOVEL     '2Z'          COD
103900020906     C                   Z-ADD     0             XI
104000981008     C     KTAB          SETLL     TABEL
104100981008     C     KTAB          READE     TABEL                                  31
104200981008     C*
104300981008DO  1C     *IN31         DOWEQ     '0'
104400981008IF  2C     TBLFLG        IFEQ      ' '
104500981008     C                   MOVEL     TBLUNI        DS2Z
104600981008     C*
104700981008IF  3C     §2ZEFS        IFEQ      'S'
104800020906     C                   ADD       1             XI
104900020906     C                   MOVEL     TBLKEY        EFS(XI)
105000981008E   3C                   ENDIF
105100981008E   2C                   ENDIF
105200981008     C*
105300981008     C     KTAB          READE     TABEL                                  31
105400981008E   1C                   ENDDO
105500930111     C*
105600960506     C***
105700960506     C* ANOMALIE  IDD
105800960506     C***
105900960506     C                   MOVEL     '7C'          COD
106000020906     C                   Z-ADD     0             XI
106100020906     C                   Z-ADD     0             XX
106200960506     C     KTAB          SETLL     TABEL
106300960506     C     KTAB          READE     TABEL                                  31
106400960506     C*
106500960506DO  1C     *IN31         DOWEQ     '0'
106600960506IF  2C     TBLFLG        IFEQ      ' '
106700960506     C                   MOVEL     TBLUNI        DS7C
106800960506     C*
106900960506IF  3C     §7CSAN        IFEQ      ' '
107000020906     C                   ADD       1             XI
107100020906     C                   MOVEL     TBLKEY        SAN(XI)
107200960506E   3C                   ENDIF
107300961106     C*
107400961106     C* CARICO LE ANOMALIE ESTERNE AUTOCHIUSE
107500961106IF  3C     §7CAIE        IFEQ      'E'
107600961106     C     §7CCHA        ANDEQ     'S'
107700020906     C                   ADD       1             XX
107800020906     C                   MOVEL     TBLKEY        CHA(XX)
107900961106E   3C                   ENDIF
108000960506E   2C                   ENDIF
108100960506     C*
108200960506     C     KTAB          READE     TABEL                                  31
108300960506E   1C                   ENDDO
108400061023     C***
108500061023     C* carico le anomalie di spunte per DANNI
108600061023     C***
108700061023     C                   MOVEL     '3E'          COD
108800061023     C                   Z-ADD     0             XI
108900061023     C     KTAB          SETLL     TABEL
109000061023     C     KTAB          READE     TABEL                                  31
109100061023     C*
109200061023DO  1C     *IN31         DOWEQ     '0'
109300061023IF  2C     TBLFLG        IFEQ      ' '
109400061023     C                   MOVEL     TBLUNI        DS3E
109500061023     C*
109600061023     c* Escludo le anomlie di rientro che servono per la
109700061023     c*  chiusura distinta
109800061023IF  3C     §3eFTA        IFne      'R'
109900061023     C                   ADD       1             XI
110000061023     C                   MOVEL     TBLKEY        ANOM(XI)
110100061023E   3C                   ENDIF
110200061023E   2C                   ENDIF
110300061023     C*
110400061023     C     KTAB          READE     TABEL                                  31
110500061023E   1C                   ENDDO
110600960506     C*
110700930524     C                   SETOFF                                       31
110800950403     C***
110900950403     C* DETERMINO LA DATA DI PULIZIA PIU' RECENTE
111000950403     C***
111100950403     C                   SELECT
111200950403     C     DATSPP        WHENGE    DATBRV
111300950403     C     DATSPP        ANDGE     DATBRC
111400950403     C                   Z-ADD     DATSPP        WDAT
111500950403     C*
111600950403     C     DATBRV        WHENGE    DATSPP
111700950403     C     DATBRV        ANDGE     DATBRC
111800950403     C                   Z-ADD     DATBRV        WDAT
111900950403     C*
112000950403     C                   OTHER
112100950403     C                   Z-ADD     DATBRC        WDAT
112200950403     C                   ENDSL
112300950403     C*
112400930111     C                   ENDSR
112500070821     C*---------------------------------------------------------------*
112600070821     C*                    CerDATbrv                                  *
112700070821     C* Cerco la data più recente della spunta letta                  *
112800070821     C*---------------------------------------------------------------*
112900070821     c     CerDATbrv     BEGSR
113000070821    3c                   Select
113100070821     c                   When      BrvDfs > *Date and BrvDcs > *Date
113200070821     c                   If        BrvDfs > BrvDcs
113300070821     c                   Eval      Pudcs = BrvDcs
113400070821     c                   Else
113500070821     c                   Eval      Pudcs = BrvDfs
113600070821     c                   EndIf
113700070821      * Prendo la data caricamento se data immissione maggiore di udate
113800070821     c                   When      BrvDfs > *Date
113900070821     c                   Eval      Pudcs = BrvDcs
114000070821      * Prendo la data immissione se data caricamento maggiore di udate
114100070821     c                   When      BrvDcs > *Date
114200070821     c                   Eval      Pudcs = BrvDfs
114300070821
114400070821     c                   Other
114500070821     C* PRENDO LA DATA PIÙ ALTA FRA QUELLA DI CARICAMENTO E QUELLA
114600070821     C* DI IMMISSIONE/VARIAZIONE
114700070821     C     BRVDFS        IFGE      BRVDCS
114800070821     C                   Z-ADD     BRVDFS        PUDCS
114900070821     C                   ELSE
115000070821     C                   Z-ADD     BRVDCS        PUDCS
115100070821     C                   ENDIF
115200070821    3c                   EndSl
115300070821     c                   ENDSR
115400930519     C*---------------------------------------------------------------*
115500930519     C*                    DEFVAR                                     *
115600930519     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
115700930519     C*---------------------------------------------------------------*
115800930519     C     DEFVAR        BEGSR
115900930519     C*
116000930519     C     *ENTRY        PLIST
116100930519     C                   PARM                    KPJBA
116200100504      *
116300100504     c     *dtaara       define    §azute        AZUTEds
116400100504     c     *dtaara       define    §datiute      dDATIUTE
116500100504     c                   in(E)     *dtaara
116600100504     c                   if        %ERROR or RSUT = *blanks
116700100504     c                   clear                   Tibs34Ds
116800100504     c                   call      'TIBS34R'
116900100504     c                   parm                    Tibs34Ds
117000100504     c                   in        *dtaara
117100100504     c                   endif
117200100504     C*
117300100504     c
117400100504     C                   CALL      'FNLV61R'
117500100504     C                   PARM                    O61DRF            8 0
117600100618     C                   PARM      ' '           i61Rep            1
117700100618     C                   PARM                    dvpopuladd
117800100618     c
117900100504     C                   z-add     o61drf        G02inv
118000100504     C                   MOVEL     '3'           G02ERR
118100100504     C                   CALL      'XSRDA8'
118200100504     C                   PARM                    WLBDAT
118300100618     c
118400100618     c* Se ci sono aggiuntivi di pulizia da sottrarre --> tolti anche questi
118500100504     c
118600100504     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A
118700100504     C                   MOVEL     '5A'          COD
118800100504     C                   MOVEL     '1       '    KEY
118900100504     C     KTAB2         CHAIN     TABEL                              30
119000100504     C  N30              MOVEL     TBLUNI        DS5A
119100100504     C   30              MOVEL     *ZEROS        DS5A
119200100504     C*
119300100504     C* CALCOLO DATE PULIZIA SPUNTE CON BOLLE CONSEGNATE SENZA EVENTI
119400100504     C*   ANOMALI E CON EVENTI ANOMALI
119500100504     C                   EXSR      CALDAT
119600100504     C*
119700100504     C* PULIZIA SPUNTE :  SOTTRAGGO §5ASPP AI GIORNI
119800100504     C*   PULIZIA SPUNTE DI DISGUIDI O BOLLE TRANSITO
119900100504     C     G02TGI        SUB       §5ASPP        GIOTGI
120000100618     c                   sub       §vpogg        GIOTGI
120100100504     C*
120200100504     C                   CALL      'XSRGI8'
120300100504     C                   PARM                    WGIDAT
120400100504     C                   Z-ADD     GIOINV        datSPP
120500100504      *
120600100504      * pulizia spunte con c.a.: sottraggo §5ASCA ai giorni
120700100504     c     G02tgi        sub       §5ASca        GIOtgi
120800100504     c                   call      'XSRGI8'
120900100504     c                   parm                    WGIdat
121000100504     c                   z-add     GIOinv        datSCA
121100100504      *
121200100504      * pulizia spunte con BRVCAN impostato: sottatto §5ABLP ai giorni
121300100504     c*  tengo infatti le spunte come le bolle partenza
121400100504     c     G02tgi        sub       §5Ablp        GIOtgi
121500100618     c                   sub       §vpogg        GIOTGI
121600100504     c                   call      'XSRGI8'
121700100504     c                   parm                    WGIdat
121800100504     c                   z-add     GIOinv        datcan
121900100504     c*
122000930519     C*
122100941214     C* LETTURA TABEL00F
122200941214     C     KTAB          KLIST
122300051007     C                   KFLD                    CODUT             1 0
122400941214     C                   KFLD                    COD
122500100504     C     KTAB2         KLIST
122600100504     C                   KFLD                    CODUT
122700100504     C                   KFLD                    COD
122800100504     C                   KFLD                    KEY
122900100504     C*
123000941214     C* LETTURA FNBLT27L
123100941214     C     KBLT          KLIST
123200941214     C                   KFLD                    BRVLNP
123300941214     C                   KFLD                    BRVLNA
123400941214     C                   KFLD                    BRVNRS
123500941214     C                   KFLD                    BRVNSC
123600941214     C* LETTURA FNBLP01L
123700941214     C     KBLP          KLIST
123800941214     C                   KFLD                    BLTAAS
123900941214     C                   KFLD                    BLTLNP
124000941214     C                   KFLD                    BLTNRS
124100941214     C                   KFLD                    BLTNSP
124200941214     C* LETTURA FNEVB01L
124300941214     C     KEVB          KLIST
124400941214     C                   KFLD                    WAAS
124500941214     C                   KFLD                    BLTLNP
124600941214     C                   KFLD                    BLTNRS
124700941214     C                   KFLD                    BLTNSP
124800941214     C     KEVB2         KLIST
124900941214     C                   KFLD                    WAAS
125000941214     C                   KFLD                    ARTLNP
125100941214     C                   KFLD                    ARTNRS
125200941214     C                   KFLD                    ARTNSP
125300941214     C* LETTURA FNARB01L
125400941214     C     KARB          KLIST
125500941214     C                   KFLD                    ARTAAS
125600941214     C                   KFLD                    ARTLNP
125700941214     C                   KFLD                    ARTNRS
125800941214     C                   KFLD                    ARTNSP
125900970611     C* LETTURA FNEVS02L
126000970611     C     KEVS          KLIST
126100040324     C                   KFLD                    WRVLNP
126200040324     C                   KFLD                    WRVLNA
126300040324     C                   KFLD                    WRVNRS
126400040324     C                   KFLD                    WRVNSC
126500070115     C* POSIZIONAMENTO FNBRV07L
126600970611     C     KBRV          KLIST
126700970611     C                   KFLD                    WRVLNP
126800970611     C                   KFLD                    WRVLNA
126900970611     C                   KFLD                    WRVNRS
127000970611     C                   KFLD                    WRVNSC
127100970611     C     KBRV7         KLIST
127200970611     C                   KFLD                    BRVLNP
127300970611     C                   KFLD                    BRVLNA
127400970611     C                   KFLD                    BRVNRS
127500970611     C                   KFLD                    BRVNSC
127600051007     C     Kdct          KLIST
127700051007     C                   KFLD                    dctaac
127800051007     C                   KFLD                    dctfil
127900051007     C                   KFLD                    dctnca
128000930521     C*
128100930519     C                   ENDSR
128200100618     c
128300100504     C*--- CALCOLO DATE DI PULIZIA SPUNTE ----------------------------*
128400100504     C     CALDAT        BEGSR
128500100504     C*
128600100504     C* GIORNI PULIZIA
128700100504     C                   MOVEL     §5ABRV        COMBRV
128800100504     C                   MOVEL     §5ABRC        COMBRC
128900100618     c* addiziono eventuali gg lavorativi aggiuntivi
129000100618     c                   add       §vpogglav     COMBRV
129100100618     c                   add       §vpogglav     COMBRC
129200100618     c
129300100618     C****
129400100618     C** GIORNI DATA CONSEGNA PER BOLLE CON   EVENTI ANOMALI
129500100618     C****
129600100618     c                   clear                   xgiolavds
129700100618     c                   eval      ixgldata=o61drf
129800100618     c                   eval      ixglfil =simfel
129900100618     c                   eval      ixglpa  ='A'
130000100618     c                   eval      ixglsub ='S'
130100100618     C* AGGIUNGO 1 AI GIORNI DI PULIZIA CONTENUTI IN TABELLA
130200100618     c                   eval      ixglgg  =combrv+1
130300100618     c                   eval      ixgllav ='S'
130400100618     c                   call      'XGIOLAV'
130500100618     c                   parm                    xgiolavds
130600100618     c                   Move      oxgldata      datbrv
130700100618
130800100618     C****
130900100618     C** GIORNI DATA CONSEGNA PER BOLLE SENZA EVENTI ANOMALI
131000100618     C****
131100100618     c                   clear                   xgiolavds
131200100618     c                   eval      ixgldata=o61drf
131300100618     c                   eval      ixglfil =simfel
131400100618     c                   eval      ixglpa  ='A'
131500100618     c                   eval      ixglsub ='S'
131600100618     C* AGGIUNGO 1 AI GIORNI DI PULIZIA CONTENUTI IN TABELLA
131700100618     c                   eval      ixglgg  =combrc+1
131800100618     c                   eval      ixgllav ='S'
131900100618     c                   call      'XGIOLAV'
132000100618     c                   parm                    xgiolavds
132100100618     c                   Move      oxgldata      datbrc
132200160601     c
132300160601     c* bolle export a "rischio" 4 mesi
132400160601     c                   move      o61drf        data_iso
132500160601     c                   subdur    4:*m          data_iso
132600160601     c                   move      data_iso      dtpEXP_ymd        8 0
132700160601     c
132800100618
132900100504     C                   ENDSR
133000160601**
133100160601333334360000000000000000000000
