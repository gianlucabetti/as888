000100021105     H DECEDIT('0,') DATEDIT(*ymd.)
000200020906      * FNLR10R *-----------------------------------------------------*
000300020906      *           PULIZIA SPUNTE E ANOMALIE                           *
000400020906      *---------------------------------------------------------------*
000500100504     fAzcln01l  if   e           k Disk
000600070115     FFNBRV07L  UF   E           K DISK
000700070115     F                                     RENAME(FNBRV000:FNBRV007)
000800070115     FFNBRVE1L  UF   E           K DISK    USROPN
000900070115     F                                     RENAME(FNBRV000:FNBRVE)
001000990510     FFNANM02L  UF   E           K DISK
001100990510     F                                     RENAME(FNANM000:FNANM002)
001200970611     FFNEVS02L  UF   E           K DISK
001300941214     FFNBLP01L  IF   E           K DISK
001400960613     FFNBLT27L  IF   E           K DISK
001500050331     FTigcp01L  IF   E           K DISK
001600941214     FFNARB01L  IF   E           K DISK
001700941214     FFNART27L  IF   E           K DISK
001800050331     FTigcp21L  IF   E           K DISK
001900050331     F                                     RENAME(TIGCP000:TIGCP21)
002000990804     FFNEVB01L  IF   E           K DISK
002100941214     FFNLBL01L  IF   E           K DISK
002200941214     FFNLBL02L  IF   E           K DISK
002300941214     F                                     RENAME(FNLBL000:FNLBL002)
002400930111     FTABEL00F  IF   E           K DISK
002500020906     FAZORG01L  IF   E           K DISK
002600051007     Ffndct01l  IF   E           K DISK
002700020906
002800960506     D*
002900960506     D** DEFINIZIONE SCHIERE
003000020808     D*
003100080903     D EFS             S              3    DIM(999)
003200061023     D anom            S              1    DIM(50)
003300960506     D SAN             S              3  0 DIM(30)                              SOGGETTO ANOMALIA
003400961106     D CHA             S              3  0 DIM(30)                              AN.ESTERNE AUTOCHIUS
003500020906
003600960506     D*
003700020906     D** DEFINIZIONE STRUTTURE DATI ESTERNE
003800020808     D*
003900930519     D KPJBA         E DS
004000100504      * - Parametri x Controllo profilo utenti
004100100504     d TIBS34ds      e ds                  inz
004200100504      * - Ds di riferimento al file esterno AZUTE00F
004300100504     d AZUTEds       e ds                  inz extname(AZUTE00F)
004400100504      * - Ds per dati organigramma
004500100504     d dDATIUTE      e ds                  inz
004600100504      *
004700020808     D FNLV53DS      E DS
004800020808     D DS2Z          E DS
004900061023     D DS3E          E DS
005000020808     D DS7C          E DS
005100100504     D DS5A          E DS
005200100504     d Ds1y          e ds                  inz
005300040806      * ds per il controllo della presenza di CA per la spedizione richiesta
005400040806     d FIDN12DS      e ds
005500051007     d  skKey                 26    305    dim(20)
005600051007     d  skDca                326    485  0 dim(20)
005700051007     d
005800051007     d dsKey           ds
005900051007     d  dctaac                        4  0
006000051007     d  dctfil                        3  0
006100051007     d  dctnca                        7  0
006200960506     D*
006300960506     D** PARAMETRI PASSATI DAL FILTRO
006400100504     D*PARAM           DS
006500950120     D* DATA PULIZIA PER SPUNTE SENZA BOLLA
006600100504     D**DATSPP                 5     12  0
006700950120     D* DATA PULIZIA PER SPUNTE CON BOLLE CON   EVENTI ANOMALI
006800100504     D**DATBRV                13     20  0
006900950120     D* DATA PULIZIA PER SPUNTE CON BOLLE SENZA EVENTI ANOMALI
007000100504     D*+DATBRC                21     28  0
007100100504     D* DATA PULIZIA PER SPUNTE CON BOLLE  con c.a. dalla data apertura c.a.
007200100504     D**DATSCA                45     52  0
007300061019     D* DATA PULIZIA PER SPUNTE CON BOLLE  con BRVCAN<>' '              c.a.
007400061019     d*  pulisco stessi giorni delle bolle partenza (30)
007500100504     D**DATcan                53     60  0
007600020808     D*
007700950303     D                 DS
007800950303     D  BRVLNP                 1      3  0
007900950303     D  BRVLNA                 4      6  0
008000950303     D  BRVNRS                 7      8  0
008100950303     D  BRVNSC                 9     15  0
008200950303     D  BRVSE                  1     15  0
008300970611     D                 DS
008400970611     D  WRVLNP                 1      3  0
008500970611     D  WRVLNA                 4      6  0
008600970611     D  WRVNRS                 7      8  0
008700970611     D  WRVNSC                 9     15  0
008800970611     D  WBRVSE                 1     15  0
008900950303     D                 DS
009000950303     D  BLTAAS                 1      4  0
009100950303     D  BLTLNP                 5      7  0
009200950303     D  BLTNRS                 8      9  0
009300950303     D  BLTNSP                10     16  0
009400950303     D  BLTSPE                 1     16  0
009500950303     D                 DS
009600950303     D  ARTAAS                 1      4  0
009700950303     D  ARTLNP                 5      7  0
009800950303     D  ARTNRS                 8      9  0
009900950303     D  ARTNSP                10     16  0
010000950303     D  ARTSPE                 1     16  0
010100950403     D                 DS
010200950404     D  BRVNPG                 1      1  0
010300020916     d  BrvFgs                 2      4  0
010400000125     D  BRVNFV                 5     10  0
010500000125     D  WFVV                   1     10  0
010600960506     D*
010700100504     D WLBDAT          DS                  inz
010800100504     D  G02DAT                 1      8  0 inz
010900100504     D  G02INV                 9     16  0 inz
011000100504     D  G02ERR                17     17    inz
011100100504     D  G02TGI                18     22  0 inz
011200100504     D WGIDAT          DS                  inz
011300100504     D  GIODAT                 1      8  0 inz
011400100504     D  GIOINV                 9     16  0 inz
011500100504     D  GIOTGI                17     21  0 inz
011600100504
011700100504     d ClnMat          ds
011800100504     d  Mat                    1     31    Dim(31)
011900100504     d ClnPom          ds
012000100504     d  Pom                    1     31    Dim(31)
012100100504
012200020906
012300020906     D*
012400020906     D** DEFINIZIONE CAMPI
012500020906     D*
012600100504     d CODUT           s                   like(TBLkut) inz(1)
012700100504     d KEY             s                   like(TBLKEY) inz
012800020906     D COD             S                   LIKE(TBLCOD) inz
012900020906     D PUDCS           S                   LIKE(BRVDCS) inz
013000020906     D PULNP           S                   LIKE(BRVLNP) inz
013100020906     D WAAS            S                   LIKE(EVBAAS) inz
013200020906     D WDAT            S                   LIKE(DATSPP) inz
013300020906     D SAVFVV          S                   LIKE(WFVV)   inz
013400020906     D SAVBRC          S                   LIKE(DATBRC) inz
013500020906     D WMAX            S                   LIKE(TBLKEY) inz
013600020906     D SESAV           S                   LIKE(BRVSE)  inz
013700070821     D FVSAV           S                   LIKE(WFVV)   inz
013800020906     D XI              S              2  0
013900020906     D XX              S              3  0
014000070821     D WFILEERR        S              1    inz
014100070821     D WOK             S              1    inz
014200020906     D WGIR            S              1    inz
014300020906     D W0080           S              8  0 inz
014400020906     D WCABLP          S              1    inz
014500020906     D SAVse           S             15  0 inz
014600020925     D SAVpar          S             15  0 inz
014700020906     D SAVspe          S             16  0 inz
014800020906     D SAVsp2          S             16  0 inz
014900020906      * Flag "Trovata bolla"
015000020906     d $Found          s              1    inz(*off)
015100090416     d DataPuliz       s              8  0 inz
015200100504     d COMBRV          s                   like(§5ABRV) inz
015300100504     d COMBRC          s                   like(§5ABRC) inz
015400100504     d ktfp            s                   like(clntfp) inz
015500100504     d ktfa            s                   like(clntfa) inz
015600100504     d kann            s                   like(clnann) inz
015700100504     d kmes            s                   like(clnmes) inz
015800100504     d Wdata           s               d   Datfmt(*iso) inz
015900100504     d Wgiorno         s              2  0 inz
016000100504     d datbrv          s              8  0 inz
016100100504     d datbrc          s              8  0 inz
016200100504     d datSPP          s              8  0 inz
016300100504     d datSca          s              8  0 inz
016400100504     d datcan          s              8  0 inz
016500020906
016600960506     I*
016700990510     I** RIDENOMINAZIONE CAMPI FNANM
016800990510     IFNANM002
016900960430     I              ANMLNA                      BRVLNA
017000960430     I              ANMNRS                      BRVNRS
017100960430     I              ANMSCN                      BRVNSC
017200020906
017300941214     C*---------------------------------------------------------------*
017400941214     C* INDICATORI USATI                                              *
017500941214     C*---------------------------------------------------------------*
017600960506     C* 21    - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE OPPURE CON
017700960506     C*         TIPO ANOMALIA "T"
017800961106     C* 22    - ANOMALIA ESTERNA AUTOCHIUSA
017900020926     C* 31 33 - uso esclusivo della RICPAR
018000020926     C* 32 34 - uso esclusivo della RICARR
018100061023     C* 36    - controllo se brvcan ha anomalia x danni
018200020926     C* 30/39 - CHAIN SUI FILES
018300930519     C*---------------------------------------------------------------*
018400930519     C*
018500930519     C* DEFINIZIONE VARIABILI, KLIST E PLIST
018600930520     C                   EXSR      DEFVAR
018700100513     c*
018800100513   -1c                   if        datbrv>0 and datbrc>0 and datspp>0
018900930520     C*
019000930521     C* CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE
019100930521     C                   EXSR      CARTAB
019200950303     C*
019300970611     C                   CLEAR                   WBRVSE
019400970611     C*
019500960506     C* ESEGUO 2 VOLTE IL CICLO DEL PGM: -PER LA PULIZIA DELLE ANOMALIE
019600960506     C*                                  -PER LA PULIZIA DELLE SPUNTE
019700020906     C                   MOVEL     'A'           WGIR
019800960506     C*
019900960515    0C                   DO        2
020000950303     C                   CLEAR                   SAVSE
020100020925     C                   CLEAR                   SAVpar
020200970611     C                   CLEAR                   SESAV
020300950303     C                   CLEAR                   SAVSPE
020400950303     C                   CLEAR                   SAVSP2
020500950403     C                   CLEAR                   SAVFVV
020600070821     C                   CLEAR                   WfileERR
020700941214     C*
020800960430     C* LETTURA  A N O M A L I E
020900980217     C*  PER LE ANOMALIE PULIAMO TUTTO CON LA DATA PIU' ALTA
021000960430    1C     WGIR          IFEQ      'A'
021100960515     C                   MOVEL     DATBRC        SAVBRC
021200960515     C                   MOVEL     DATBRV        DATBRC
021300960515     C*
021400990510     C     *LOVAL        SETLL     FNANM002
021500990510     C                   READ      FNANM002                             9930
021600990510     C   99              READ(N)   FNANM002                               30
021700960515     C*
021800960430   X1C                   ELSE
021900960515     C                   MOVEL     SAVBRC        DATBRC
022000960430     C* LETTURA  S P U N T E
022100070115     C     *LOVAL        SETLL     FNBRV007
022200070115     C                   READ      FNBRV007                             9930
022300070115     C   99              READ(N)   FNBRV007                               30
022400960430    1C                   ENDIF
022500941214     C*
022600941214    1C     *IN30         DOWEQ     *OFF
022700960730     C*
022800960730     C* SOLO SE SONO RIUSCITO AD ALLOCARE IL RECORD
022900960730   1AC     *IN99         IFEQ      *OFF
023000960430     C*
023100990510     C* SOLO PER  A N O M A L I E :
023200960506    2C     WGIR          IFEQ      'A'
023300960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
023400960903     C*                           CANCELLARE L'ANOMALIA
023500980217    3C     ANMDAO        IFEQ      0
023600990510     C                   Z-ADD     19000101      PUDCS
023700980217   X3C                   ELSE
023800990510     C                   Z-ADD     ANMDAO        PUDCS
023900980217    3C                   ENDIF
024000980217     C**
024100980217   X2C                   ELSE
024200021105      * Prendo la data minore tra caricamento e immissione se tutte e 2 sono
024300021105      * maggiori di udate
024400070821     c                   EXSR      CerDATbrv
024500070821     c
024600980217    2C                   ENDIF
024700960430     C*
024800980217     C* SE LA DATA APERTURA ANOMALIA O
024900980217     C*    LA DATA DI CARICAMENTO SPUNTA E' > DELLA DATA PIU' RECENTE
025000950403     C*  DI PULIZIA --> NON LA CONSIDERO NEMMENO
025100980217    2C     PUDCS         IFLE      WDAT
025200020906     C                   MOVEL     'S'           WOK
025300941214     C*
025400960430     C* SOLO PER  A N O M A L I E
025500960430    3C     WGIR          IFEQ      'A'
025600960506     C*
025700960506     C* 21 ON  - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE
025800960506     C     ANMCAA        LOOKUP    SAN                                    21
025900960506     C*
026000960430     C* LE ANOMALIE "E/R" SI PULISCONO SOLO SE HANNO LA DATA CHIUSURA
026100960506    4C  N21ANMAIE        IFEQ      'E'
026200960430     C     ANMAIE        OREQ      'R'
026300961106     C                   SETOFF                                       22
026400961106     C*
026500961106     C* 22 ON  - SI TRATTA DI ANOMALIA ESTERNA AUTOCHIUSA
026600961106    5C     ANMAIE        IFEQ      'E'
026700961106     C     ANMCAA        ANDNE     70
026800961106     C     ANMCAA        ANDNE     71
026900961106     C     ANMCAA        LOOKUP    CHA                                    22
027000961106    5C                   ENDIF
027100961106     C*
027200961106    5C     ANMAIE        IFEQ      'E'
027300961106     C     *IN22         ANDEQ     *OFF
027400980217     C**
027500980217     C* VERIFICO SE LNA FA O MENO L'IDD
027600020906     c                   eval      XX = 1
027700050805     c**   BRVlna        lookup    SKpo(xx)                               37
027800050805     c** 37              movel     NIDD(xx)      LNANID
027900050805     c**N37              movel     'N'           LNANID
028000980217     C**
028100050805    6C**   LNANID        IFNE      'N'
028200980217    7C     ANMDCH        IFEQ      0
028300980217     C     ANMFT1        OREQ      ' '
028400980217     C     ANMFL1        ANDGT     0
028500980217     C     ANMFT2        OREQ      ' '
028600980217     C     ANMFL2        ANDGT     0
028700980217     C                   CLEAR                   WOK
028800980217    7C                   ENDIF
028900050805    6C**                 ENDIF
029000050805    5C                   ENDIF
029100961106     C*
029200960430   X4C                   ELSE
029300960506     C* 21 ON  - ANOMALIA CON TIPO ANOMALIA "T"
029400960506     C     ANMAIE        IFEQ      'T'
029500960506     C                   SETON                                        21
029600960430     C                   ENDIF
029700960430    4C                   ENDIF
029800960430    3C                   ENDIF
029900960430     C*
030000960430     C* WOK = "S" ---> RECORD DA ELABORARE
030100960506    3C     WOK           IFEQ      'S'
030200960430     C*
030300960506    4C     *IN21         IFEQ      *OFF
030400960530     C*
030500960506    5C     WGIR          IFEQ      'A'
030600960506     C     ANMLNP        IFGT      0
030700960506     C                   MOVEL     ANMLNP        BRVLNP
030800960506     C                   ELSE
030900960506     C                   MOVEL     ANMFLS        BRVLNP
031000960506     C                   ENDIF
031100960506    5C                   ENDIF
031200960530     C*
031300970611     C*
031400970611     C* A CAMBIO SEGNACOLLO:
031500970611     C     WGIR          IFEQ      'S'
031600970611     C     BRVSE         ANDNE     SESAV
031700970611     C* Verifico se per il collo precedente devo cancellare gli eventi
031800970611     C     WBRVSE        IFGT      *ZEROS
031900970611     C                   EXSR      DELEVS
032000970611     C                   CLEAR                   WBRVSE
032100970611     C                   END
032200970611     C                   MOVE      BRVSE         SESAV
032300970611     C                   END
032400941214     C*
032500020906     c                   eval      $Found = *off
032600941214     C* CERCO NELLE BOLLE IN ARRIVO
032700941214     C                   EXSR      RICARR
032800020906     c*
032900020906     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
033000020906     c                   if        $Found = *off
033100020906     c                   exsr      RICPAR
033200020906     c                   endif
033300020925     c                   else
033400020925     c* se anomalia senza spedizione o di tipo "T" eseguo solo il ciclo
033500020925     c*  NO BOLLE
033600020925     c                   eval      $Found = *off
033700020925    4c                   endif
033800020906     C*
033900020906     c                   if        $Found = *off
034000020906     C     WGIR          IFEQ      'A'
034100020906     C                   EXSR      NOBOL2
034200020906     C                   ELSE
034300020906     C                   EXSR      NOBOLL
034400020906     C                   ENDIF
034500020906     c                   endif
034600950403     C*
034700960506    3C                   ENDIF
034800960506    2C                   ENDIF
034900960730     C*
035000960730   1AC                   ENDIF
035100941214     C*
035200960506    2C     WGIR          IFEQ      'A'                                          *ANOMALIE
035300990510     C                   READ      FNANM002                             9930
035400990510     C   99              READ(N)   FNANM002                               30
035500960730   X2C                   ELSE                                                   *SPUNTE
035600960730     C*
035700070115     C                   READ      FNBRV007                             9930
035800070115     C   99              READ(N)   FNBRV007                               30
035900960506    2C                   ENDIF
036000960506    1C                   ENDDO
036100941214     C*
036200960506     C* IMPOSTO CAMPI PER PULIZIA SPUNTE
036300960506     C                   MOVEL     'S'           WGIR
036400960506     C                   SETOFF                                       21
036500960506    0C                   ENDDO
036600970611     C*
036700070821     C* Verifico se x l'ultimo collo devo cancellare gli eventi
036800970611     C     WBRVSE        IFGT      *ZEROS
036900970611     C                   EXSR      DELEVS
037000970611     C                   CLEAR                   WBRVSE
037100970611     C                   END
037200980210     C****
037300980210     C* PULIZIA SPUNTE ERRATE SEGNACOLLI
037400980210     C****
037500070821     c                   eval      wfileerr='S'
037600070115     C                   OPEN      FNBRVE1L
037700070115     C     *LOVAL        SETLL     FNBRVE1L
037800070115     C                   READ      FNBRVE1L                               30
037900980210    1C     *IN30         DOWEQ     *OFF
038000980210     C* Prendo la data foglio a cambio di numero
038100070821     c                   if        wfvv<>FVSAV
038200020808     C                   CLEAR                   FNLV53DS
038300070821     C                   Z-ADD     BRvNPG        D53NPG
038400070821     C                   Z-ADD     BRvNFV        D53NFV
038500070821     C                   Z-ADD     BRvFGS        D53FGS
038600980210     C                   CALL      'FNLV53R'
038700020808     C                   PARM                    FNLV53DS
038800070821     C                   MOVE      wfvv          FVSAV
038900090416     c* se errore, imposto data caricammento spunte in data foglio
039000090416     c                   if        d53err<>' '
039100090416     c                   movel     PUDCS         D53DFV
039200090416     c                   clear                   FVSAV
039300090416     c                   endif
039400090416     c
039500980210    2C                   ENDIF
039600070821     c
039700070821    2C     D53DFV        IFLE      DATSPP
039800070821     c
039900070821     c* Per categorie 4 / 8 / 1 partenza, deleto
040000070821    3c                   if        brvnpg=4 or brvnpg = 8 or
040100070821     c                             (brvnpg=1 and brvftr='L')
040200070115     C                   DELETE    FNBRVE
040300070821   x3c                   else
040400070821     c
040500070823     c* Per le altre categorie mi legGo alla bolla
040600070823     c                   EXSR      CerDATbrv
040700070823     C
040800070821     c* Per le altre categorie mi lego alla bolla
040900070821     c                   eval      $Found = *off
041000070821     C* CERCO NELLE BOLLE IN ARRIVO
041100070821     C                   EXSR      RICARR
041200070821     c*
041300070821     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
041400070821    4c                   if        $Found = *off
041500070821     c                   exsr      RICPAR
041600070823     C                   ENDIF
041700070823     C
041800070823    4c                   if        $Found = *off
041900070821     C                   DELETE    FNBRVE
042000070821    4c                   endif
042100070821    3c                   endif
042200070821     c
042300070821    2C                   END
042400070115     C                   READ      FNBRVE1L                               30
042500980210    1C                   ENDDO
042600100513     c
042700100513   -1c                   endif
042800040806     C*
042900040806     C                   MOVEL     'C'           I12TLA
043000040806     C                   CALL      'FIDN12R'
043100040806     C                   PARM                    FIDN12DS
043200040806      *
043300020906     C                   MOVEL     *ON           *INLR
043400941214     C*
043500941214     C*---------------------------------------------------------------*
043600941214     C*                    RICPAR                                     *
043700941214     C* CERCO NELLE BOLLE IN PARTENZA                                 *
043800020926     c* indicatori 31 e 33 ad uso esclusivo della routine ricpar
043900941214     C*---------------------------------------------------------------*
044000941214     C     RICPAR        BEGSR
044100020926     c* Se anomlie senza segnacollo ma solo con la spedizione (70 e 71)
044200020926     c*  devo comunque entrare nell'if per chainare fnarb
044300020926     c                   if        wgir='A' and brvnsc=0
044400020926     c                   eval      savpar=*hival
044500020926     c                   endif
044600950303     C*
044700941214     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
044800020925    0C     BRVSE         IFNE      SAVpar
044900950303     C*
045000960508    1C     WGIR          IFEQ      'S'
045100960508     C     WGIR          OREQ      'A'
045200960508     C     ANMNSP        ANDEQ     0
045300941214     C     KBLT          CHAIN     FNBLT000                           31
045400960508     C*
045500960508   X1C                   ELSE
045600960514     C                   SETOFF                                       31
045700960508     C                   Z-ADD     ANMAAS        BLTAAS
045800960514     C                   MOVEL     ANMLNP        BLTLNP
045900960508     C                   MOVEL     BRVNRS        BLTNRS
046000960508     C                   MOVEL     ANMNSP        BLTNSP
046100960508    1C                   ENDIF
046200941214     C*
046300941214     C* AGGANCIO LA BOLLA
046400950303    1C  N31BLTSPE        IFNE      SAVSPE
046500950303     C     KBLP          CHAIN     FNBLP000                           31
046600990126     C* SE C'E' UNA C.A. NON CANCELLO LE SPUNTE
046700051006     c* 07 ott 05 --> addesso puliamo se c.a. in gestione alla sede
046800051006     c*               passati xxx giorni dall'apertura c.a.
046900040806      *
047000051007    1c                   If        not *in31
047100040806     c                   clear                   fidn12ds
047200040806     c                   eval      i12aas = bltaas
047300040806     c                   eval      i12lnp = bltlnp
047400040806     c                   eval      i12nrs = bltnrs
047500040806     c                   eval      i12nsp = bltnsp
047600040806     c                   eval      i12fel = 'E'
047700040806     c                   eval      i12fan = 'E'
047800040806      *
047900051007     c                   EXSR      CERCA
048000040806      *
048100950303    1C                   ENDIF
048200051007    1C                   ENDIF
048300950303     C*
048400020925     C                   MOVEL     BRVSE         SAVpar
048500950303    0C                   ENDIF
048600950120     C*
048700950120    1C     *IN31         IFEQ      *OFF
048800020906     c*
048900020906     c                   eval      $Found = *on
049000941214     C*
049100051007     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA e senza c.a.
049200051007     c
049300950303    2C     BLPDCM        IFGT      0
049400990126     C     WCABLP        ANDEQ     ' '
049500051007     c*   o con data c.a. < data gg pulizia bolle con c.a.
049600051007     c*   in questo caso pulisco anche se non è consegnata
049700051007     c     wcablp        oreq      'O'
049800950303     C*
049900960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
050000960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
050100980218   2AC     PUDCS         IFGT      BLPDCM
050200051007     c     blpdcm        andgt     0
050300960509     C*
050400090416     c                   select
050500090416     c                   when      wfileErr='S'
050600090416     c                   if        d53dfv<=datcan
050700090416     c                   delete    fnbrve
050800090416     c                   endif
050900090416     c                   when      WGIR='A'
051000960509     C                   EXSR      NOBOL2
051100090416     C                   other
051200960509     C                   EXSR      NOBOLL
051300090416     C                   ENDSL
051400960509     C*
051500960509  X2AC                   ELSE
051600090416     c*
051700090416     c* se spunta con "*" controlo direttamente la data consegna
051800090416     c*  con la data pulizia spunte con C.A.
051900090416   2bc                   if        brvnvr='*'
052000090416     c
052100090416    3c                   if        blpdcm<=datcan
052200090416     c                   exsr      BRVDEL
052300090416    3c                   endif
052400090416     c
052500090416  x2bc                   else
052600960509     C*
052700950303    3C     BLTSPE        IFNE      SAVSPE
052800941214     C*
052900941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
053000941214     C*                  - DATA CONSEGNA PARZIALE
053100950303    4C     BLPCCA        IFEQ      *BLANKS
053200941214     C     BLPDCP        ANDEQ     *ZEROS
053300950120     C                   SETOFF                                       33
053400950303   X4C                   ELSE
053500950120     C                   SETON                                        33
053600950303    4C                   ENDIF
053700941214     C*
053800981008     C* NON HA EVENTI (O SOLO QUELLI FASULLI)
053900090416    4C     *IN33         IFEQ      *OFF
054000981008     C                   Z-ADD     BLTAAS        WAAS
054100990804     C     KEVB          SETLL     FNEVB000
054200990804     C     KEVB          READE     FNEVB000                               98
054300090416    5C     *IN98         DOWEQ     *OFF
054400981008     C     EVBCEV        LOOKUP    EFS                                    97
054500981008     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
054600090416    6C     *IN97         IFEQ      *OFF
054700981008     C                   SETON                                        9833
054800981008     C                   ELSE
054900981008     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
055000990804     C     KEVB          READE     FNEVB000                               98
055100090416    6C                   ENDIF
055200090416    5C                   ENDDO
055300090416    4C                   ENDIF
055400941214     C*
055500941214     C* NON HA GIACENZA
055600050331     C  N33KBLP          SETLL     Tigcp01l                               33
055700941214     C*
055800941214     C* NON HA LEGAMI BOLLA (MAMMA)
055900950120     C  N33KBLP          SETLL     FNLBL000                               33
056000941214     C*
056100941214     C* NON HA LEGAMI BOLLA (FIGLIA)
056200950120     C  N33KBLP          SETLL     FNLBL002                               33
056300950303     C*
056400020906     C                   MOVEL     BLTSPE        SAVSPE
056500950303    3C                   ENDIF
056600090416     c
056700090416     c* Imposto la data di pulizia :
056800090416    3c                   select
056900090416     c* 1) Spunte nel file degli errori
057000090416     c                   when      wfileErr='S'
057100090416     c                   eval      datapuliz=DATcan
057200090416     C* 2) 33 OFF -    BOLLA SENZA EVENTI ANOMALI
057300090416     c                   when      *in33 =*off
057400090416     c                   eval      datapuliz=DATBRC
057500090416     c                   other
057600090416     C* 2)  altrimenti BOLLA CON   EVENTI ANOMALI
057700090416     c                   eval      datapuliz=DATBRV
057800090416    3c                   ENDSL
057900950303     C*
058000941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
058100090416    3C     BLPDCM        IFLE      DataPuliz
058200051007     c     blpdcm        andgt     0
058300051007     c* pulisco sempre se non vonsegna con c.a. perchè pulisco in base
058400051007     c*  alla data pulizia spunte con c.a.
058500051007     c     blpdcm        oreq      0
058600051007     c     wcablp        andeq     'O'
058700051007     c
058800090416    4C     WGIR          IFEQ      'A'
058900990510     C                   DELETE    FNANM002
059000090416   X4C                   ELSE
059100061019     c
059200090416     c* Se ha anomalia di spunta x DANNI, deleto solo se data <=data pulizia
059300061019     c*  spunte con anomalia
059400061023     c*
059500061023     c                   setoff                                       36
059600090416    5c                   if        brvcan<>' '
059700061023     c     brvcan        lookup    anom                                   36
059800090416    5c                   endif
059900061023     c
060000090416    5c                   if        not *in36  or wcablp='O'
060100070821     c                   EXSR      BRVDEL
060200090416   x5c                   else
060300061019     c
060400090416    6c                   if        blpdcm<=datcan
060500070821     c                   EXSR      BRVDEL
060600090416   x6c                   else
060700061019     c* altrimenti flaggo la spunta come non valida ai fini
060800061019     c*  del calcolo della responsabilità
060900090416    7c                   if        wfileerr=' '
061000070115     c                   eval      brvnvr='*'
061100070115     c                   update    fNbrv007
061200090416    7c                   endif
061300070821     c
061400090416    6c                   endif
061500090416    5c                   endif
061600090416    4C                   ENDIF
061700090416    3C                   ENDIF
061800090416   2bC                   ENDIF
061900090416   2AC                   ENDIF
062000941214     C*
062100950303   X2C                   ELSE
062200020906     C                   MOVEL     BLTSPE        SAVSPE
062300950120    2C                   ENDIF
062400941214     C*
062500950120    1C                   ENDIF
062600941214     C*
062700941214     C                   ENDSR
062800941214     C*
062900941214     C*---------------------------------------------------------------*
063000051007     C* cerco se bolla con c.a.
063100051007     C*---------------------------------------------------------------*
063200051007     c     CERCA         BEGSR
063300090417     C                   CLEAR                   WCABLP
063400051007     c                   call      'FIDN12R'
063500051007     c                   parm                    fidn12ds
063600051007      *
063700051007      * se non ci sono errori
063800051007    2c                   if        o12err = ' '
063900051007      * se numero di CA trovate maggiore di zero
064000051007    3c                   if        o12nca > 0
064100051007     c                   z-add     1             ii                3 0
064200051007    4c                   dow       skdca(II)>0 and wcablp=' '
064300051007    5c                   if        skdca(II)>datsca
064400051007     c                   movel     'S'           Wcablp
064500051007    5c                   endif
064600051007     c                   add       1             ii
064700051007    4c                   enddo
064800051007     c
064900051007     c* Se tutte le c.a. con data apertura < chain su dtc per controllare
065000070822     c*  che siano tutte LE APERTE in fase gestione SEDE
065100051007    4c                   if        wcablp=' '
065200051007     c                   z-add     1             ii
065300051007    5c                   dow       skdca(II)>0  and wcablp=' '
065400051007     c                   movel     skkey(II)     dskey
065500051007     c     kdct          chain     fndct01l
065600070822    6c                   if        %found(fndct01l) and dctgfc<>46  and
065700070822     c                             dctdch=0
065800051007     c                   movel     'S'           Wcablp
065900051007    6c                   endif
066000051007     c                   add       1             ii
066100051007    5c                   enddo
066200051007     c
066300051007     c                   if        wcablp=' '
066400051007     c                   eval      wcablp='O'
066500051007     c                   endif
066600051007     c
066700051007    4c                   endif
066800051007      *
066900051007    3c                   endif
067000051007      *
067100051007    2c                   endif
067200051007     c                   ENDSR
067300941214     C*---------------------------------------------------------------*
067400941214     C*                    RICARR                                     *
067500941214     C* CERCO NELLE BOLLE IN ARRIVO                                   *
067600020926     c* indicatori 32 e 34 ad uso esclusivo della routine ricarr
067700941214     C*---------------------------------------------------------------*
067800941214     C     RICARR        BEGSR
067900051007     c* Se anomalie  senza segnac.  ma solo con la spedizione (70 e 71)
068000020926     c*  devo comunque entrare nell'if per chainare fnarb
068100020926     c                   if        wgir='A' and brvnsc=0
068200020926     c                   eval      savse=*hival
068300020926     c                   endif
068400950303     C*
068500950303     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
068600950303    0C     BRVSE         IFNE      SAVSE
068700960508     C*
068800960508    1C     WGIR          IFEQ      'S'
068900960508     C     WGIR          OREQ      'A'
069000960508     C     ANMNSP        ANDEQ     0
069100950303     C     KBLT          CHAIN     FNART000                           32
069200960508     C*
069300960508   X1C                   ELSE
069400960514     C                   SETOFF                                       32
069500960508     C                   Z-ADD     ANMAAS        ARTAAS
069600960514     C                   MOVEL     ANMLNP        ARTLNP
069700960508     C                   MOVEL     BRVNRS        ARTNRS
069800960508     C                   MOVEL     ANMNSP        ARTNSP
069900960508    1C                   ENDIF
070000941214     C*
070100941214     C* AGGANCIO LA BOLLA
070200990126    1C  N32ARTSPE        IFNE      SAVSP2
070300950303     C     KARB          CHAIN     FNARB000                           32
070400051007     C* SE C'E' UNA C.A. cancello dalla data apertura c.a.
070500040806      *
070600090416    2c                   If        not *in32
070700040806     c                   clear                   fidn12ds
070800040806     c                   eval      i12aas = artaas
070900040806     c                   eval      i12lnp = artlnp
071000040806     c                   eval      i12nrs = artnrs
071100040806     c                   eval      i12nsp = artnsp
071200040806     c                   eval      i12fel = 'E'
071300040806     c                   eval      i12fan = 'E'
071400051007     c
071500051007     c                   EXSR      CERCA
071600040806      *
071700090416    2c                   endif
071800950303    1C                   ENDIF
071900950303     C*
072000950303     C                   MOVEL     BRVSE         SAVSE
072100950303    0C                   ENDIF
072200950120     C*
072300950303    1C     *IN32         IFEQ      *OFF
072400020906     c*
072500020906     c                   eval      $Found = *on
072600941214     C*
072700941214     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA
072800990126     C* E SE NON CI SONO C.A.
072900950120    2C     ARBDCM        IFGT      0
073000051007     C     WCAblp        ANDEQ     ' '
073100051007     c*  opure se ha c.a. con data < alla data di pulizia. cancello
073200051007     c*  anche le non consegnate
073300051007     C     WCAblp        oreq      'O'
073400960509     C*
073500960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
073600960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
073700061018     c*   se non ha c.a., altrimenti seguo la pulizia per data c.a.
073800980218   2AC     PUDCS         IFGT      ARBDCM
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
075200960509     C*
075300960509  X2AC                   ELSE
075400090416     c*
075500090416     c* se spunta con "*" controllo direttamente la data consegna
075600090416     c*  con la data pulizia spunte con C.A.
075700090416   2bc                   if        brvnvr='*'
075800090416     c
075900090416    3c                   if        arbdcm<=datcan
076000090416     c                   exsr      BRVDEL
076100090416    3c                   endif
076200090416     c
076300090416  x2bc                   else
076400090416     C*
076500950303    3C     ARTSPE        IFNE      SAVSP2
076600941214     C*
076700941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
076800941214     C*                  - DATA CONSEGNA PARZIALE
076900950303    4C     ARBCCA        IFEQ      *BLANKS
077000941214     C     ARBDCP        ANDEQ     *ZEROS
077100950303     C                   SETOFF                                       34
077200950303   X4C                   ELSE
077300950303     C                   SETON                                        34
077400950303    4C                   ENDIF
077500941214     C*
077600000614     C* NON HA EVENTI O SOLO QUELLI FASULLI
077700950303     C  N34              Z-ADD     ARTAAS        WAAS
077800000614     C     *IN34         IFEQ      *OFF
077900000614     C     KEVB2         SETLL     FNEVB000
078000000614     C     KEVB2         READE     FNEVB000                               98
078100000614     C     *IN98         DOWEQ     *OFF
078200000614     C     EVBCEV        LOOKUP    EFS                                    97
078300000614     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
078400000614     C     *IN97         IFEQ      *OFF
078500000614     C                   SETON                                        9834
078600000614     C                   ELSE
078700000614     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
078800000614     C     KEVB2         READE     FNEVB000                               98
078900000614     C                   ENDIF
079000000614     C                   ENDDO
079100000614     C                   ENDIF
079200941214     C*
079300941214     C* NON HA GIACENZA
079400050331     C  N34KARB          SETLL     Tigcp21l                               34
079500941214     C*
079600941214     C* NON HA LEGAMI BOLLA (MAMMA)
079700950303     C  N34KARB          SETLL     FNLBL000                               34
079800941214     C*
079900941214     C* NON HA LEGAMI BOLLA (FIGLIA)
080000950303     C  N34KARB          SETLL     FNLBL002                               34
080100950303     C*
080200020906     C                   MOVEL     ARTSPE        SAVSP2
080300950303    3C                   ENDIF
080400090416     c
080500090416     c* Imposto la data di pulizia :
080600090416    3c                   select
080700090416     c* 1) Spunte nel file degli errori
080800090416     c                   when      wfileErr='S'
080900090416     c                   eval      datapuliz=DATcan
081000090416     C* 2) 34 OFF -    BOLLA SENZA EVENTI ANOMALI
081100090416     c                   when      *in34 =*off
081200090416     c                   eval      datapuliz=DATBRC
081300090416     c                   other
081400090416     C* 2)  altrimenti BOLLA CON   EVENTI ANOMALI
081500090416     c                   eval      datapuliz=DATBRV
081600090416    3c                   ENDSL
081700950120     C*
081800941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
081900090416    3C     ARBDCM        IFLE      Datapuliz
082000051007     c     arbdcm        andgt     0
082100051007     c
082200051007     c     arbdcm        oreq      0
082300051007     c     wcablp        andeq     'O'
082400051007     c
082500090416    4C     WGIR          IFEQ      'A'
082600990510     C                   DELETE    FNANM002
082700090416   x4C                   ELSE
082800061023     c* Se ha anomalia di spunta x danni, deleto solo se data <=data pulizia
082900061019     c*  spunte con anomalia
083000061023     c*
083100061023     c                   setoff                                       36
083200090416    5c                   if        brvcan<>' '
083300061023     c     brvcan        lookup    anom                                   36
083400090416    5c                   endif
083500061023     c
083600090416    5c                   if        not *in36   or Wcablp='O'
083700070821     c                   EXSR      BRVDEL
083800090416   x5c                   else
083900061019     c
084000090416    6c                   if        arbdcm<=datcan
084100070821     c                   EXSR      BRVDEL
084200090416   x6c                   else
084300061019     c* altrimenti flaggo la spunta come non valida ai fini
084400061019     c*  del calcolo della responsabilità
084500090416    7c                   if        wfileerr=' '
084600070115     c                   eval      brvnvr='*'
084700070115     c                   update    fnbrv007
084800090416    7c                   endif
084900070821     c
085000090416    6c                   endif
085100090416    5c                   endif
085200980114     C**
085300090416    4c                   ENDIF
085400090416    3C                   ENDIF
085500090416   2bC                   ENDIF
085600090416   2AC                   ENDIF
085700950120     C*
085800090416   x2C                   ELSE
085900020906     C                   MOVEL     ARTSPE        SAVSP2
086000950120    2C                   ENDIF
086100941214     C*
086200950120    1C                   ENDIF
086300941214     C*
086400941214     C                   ENDSR
086500941214     C*---------------------------------------------------------------*
086600941214     C*                    NOBOLL                                     *
086700941214     C* CANCELLAZIONE SPUNTE SENZA BOLLA                              *
086800941214     C*---------------------------------------------------------------*
086900941214     C     NOBOLL        BEGSR
087000051007     c
087100051007    1C     wfvv          IFNE      savfvv
087200051007     C                   CLEAR                   FNLV53DS
087300051007     C                   Z-ADD     BRvNPG        D53NPG
087400051007     C                   Z-ADD     BRvNFV        D53NFV
087500051007     C                   Z-ADD     BRvFGS        D53FGS
087600051007     C                   CALL      'FNLV53R'
087700051007     C                   PARM                    FNLV53DS
087800051007     C                   MOVE      wfvv          savfvv
087900051007     c                   movel     d53err        werr              1
088000051007     c                   movel     d53dfv        wDFV              8 0
088100051007    1C                   ENDIF
088200061023     c
088300061023     c                   setoff                                       36
088400061023     c                   if        brvcan<>' '
088500061023     c     brvcan        lookup    anom                                   36
088600061023     c                   endif
088700051007     c
088800051007    1c                   if        Werr=' '
088900061019     c*
089000051007    2C     Wdfv          IFLE      DATSPP
089100061019     c
089200061019     C                   MOVE      BRVSE         WBRVSE
089300061023    3c                   if        not *in36
089400070821     c                   EXSR      BRVDEL
089500061019   x3c                   else
089600061019    4c                   if        wdfv<=datcan
089700070821     c                   EXSR      BRVDEL
089800061019   x4c                   else
089900070821    5c                   if        wfileerr=' '
090000070115     c                   eval      brvnvr='*'
090100070115     c                   update    fnbrv007
090200070821    5c                   endif
090300070821     c
090400070821    4c                   endif
090500061019    3c                   endif
090600061019     c
090700051007    2C                   ENDIF
090800051007     C*
090900051007     c                   else
091000051007     c* se non trovato foglio pulisco in base alla data spunta
091100061019     c
091200061019    2C     PUDCS         IFLE      DATSPP
091300061019     C                   MOVE      BRVSE         WBRVSE
091400061019     c
091500061023    3c                   if        not *in36
091600070821     C****               DELETE    FNBRV007
091700070821     c                   EXSR      BRVDEL
091800061019   x3c                   else
091900061019    4c                   if        pudcs<=datcan
092000070821     C****               DELETE    FNBRV007
092100070821     c                   EXSR      BRVDEL
092200061019   x4c                   else
092300070821    5c                   if        wfileerr=' '
092400070115     c                   eval      brvnvr='*'
092500070115     c                   update    fnbrv007
092600070821    5c                   endif
092700070821     c
092800070821    4c                   endif
092900061019    3c                   endif
093000061019     c
093100051007    2C                   ENDIF
093200051007    1C                   ENDIF
093300941214     C*
093400941214     C                   ENDSR
093500970611     C*---------------------------------------------------------------*
093600970611     C* CANCELLAZIONE EVENTI SEGNACOLLI                               *
093700970611     C*---------------------------------------------------------------*
093800970611     C     DELEVS        BEGSR
093900970611     C* VERIFICO SE PER IL SEGNACOLLO ESISTONO ANCORA DELLE SPUNTE E
094000970611     C* SE E' PRESENTE ALMENO UNA SPUNTA NON CANCELLO GLI EVENTI
094100070115     C     KBRV          SETLL     FNBRV07L                               35
094200970611     C     *IN35         IFEQ      *OFF
094300970611     C     *IN30         DOUEQ     *ON
094400970611     C     KEVS          DELETE    FNEVS000                           30
094500970611     C                   ENDDO
094600970611     C                   END
094700970611     C* MI RIPOSIZIONO SULLA SPUNTA CHE STAVO LEGGENDO
094800070115     C     KBRV7         SETLL     FNBRV07L
094900070115     C     KBRV7         READE     FNBRV07L                               30
095000970611     C                   ENDSR
095100960506     C*
095200960506     C*---------------------------------------------------------------*
095300960506     C*                    NOBOL2                                     *
095400960506     C* CANCELLAZIONE:  - ANOMALIE SENZA BOLLA                        *
095500960506     C*                 - ANOMALIE SENZA SEGNACOLLO E SPEDIZIONE      *
095600960506     C*                 - ANOMALIE CON TIPO ANOMALIA "T"              *
095700960506     C*---------------------------------------------------------------*
095800960506     C     NOBOL2        BEGSR
095900960506     C* PRENDO LA MAGGIORE TRA DATA APERTURA E DATA CHIUSURA
096000960506     C     ANMDCH        IFGT      ANMDAO
096100020906     C                   Z-ADD     ANMDCH        W0080
096200960506     C                   ELSE
096300990510     C                   Z-ADD     ANMDAO        W0080
096400960506     C                   ENDIF
096500960506     C*
096600960508     C*
096700960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
096800960903     C*                           CANCELLARE L'ANOMALIA
096900960903     C     ANMDAO        IFEQ      0
097000990510     C                   Z-ADD     19000101      W0080
097100960903     C                   ENDIF
097200960903     C*
097300990510    1C     W0080         IFLE      DATSPP
097400990510     C                   DELETE    FNANM002
097500960506    1C                   ENDIF
097600960506     C*
097700960506     C                   ENDSR
097800960506     C*
097900070821     C*---------------------------------------------------------------*
098000070821     c* Cancella record di FNBRV o FNBRVE
098100070821     C*---------------------------------------------------------------*
098200070821     c     BRVDEL        BEGSR
098300070821     c                   if        wfileerr=' '
098400070821     C                   DELETE    FNBRV007
098500070821     c                   else
098600070821     c                   delete    FNBRVE
098700070821     c                   endif
098800070821     C                   ENDSR
098900930519     C*---------------------------------------------------------------*
099000930521     C*                    CARTAB                                     *
099100930521     C* ROUTINE DI CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE  *
099200930519     C*---------------------------------------------------------------*
099300930521     C     CARTAB        BEGSR
099400981008     C***
099500981008     C* EVENTI CHE SONO FASULLI DA NON CONSIDERARE PER LA PULIZIA
099600981008     C***
099700981008     C                   MOVEL     '2Z'          COD
099800020906     C                   Z-ADD     0             XI
099900981008     C     KTAB          SETLL     TABEL
100000981008     C     KTAB          READE     TABEL                                  31
100100981008     C*
100200981008DO  1C     *IN31         DOWEQ     '0'
100300981008IF  2C     TBLFLG        IFEQ      ' '
100400981008     C                   MOVEL     TBLUNI        DS2Z
100500981008     C*
100600981008IF  3C     §2ZEFS        IFEQ      'S'
100700020906     C                   ADD       1             XI
100800020906     C                   MOVEL     TBLKEY        EFS(XI)
100900981008E   3C                   ENDIF
101000981008E   2C                   ENDIF
101100981008     C*
101200981008     C     KTAB          READE     TABEL                                  31
101300981008E   1C                   ENDDO
101400930111     C*
101500960506     C***
101600960506     C* ANOMALIE  IDD
101700960506     C***
101800960506     C                   MOVEL     '7C'          COD
101900020906     C                   Z-ADD     0             XI
102000020906     C                   Z-ADD     0             XX
102100960506     C     KTAB          SETLL     TABEL
102200960506     C     KTAB          READE     TABEL                                  31
102300960506     C*
102400960506DO  1C     *IN31         DOWEQ     '0'
102500960506IF  2C     TBLFLG        IFEQ      ' '
102600960506     C                   MOVEL     TBLUNI        DS7C
102700960506     C*
102800960506IF  3C     §7CSAN        IFEQ      ' '
102900020906     C                   ADD       1             XI
103000020906     C                   MOVEL     TBLKEY        SAN(XI)
103100960506E   3C                   ENDIF
103200961106     C*
103300961106     C* CARICO LE ANOMALIE ESTERNE AUTOCHIUSE
103400961106IF  3C     §7CAIE        IFEQ      'E'
103500961106     C     §7CCHA        ANDEQ     'S'
103600020906     C                   ADD       1             XX
103700020906     C                   MOVEL     TBLKEY        CHA(XX)
103800961106E   3C                   ENDIF
103900960506E   2C                   ENDIF
104000960506     C*
104100960506     C     KTAB          READE     TABEL                                  31
104200960506E   1C                   ENDDO
104300061023     C***
104400061023     C* carico le anomalie di spunte per DANNI
104500061023     C***
104600061023     C                   MOVEL     '3E'          COD
104700061023     C                   Z-ADD     0             XI
104800061023     C     KTAB          SETLL     TABEL
104900061023     C     KTAB          READE     TABEL                                  31
105000061023     C*
105100061023DO  1C     *IN31         DOWEQ     '0'
105200061023IF  2C     TBLFLG        IFEQ      ' '
105300061023     C                   MOVEL     TBLUNI        DS3E
105400061023     C*
105500061023     c* Escludo le anomlie di rientro che servono per la
105600061023     c*  chiusura distinta
105700061023IF  3C     §3eFTA        IFne      'R'
105800061023     C                   ADD       1             XI
105900061023     C                   MOVEL     TBLKEY        ANOM(XI)
106000061023E   3C                   ENDIF
106100061023E   2C                   ENDIF
106200061023     C*
106300061023     C     KTAB          READE     TABEL                                  31
106400061023E   1C                   ENDDO
106500960506     C*
106600930524     C                   SETOFF                                       31
106700950403     C***
106800950403     C* DETERMINO LA DATA DI PULIZIA PIU' RECENTE
106900950403     C***
107000950403     C                   SELECT
107100950403     C     DATSPP        WHENGE    DATBRV
107200950403     C     DATSPP        ANDGE     DATBRC
107300950403     C                   Z-ADD     DATSPP        WDAT
107400950403     C*
107500950403     C     DATBRV        WHENGE    DATSPP
107600950403     C     DATBRV        ANDGE     DATBRC
107700950403     C                   Z-ADD     DATBRV        WDAT
107800950403     C*
107900950403     C                   OTHER
108000950403     C                   Z-ADD     DATBRC        WDAT
108100950403     C                   ENDSL
108200950403     C*
108300930111     C                   ENDSR
108400070821     C*---------------------------------------------------------------*
108500070821     C*                    CerDATbrv                                  *
108600070821     C* Cerco la data più recente della spunta letta                  *
108700070821     C*---------------------------------------------------------------*
108800070821     c     CerDATbrv     BEGSR
108900070821    3c                   Select
109000070821     c                   When      BrvDfs > *Date and BrvDcs > *Date
109100070821     c                   If        BrvDfs > BrvDcs
109200070821     c                   Eval      Pudcs = BrvDcs
109300070821     c                   Else
109400070821     c                   Eval      Pudcs = BrvDfs
109500070821     c                   EndIf
109600070821      * Prendo la data caricamento se data immissione maggiore di udate
109700070821     c                   When      BrvDfs > *Date
109800070821     c                   Eval      Pudcs = BrvDcs
109900070821      * Prendo la data immissione se data caricamento maggiore di udate
110000070821     c                   When      BrvDcs > *Date
110100070821     c                   Eval      Pudcs = BrvDfs
110200070821
110300070821     c                   Other
110400070821     C* PRENDO LA DATA PIÙ ALTA FRA QUELLA DI CARICAMENTO E QUELLA
110500070821     C* DI IMMISSIONE/VARIAZIONE
110600070821     C     BRVDFS        IFGE      BRVDCS
110700070821     C                   Z-ADD     BRVDFS        PUDCS
110800070821     C                   ELSE
110900070821     C                   Z-ADD     BRVDCS        PUDCS
111000070821     C                   ENDIF
111100070821    3c                   EndSl
111200070821     c                   ENDSR
111300930519     C*---------------------------------------------------------------*
111400930519     C*                    DEFVAR                                     *
111500930519     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
111600930519     C*---------------------------------------------------------------*
111700930519     C     DEFVAR        BEGSR
111800930519     C*
111900930519     C     *ENTRY        PLIST
112000930519     C                   PARM                    KPJBA
112100100504      *
112200100504     c     *dtaara       define    §azute        AZUTEds
112300100504     c     *dtaara       define    §datiute      dDATIUTE
112400100504     c                   in(E)     *dtaara
112500100504     c                   if        %ERROR or RSUT = *blanks
112600100504     c                   clear                   Tibs34Ds
112700100504     c                   call      'TIBS34R'
112800100504     c                   parm                    Tibs34Ds
112900100504     c                   in        *dtaara
113000100504     c                   endif
113100100504     C*
113200100504     c
113300100504     C                   CALL      'FNLV61R'
113400100504     C                   PARM                    O61DRF            8 0
113500100504     C                   z-add     o61drf        G02inv
113600100504     C                   MOVEL     '3'           G02ERR
113700100504     C                   CALL      'XSRDA8'
113800100504     C                   PARM                    WLBDAT
113900100504     c
114000100504     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A
114100100504     C                   MOVEL     '5A'          COD
114200100504     C                   MOVEL     '1       '    KEY
114300100504     C     KTAB2         CHAIN     TABEL                              30
114400100504     C  N30              MOVEL     TBLUNI        DS5A
114500100504     C   30              MOVEL     *ZEROS        DS5A
114600100504     C*
114700100504     C* CALCOLO DATE PULIZIA SPUNTE CON BOLLE CONSEGNATE SENZA EVENTI
114800100504     C*   ANOMALI E CON EVENTI ANOMALI
114900100504     C                   EXSR      CALDAT
115000100504     C*
115100100504     C* PULIZIA SPUNTE :  SOTTRAGGO §5ASPP AI GIORNI
115200100504     C*   PULIZIA SPUNTE DI DISGUIDI O BOLLE TRANSITO
115300100504     C     G02TGI        SUB       §5ASPP        GIOTGI
115400100504     C*
115500100504     C                   CALL      'XSRGI8'
115600100504     C                   PARM                    WGIDAT
115700100504     C                   Z-ADD     GIOINV        datSPP
115800100504      *
115900100504      * pulizia spunte con c.a.: sottraggo §5ASCA ai giorni
116000100504     c     G02tgi        sub       §5ASca        GIOtgi
116100100504     c                   call      'XSRGI8'
116200100504     c                   parm                    WGIdat
116300100504     c                   z-add     GIOinv        datSCA
116400100504      *
116500100504      * pulizia spunte con BRVCAN impostato: sottatto §5ABLP ai giorni
116600100504     c*  tengo infatti le spunte come le bolle partenza
116700100504     c     G02tgi        sub       §5Ablp        GIOtgi
116800100504     c                   call      'XSRGI8'
116900100504     c                   parm                    WGIdat
117000100504     c                   z-add     GIOinv        datcan
117100100504     c*
117200930519     C*
117300941214     C* LETTURA TABEL00F
117400941214     C     KTAB          KLIST
117500051007     C                   KFLD                    CODUT             1 0
117600941214     C                   KFLD                    COD
117700100504     C     KTAB2         KLIST
117800100504     C                   KFLD                    CODUT
117900100504     C                   KFLD                    COD
118000100504     C                   KFLD                    KEY
118100100504
118200100504     c     Kazcln        Klist
118300100504     c                   Kfld                    Ktfp
118400100504     c                   Kfld                    Ktfa
118500100504     c                   Kfld                    Kann
118600100504     c                   Kfld                    Kmes
118700100504     C*
118800941214     C* LETTURA FNBLT27L
118900941214     C     KBLT          KLIST
119000941214     C                   KFLD                    BRVLNP
119100941214     C                   KFLD                    BRVLNA
119200941214     C                   KFLD                    BRVNRS
119300941214     C                   KFLD                    BRVNSC
119400941214     C* LETTURA FNBLP01L
119500941214     C     KBLP          KLIST
119600941214     C                   KFLD                    BLTAAS
119700941214     C                   KFLD                    BLTLNP
119800941214     C                   KFLD                    BLTNRS
119900941214     C                   KFLD                    BLTNSP
120000941214     C* LETTURA FNEVB01L
120100941214     C     KEVB          KLIST
120200941214     C                   KFLD                    WAAS
120300941214     C                   KFLD                    BLTLNP
120400941214     C                   KFLD                    BLTNRS
120500941214     C                   KFLD                    BLTNSP
120600941214     C     KEVB2         KLIST
120700941214     C                   KFLD                    WAAS
120800941214     C                   KFLD                    ARTLNP
120900941214     C                   KFLD                    ARTNRS
121000941214     C                   KFLD                    ARTNSP
121100941214     C* LETTURA FNARB01L
121200941214     C     KARB          KLIST
121300941214     C                   KFLD                    ARTAAS
121400941214     C                   KFLD                    ARTLNP
121500941214     C                   KFLD                    ARTNRS
121600941214     C                   KFLD                    ARTNSP
121700970611     C* LETTURA FNEVS02L
121800970611     C     KEVS          KLIST
121900040324     C                   KFLD                    WRVLNP
122000040324     C                   KFLD                    WRVLNA
122100040324     C                   KFLD                    WRVNRS
122200040324     C                   KFLD                    WRVNSC
122300070115     C* POSIZIONAMENTO FNBRV07L
122400970611     C     KBRV          KLIST
122500970611     C                   KFLD                    WRVLNP
122600970611     C                   KFLD                    WRVLNA
122700970611     C                   KFLD                    WRVNRS
122800970611     C                   KFLD                    WRVNSC
122900970611     C     KBRV7         KLIST
123000970611     C                   KFLD                    BRVLNP
123100970611     C                   KFLD                    BRVLNA
123200970611     C                   KFLD                    BRVNRS
123300970611     C                   KFLD                    BRVNSC
123400051007     C     Kdct          KLIST
123500051007     C                   KFLD                    dctaac
123600051007     C                   KFLD                    dctfil
123700051007     C                   KFLD                    dctnca
123800930521     C*
123900930519     C                   ENDSR
124000100504     C*--- CALCOLO DATE DI PULIZIA SPUNTE ----------------------------*
124100100504     C     CALDAT        BEGSR
124200100504     C*
124300100504     C* GIORNI PULIZIA
124400100504     C                   MOVEL     §5ABRV        COMBRV
124500100504     C                   MOVEL     §5ABRC        COMBRC
124600100504     C****
124700100504     C** GIORNI DATA CONSEGNA PER BOLLE CON   EVENTI ANOMALI
124800100504     C****
124900100504     C*
125000100504     C* AGGIUNGO 1 AI GIORNI DI PULIZIA CONTENUTI IN TABELLA
125100100504     C                   ADD       1             COMBRV
125200100504
125300100504      * Calcolo la data eliminando i gg. che dal calendario risultano festivi
125400100504     c     *iso          Movel     o61drf        wdata
125500100504     c                   Clear                   Ktfp
125600100504     c                   Movel     Simfel        Ktfa
125700100504     c                   Movel(p)  '1Y'          Cod
125800100504
125900100504Do  1c                   Do        combrv
126000100504     c     Festav        Tag
126100100504     c                   Subdur    1:*D          Wdata
126200100504     c                   Extrct    Wdata:*y      Kann
126300100504     c                   Extrct    Wdata:*m      Kmes
126400100504     c                   Extrct    Wdata:*d      Wgiorno
126500100504     c     Kazcln        Chain     Azcln01l
126600100504If  2c                   If        %Found(Azcln01l)
126700100504If  3c                   If        Mat(Wgiorno) <> *blanks
126800100504     c                   Movel     Mat(Wgiorno)  Key
126900100504     c     Ktab2         Chain     Tabel00f
127000100504If  4c                   If        %Found(Tabel00f)
127100100504     c                   Movel     Tbluni        Ds1y
127200100504E   4c                   EndIf
127300100504If  4c                   If        §1yfei = 'S'
127400100504     c                   Goto      Festav
127500100504E   4c                   EndIf
127600100504E   3c                   EndIf
127700100504If  3c                   If        Pom(Wgiorno) <> *blanks
127800100504     c                   Movel     Pom(Wgiorno)  Key
127900100504     c     Ktab2         Chain     Tabel00f
128000100504If  4c                   If        %Found(Tabel00f)
128100100504     c                   Movel     Tbluni        Ds1y
128200100504E   4c                   EndIf
128300100504If  4c                   If        §1yfei = 'S'
128400100504     c                   Goto      Festav
128500100504E   4c                   EndIf
128600100504E   3c                   EndIf
128700100504E   2c                   EndIf
128800100504E   1c                   EndDo
128900100504
129000100504     c                   Move      Wdata         datbrv
129100100504     C*
129200100504     C****
129300100504     C** GIORNI DATA CONSEGNA PER BOLLE SENZA EVENTI ANOMALI
129400100504     C****
129500100504     C*
129600100504     C* AGGIUNGO 1 AI GIORNI DI PULIZIA CONTENUTI IN TABELLA
129700100504     C                   ADD       1             COMBRC
129800100504      * Calcolo la data eliminando i gg. che dal calendario risultano festivi
129900100504     c     *iso          Movel     o61drf        wdata
130000100504     c                   Clear                   Ktfp
130100100504     c                   Movel     Simfel        Ktfa
130200100504     c                   Movel(p)  '1Y'          Cod
130300100504
130400100504Do  1c                   Do        combrc
130500100504     c     Festac        Tag
130600100504     c                   Subdur    1:*D          Wdata
130700100504     c                   Extrct    Wdata:*y      Kann
130800100504     c                   Extrct    Wdata:*m      Kmes
130900100504     c                   Extrct    Wdata:*d      Wgiorno
131000100504     c     Kazcln        Chain     Azcln01l
131100100504If  2c                   If        %Found(Azcln01l)
131200100504If  3c                   If        Mat(Wgiorno) <> *blanks
131300100504     c                   Movel     Mat(Wgiorno)  Key
131400100504     c     Ktab2         Chain     Tabel00f
131500100504If  4c                   If        %Found(Tabel00f)
131600100504     c                   Movel     Tbluni        Ds1y
131700100504E   4c                   EndIf
131800100504If  4c                   If        §1yfei = 'S'
131900100504     c                   Goto      Festac
132000100504E   4c                   EndIf
132100100504E   3c                   EndIf
132200100504If  3c                   If        Pom(Wgiorno) <> *blanks
132300100504     c                   Movel     Pom(Wgiorno)  Key
132400100504     c     Ktab2         Chain     Tabel00f
132500100504If  4c                   If        %Found(Tabel00f)
132600100504     c                   Movel     Tbluni        Ds1y
132700100504E   4c                   EndIf
132800100504If  4c                   If        §1yfei = 'S'
132900100504     c                   Goto      Festac
133000100504E   4c                   EndIf
133100100504E   3c                   EndIf
133200100504E   2c                   EndIf
133300100504E   1c                   EndDo
133400100504
133500100504     c                   Move      Wdata         datbrc
133600100504     C*
133700100504     C                   ENDSR
