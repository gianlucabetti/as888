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
003900020808     D FNLV53DS      E DS
004000020808     D DS2Z          E DS
004100061023     D DS3E          E DS
004200020808     D DS7C          E DS
004300040806      * ds per il controllo della presenza di CA per la spedizione richiesta
004400040806     d FIDN12DS      e ds
004500051007     d  skKey                 26    305    dim(20)
004600051007     d  skDca                326    485  0 dim(20)
004700051007     d
004800051007     d dsKey           ds
004900051007     d  dctaac                        4  0
005000051007     d  dctfil                        3  0
005100051007     d  dctnca                        7  0
005200020906
005300020906     D*
005400020906     D** DEFINIZIONE STRUTTURE DATI INTERNE
005500960506     D*
005600960506     D** PARAMETRI PASSATI DAL FILTRO
005700900618     D PARAM           DS
005800950120     D* DATA PULIZIA PER SPUNTE SENZA BOLLA
005900950120     D  DATSPP                 5     12  0
006000950120     D* DATA PULIZIA PER SPUNTE CON BOLLE CON   EVENTI ANOMALI
006100950120     D  DATBRV                13     20  0
006200950120     D* DATA PULIZIA PER SPUNTE CON BOLLE SENZA EVENTI ANOMALI
006300950120     D  DATBRC                21     28  0
006400051006     D* DATA PULIZIA PER SPUNTE CON BOLLE  con c.a. dalla data apertura c.a.
006500051007     D  DATSCA                45     52  0
006600061019     D* DATA PULIZIA PER SPUNTE CON BOLLE  con BRVCAN<>' '              c.a.
006700061019     d*  pulisco stessi giorni delle bolle partenza (30)
006800061019     D  DATcan                53     60  0
006900020808     D*
007000950303     D                 DS
007100950303     D  BRVLNP                 1      3  0
007200950303     D  BRVLNA                 4      6  0
007300950303     D  BRVNRS                 7      8  0
007400950303     D  BRVNSC                 9     15  0
007500950303     D  BRVSE                  1     15  0
007600970611     D                 DS
007700970611     D  WRVLNP                 1      3  0
007800970611     D  WRVLNA                 4      6  0
007900970611     D  WRVNRS                 7      8  0
008000970611     D  WRVNSC                 9     15  0
008100970611     D  WBRVSE                 1     15  0
008200950303     D                 DS
008300950303     D  BLTAAS                 1      4  0
008400950303     D  BLTLNP                 5      7  0
008500950303     D  BLTNRS                 8      9  0
008600950303     D  BLTNSP                10     16  0
008700950303     D  BLTSPE                 1     16  0
008800950303     D                 DS
008900950303     D  ARTAAS                 1      4  0
009000950303     D  ARTLNP                 5      7  0
009100950303     D  ARTNRS                 8      9  0
009200950303     D  ARTNSP                10     16  0
009300950303     D  ARTSPE                 1     16  0
009400950403     D                 DS
009500950404     D  BRVNPG                 1      1  0
009600020916     d  BrvFgs                 2      4  0
009700000125     D  BRVNFV                 5     10  0
009800000125     D  WFVV                   1     10  0
009900960506     D*
010000020906
010100020906     D*
010200020906     D** DEFINIZIONE CAMPI
010300020906     D*
010400020906     D COD             S                   LIKE(TBLCOD) inz
010500020906     D PUDCS           S                   LIKE(BRVDCS) inz
010600020906     D PULNP           S                   LIKE(BRVLNP) inz
010700020906     D WAAS            S                   LIKE(EVBAAS) inz
010800020906     D WDAT            S                   LIKE(DATSPP) inz
010900020906     D SAVFVV          S                   LIKE(WFVV)   inz
011000020906     D SAVBRC          S                   LIKE(DATBRC) inz
011100020906     D WMAX            S                   LIKE(TBLKEY) inz
011200020906     D SESAV           S                   LIKE(BRVSE)  inz
011300070821     D FVSAV           S                   LIKE(WFVV)   inz
011400020906     D XI              S              2  0
011500020906     D XX              S              3  0
011600070821     D WFILEERR        S              1    inz
011700070821     D WOK             S              1    inz
011800020906     D WGIR            S              1    inz
011900020906     D W0080           S              8  0 inz
012000020906     D WCABLP          S              1    inz
012100020906     D SAVse           S             15  0 inz
012200020925     D SAVpar          S             15  0 inz
012300020906     D SAVspe          S             16  0 inz
012400020906     D SAVsp2          S             16  0 inz
012500020906      * Flag "Trovata bolla"
012600020906     d $Found          s              1    inz(*off)
012700090416     d DataPuliz       s              8  0 inz
012800020906
012900960506     I*
013000990510     I** RIDENOMINAZIONE CAMPI FNANM
013100990510     IFNANM002
013200960430     I              ANMLNA                      BRVLNA
013300960430     I              ANMNRS                      BRVNRS
013400960430     I              ANMSCN                      BRVNSC
013500020906
013600941214     C*---------------------------------------------------------------*
013700941214     C* INDICATORI USATI                                              *
013800941214     C*---------------------------------------------------------------*
013900960506     C* 21    - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE OPPURE CON
014000960506     C*         TIPO ANOMALIA "T"
014100961106     C* 22    - ANOMALIA ESTERNA AUTOCHIUSA
014200020926     C* 31 33 - uso esclusivo della RICPAR
014300020926     C* 32 34 - uso esclusivo della RICARR
014400061023     C* 36    - controllo se brvcan ha anomalia x danni
014500020926     C* 30/39 - CHAIN SUI FILES
014600930519     C*---------------------------------------------------------------*
014700930519     C*
014800930519     C* DEFINIZIONE VARIABILI, KLIST E PLIST
014900930520     C                   EXSR      DEFVAR
015000930520     C*
015100930521     C* CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE
015200930521     C                   EXSR      CARTAB
015300950303     C*
015400970611     C                   CLEAR                   WBRVSE
015500970611     C*
015600960506     C* ESEGUO 2 VOLTE IL CICLO DEL PGM: -PER LA PULIZIA DELLE ANOMALIE
015700960506     C*                                  -PER LA PULIZIA DELLE SPUNTE
015800020906     C                   MOVEL     'A'           WGIR
015900960506     C*
016000960515    0C                   DO        2
016100950303     C                   CLEAR                   SAVSE
016200020925     C                   CLEAR                   SAVpar
016300970611     C                   CLEAR                   SESAV
016400950303     C                   CLEAR                   SAVSPE
016500950303     C                   CLEAR                   SAVSP2
016600950403     C                   CLEAR                   SAVFVV
016700070821     C                   CLEAR                   WfileERR
016800941214     C*
016900960430     C* LETTURA  A N O M A L I E
017000980217     C*  PER LE ANOMALIE PULIAMO TUTTO CON LA DATA PIU' ALTA
017100960430    1C     WGIR          IFEQ      'A'
017200960515     C                   MOVEL     DATBRC        SAVBRC
017300960515     C                   MOVEL     DATBRV        DATBRC
017400960515     C*
017500990510     C     *LOVAL        SETLL     FNANM002
017600990510     C                   READ      FNANM002                             9930
017700990510     C   99              READ(N)   FNANM002                               30
017800960515     C*
017900960430   X1C                   ELSE
018000960515     C                   MOVEL     SAVBRC        DATBRC
018100960430     C* LETTURA  S P U N T E
018200070115     C     *LOVAL        SETLL     FNBRV007
018300070115     C                   READ      FNBRV007                             9930
018400070115     C   99              READ(N)   FNBRV007                               30
018500960430    1C                   ENDIF
018600941214     C*
018700941214    1C     *IN30         DOWEQ     *OFF
018800960730     C*
018900960730     C* SOLO SE SONO RIUSCITO AD ALLOCARE IL RECORD
019000960730   1AC     *IN99         IFEQ      *OFF
019100960430     C*
019200990510     C* SOLO PER  A N O M A L I E :
019300960506    2C     WGIR          IFEQ      'A'
019400960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
019500960903     C*                           CANCELLARE L'ANOMALIA
019600980217    3C     ANMDAO        IFEQ      0
019700990510     C                   Z-ADD     19000101      PUDCS
019800980217   X3C                   ELSE
019900990510     C                   Z-ADD     ANMDAO        PUDCS
020000980217    3C                   ENDIF
020100980217     C**
020200980217   X2C                   ELSE
020300021105      * Prendo la data minore tra caricamento e immissione se tutte e 2 sono
020400021105      * maggiori di udate
020500070821     c                   EXSR      CerDATbrv
020600070821     c
020700980217    2C                   ENDIF
020800960430     C*
020900980217     C* SE LA DATA APERTURA ANOMALIA O
021000980217     C*    LA DATA DI CARICAMENTO SPUNTA E' > DELLA DATA PIU' RECENTE
021100950403     C*  DI PULIZIA --> NON LA CONSIDERO NEMMENO
021200980217    2C     PUDCS         IFLE      WDAT
021300020906     C                   MOVEL     'S'           WOK
021400941214     C*
021500960430     C* SOLO PER  A N O M A L I E
021600960430    3C     WGIR          IFEQ      'A'
021700960506     C*
021800960506     C* 21 ON  - ANOMALIA SENZA SEGNACOLLO E SPEDIZIONE
021900960506     C     ANMCAA        LOOKUP    SAN                                    21
022000960506     C*
022100960430     C* LE ANOMALIE "E/R" SI PULISCONO SOLO SE HANNO LA DATA CHIUSURA
022200960506    4C  N21ANMAIE        IFEQ      'E'
022300960430     C     ANMAIE        OREQ      'R'
022400961106     C                   SETOFF                                       22
022500961106     C*
022600961106     C* 22 ON  - SI TRATTA DI ANOMALIA ESTERNA AUTOCHIUSA
022700961106    5C     ANMAIE        IFEQ      'E'
022800961106     C     ANMCAA        ANDNE     70
022900961106     C     ANMCAA        ANDNE     71
023000961106     C     ANMCAA        LOOKUP    CHA                                    22
023100961106    5C                   ENDIF
023200961106     C*
023300961106    5C     ANMAIE        IFEQ      'E'
023400961106     C     *IN22         ANDEQ     *OFF
023500980217     C**
023600980217     C* VERIFICO SE LNA FA O MENO L'IDD
023700020906     c                   eval      XX = 1
023800050805     c**   BRVlna        lookup    SKpo(xx)                               37
023900050805     c** 37              movel     NIDD(xx)      LNANID
024000050805     c**N37              movel     'N'           LNANID
024100980217     C**
024200050805    6C**   LNANID        IFNE      'N'
024300980217    7C     ANMDCH        IFEQ      0
024400980217     C     ANMFT1        OREQ      ' '
024500980217     C     ANMFL1        ANDGT     0
024600980217     C     ANMFT2        OREQ      ' '
024700980217     C     ANMFL2        ANDGT     0
024800980217     C                   CLEAR                   WOK
024900980217    7C                   ENDIF
025000050805    6C**                 ENDIF
025100050805    5C                   ENDIF
025200961106     C*
025300960430   X4C                   ELSE
025400960506     C* 21 ON  - ANOMALIA CON TIPO ANOMALIA "T"
025500960506     C     ANMAIE        IFEQ      'T'
025600960506     C                   SETON                                        21
025700960430     C                   ENDIF
025800960430    4C                   ENDIF
025900960430    3C                   ENDIF
026000960430     C*
026100960430     C* WOK = "S" ---> RECORD DA ELABORARE
026200960506    3C     WOK           IFEQ      'S'
026300960430     C*
026400960506    4C     *IN21         IFEQ      *OFF
026500960530     C*
026600960506    5C     WGIR          IFEQ      'A'
026700960506     C     ANMLNP        IFGT      0
026800960506     C                   MOVEL     ANMLNP        BRVLNP
026900960506     C                   ELSE
027000960506     C                   MOVEL     ANMFLS        BRVLNP
027100960506     C                   ENDIF
027200960506    5C                   ENDIF
027300960530     C*
027400970611     C*
027500970611     C* A CAMBIO SEGNACOLLO:
027600970611     C     WGIR          IFEQ      'S'
027700970611     C     BRVSE         ANDNE     SESAV
027800970611     C* Verifico se per il collo precedente devo cancellare gli eventi
027900970611     C     WBRVSE        IFGT      *ZEROS
028000970611     C                   EXSR      DELEVS
028100970611     C                   CLEAR                   WBRVSE
028200970611     C                   END
028300970611     C                   MOVE      BRVSE         SESAV
028400970611     C                   END
028500941214     C*
028600020906     c                   eval      $Found = *off
028700941214     C* CERCO NELLE BOLLE IN ARRIVO
028800941214     C                   EXSR      RICARR
028900020906     c*
029000020906     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
029100020906     c                   if        $Found = *off
029200020906     c                   exsr      RICPAR
029300020906     c                   endif
029400020925     c                   else
029500020925     c* se anomalia senza spedizione o di tipo "T" eseguo solo il ciclo
029600020925     c*  NO BOLLE
029700020925     c                   eval      $Found = *off
029800020925    4c                   endif
029900020906     C*
030000020906     c                   if        $Found = *off
030100020906     C     WGIR          IFEQ      'A'
030200020906     C                   EXSR      NOBOL2
030300020906     C                   ELSE
030400020906     C                   EXSR      NOBOLL
030500020906     C                   ENDIF
030600020906     c                   endif
030700950403     C*
030800960506    3C                   ENDIF
030900960506    2C                   ENDIF
031000960730     C*
031100960730   1AC                   ENDIF
031200941214     C*
031300960506    2C     WGIR          IFEQ      'A'                                          *ANOMALIE
031400990510     C                   READ      FNANM002                             9930
031500990510     C   99              READ(N)   FNANM002                               30
031600960730   X2C                   ELSE                                                   *SPUNTE
031700960730     C*
031800070115     C                   READ      FNBRV007                             9930
031900070115     C   99              READ(N)   FNBRV007                               30
032000960506    2C                   ENDIF
032100960506    1C                   ENDDO
032200941214     C*
032300960506     C* IMPOSTO CAMPI PER PULIZIA SPUNTE
032400960506     C                   MOVEL     'S'           WGIR
032500960506     C                   SETOFF                                       21
032600960506    0C                   ENDDO
032700970611     C*
032800070821     C* Verifico se x l'ultimo collo devo cancellare gli eventi
032900970611     C     WBRVSE        IFGT      *ZEROS
033000970611     C                   EXSR      DELEVS
033100970611     C                   CLEAR                   WBRVSE
033200970611     C                   END
033300980210     C****
033400980210     C* PULIZIA SPUNTE ERRATE SEGNACOLLI
033500980210     C****
033600070821     c                   eval      wfileerr='S'
033700070115     C                   OPEN      FNBRVE1L
033800070115     C     *LOVAL        SETLL     FNBRVE1L
033900070115     C                   READ      FNBRVE1L                               30
034000980210    1C     *IN30         DOWEQ     *OFF
034100980210     C* Prendo la data foglio a cambio di numero
034200070821     c                   if        wfvv<>FVSAV
034300020808     C                   CLEAR                   FNLV53DS
034400070821     C                   Z-ADD     BRvNPG        D53NPG
034500070821     C                   Z-ADD     BRvNFV        D53NFV
034600070821     C                   Z-ADD     BRvFGS        D53FGS
034700980210     C                   CALL      'FNLV53R'
034800020808     C                   PARM                    FNLV53DS
034900070821     C                   MOVE      wfvv          FVSAV
035000090416     c* se errore, imposto data caricammento spunte in data foglio
035100090416     c                   if        d53err<>' '
035200090416     c                   movel     PUDCS         D53DFV
035300090416     c                   clear                   FVSAV
035400090416     c                   endif
035500090416     c
035600980210    2C                   ENDIF
035700070821     c
035800070821    2C     D53DFV        IFLE      DATSPP
035900070821     c
036000070821     c* Per categorie 4 / 8 / 1 partenza, deleto
036100070821    3c                   if        brvnpg=4 or brvnpg = 8 or
036200070821     c                             (brvnpg=1 and brvftr='L')
036300070115     C                   DELETE    FNBRVE
036400070821   x3c                   else
036500070821     c
036600070823     c* Per le altre categorie mi legGo alla bolla
036700070823     c                   EXSR      CerDATbrv
036800070823     C
036900070821     c* Per le altre categorie mi lego alla bolla
037000070821     c                   eval      $Found = *off
037100070821     C* CERCO NELLE BOLLE IN ARRIVO
037200070821     C                   EXSR      RICARR
037300070821     c*
037400070821     c* Cerco nelle bolle in partenza se non trovata bolla in arrivo
037500070821    4c                   if        $Found = *off
037600070821     c                   exsr      RICPAR
037700070823     C                   ENDIF
037800070823     C
037900070823    4c                   if        $Found = *off
038000070821     C                   DELETE    FNBRVE
038100070821    4c                   endif
038200070821    3c                   endif
038300070821     c
038400070821    2C                   END
038500070115     C                   READ      FNBRVE1L                               30
038600980210    1C                   ENDDO
038700040806     C*
038800040806     C                   MOVEL     'C'           I12TLA
038900040806     C                   CALL      'FIDN12R'
039000040806     C                   PARM                    FIDN12DS
039100040806      *
039200020906     C                   MOVEL     *ON           *INLR
039300941214     C*
039400941214     C*---------------------------------------------------------------*
039500941214     C*                    RICPAR                                     *
039600941214     C* CERCO NELLE BOLLE IN PARTENZA                                 *
039700020926     c* indicatori 31 e 33 ad uso esclusivo della routine ricpar
039800941214     C*---------------------------------------------------------------*
039900941214     C     RICPAR        BEGSR
040000020926     c* Se anomlie senza segnacollo ma solo con la spedizione (70 e 71)
040100020926     c*  devo comunque entrare nell'if per chainare fnarb
040200020926     c                   if        wgir='A' and brvnsc=0
040300020926     c                   eval      savpar=*hival
040400020926     c                   endif
040500950303     C*
040600941214     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
040700020925    0C     BRVSE         IFNE      SAVpar
040800950303     C*
040900960508    1C     WGIR          IFEQ      'S'
041000960508     C     WGIR          OREQ      'A'
041100960508     C     ANMNSP        ANDEQ     0
041200941214     C     KBLT          CHAIN     FNBLT000                           31
041300960508     C*
041400960508   X1C                   ELSE
041500960514     C                   SETOFF                                       31
041600960508     C                   Z-ADD     ANMAAS        BLTAAS
041700960514     C                   MOVEL     ANMLNP        BLTLNP
041800960508     C                   MOVEL     BRVNRS        BLTNRS
041900960508     C                   MOVEL     ANMNSP        BLTNSP
042000960508    1C                   ENDIF
042100941214     C*
042200941214     C* AGGANCIO LA BOLLA
042300950303    1C  N31BLTSPE        IFNE      SAVSPE
042400950303     C     KBLP          CHAIN     FNBLP000                           31
042500990126     C* SE C'E' UNA C.A. NON CANCELLO LE SPUNTE
042600051006     c* 07 ott 05 --> addesso puliamo se c.a. in gestione alla sede
042700051006     c*               passati xxx giorni dall'apertura c.a.
042800040806      *
042900051007    1c                   If        not *in31
043000040806     c                   clear                   fidn12ds
043100040806     c                   eval      i12aas = bltaas
043200040806     c                   eval      i12lnp = bltlnp
043300040806     c                   eval      i12nrs = bltnrs
043400040806     c                   eval      i12nsp = bltnsp
043500040806     c                   eval      i12fel = 'E'
043600040806     c                   eval      i12fan = 'E'
043700040806      *
043800051007     c                   EXSR      CERCA
043900040806      *
044000950303    1C                   ENDIF
044100051007    1C                   ENDIF
044200950303     C*
044300020925     C                   MOVEL     BRVSE         SAVpar
044400950303    0C                   ENDIF
044500950120     C*
044600950120    1C     *IN31         IFEQ      *OFF
044700020906     c*
044800020906     c                   eval      $Found = *on
044900941214     C*
045000051007     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA e senza c.a.
045100051007     c
045200950303    2C     BLPDCM        IFGT      0
045300990126     C     WCABLP        ANDEQ     ' '
045400051007     c*   o con data c.a. < data gg pulizia bolle con c.a.
045500051007     c*   in questo caso pulisco anche se non è consegnata
045600051007     c     wcablp        oreq      'O'
045700950303     C*
045800960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
045900960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
046000980218   2AC     PUDCS         IFGT      BLPDCM
046100051007     c     blpdcm        andgt     0
046200960509     C*
046300090416     c                   select
046400090416     c                   when      wfileErr='S'
046500090416     c                   if        d53dfv<=datcan
046600090416     c                   delete    fnbrve
046700090416     c                   endif
046800090416     c                   when      WGIR='A'
046900960509     C                   EXSR      NOBOL2
047000090416     C                   other
047100960509     C                   EXSR      NOBOLL
047200090416     C                   ENDSL
047300960509     C*
047400960509  X2AC                   ELSE
047500090416     c*
047600090416     c* se spunta con "*" controlo direttamente la data consegna
047700090416     c*  con la data pulizia spunte con C.A.
047800090416   2bc                   if        brvnvr='*'
047900090416     c
048000090416    3c                   if        blpdcm<=datcan
048100090416     c                   exsr      BRVDEL
048200090416    3c                   endif
048300090416     c
048400090416  x2bc                   else
048500960509     C*
048600950303    3C     BLTSPE        IFNE      SAVSPE
048700941214     C*
048800941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
048900941214     C*                  - DATA CONSEGNA PARZIALE
049000950303    4C     BLPCCA        IFEQ      *BLANKS
049100941214     C     BLPDCP        ANDEQ     *ZEROS
049200950120     C                   SETOFF                                       33
049300950303   X4C                   ELSE
049400950120     C                   SETON                                        33
049500950303    4C                   ENDIF
049600941214     C*
049700981008     C* NON HA EVENTI (O SOLO QUELLI FASULLI)
049800090416    4C     *IN33         IFEQ      *OFF
049900981008     C                   Z-ADD     BLTAAS        WAAS
050000990804     C     KEVB          SETLL     FNEVB000
050100990804     C     KEVB          READE     FNEVB000                               98
050200090416    5C     *IN98         DOWEQ     *OFF
050300981008     C     EVBCEV        LOOKUP    EFS                                    97
050400981008     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
050500090416    6C     *IN97         IFEQ      *OFF
050600981008     C                   SETON                                        9833
050700981008     C                   ELSE
050800981008     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
050900990804     C     KEVB          READE     FNEVB000                               98
051000090416    6C                   ENDIF
051100090416    5C                   ENDDO
051200090416    4C                   ENDIF
051300941214     C*
051400941214     C* NON HA GIACENZA
051500050331     C  N33KBLP          SETLL     Tigcp01l                               33
051600941214     C*
051700941214     C* NON HA LEGAMI BOLLA (MAMMA)
051800950120     C  N33KBLP          SETLL     FNLBL000                               33
051900941214     C*
052000941214     C* NON HA LEGAMI BOLLA (FIGLIA)
052100950120     C  N33KBLP          SETLL     FNLBL002                               33
052200950303     C*
052300020906     C                   MOVEL     BLTSPE        SAVSPE
052400950303    3C                   ENDIF
052500090416     c
052600090416     c* Imposto la data di pulizia :
052700090416    3c                   select
052800090416     c* 1) Spunte nel file degli errori
052900090416     c                   when      wfileErr='S'
053000090416     c                   eval      datapuliz=DATcan
053100090416     C* 2) 33 OFF -    BOLLA SENZA EVENTI ANOMALI
053200090416     c                   when      *in33 =*off
053300090416     c                   eval      datapuliz=DATBRC
053400090416     c                   other
053500090416     C* 2)  altrimenti BOLLA CON   EVENTI ANOMALI
053600090416     c                   eval      datapuliz=DATBRV
053700090416    3c                   ENDSL
053800950303     C*
053900941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
054000090416    3C     BLPDCM        IFLE      DataPuliz
054100051007     c     blpdcm        andgt     0
054200051007     c* pulisco sempre se non vonsegna con c.a. perchè pulisco in base
054300051007     c*  alla data pulizia spunte con c.a.
054400051007     c     blpdcm        oreq      0
054500051007     c     wcablp        andeq     'O'
054600051007     c
054700090416    4C     WGIR          IFEQ      'A'
054800990510     C                   DELETE    FNANM002
054900090416   X4C                   ELSE
055000061019     c
055100090416     c* Se ha anomalia di spunta x DANNI, deleto solo se data <=data pulizia
055200061019     c*  spunte con anomalia
055300061023     c*
055400061023     c                   setoff                                       36
055500090416    5c                   if        brvcan<>' '
055600061023     c     brvcan        lookup    anom                                   36
055700090416    5c                   endif
055800061023     c
055900090416    5c                   if        not *in36  or wcablp='O'
056000070821     c                   EXSR      BRVDEL
056100090416   x5c                   else
056200061019     c
056300090416    6c                   if        blpdcm<=datcan
056400070821     c                   EXSR      BRVDEL
056500090416   x6c                   else
056600061019     c* altrimenti flaggo la spunta come non valida ai fini
056700061019     c*  del calcolo della responsabilità
056800090416    7c                   if        wfileerr=' '
056900070115     c                   eval      brvnvr='*'
057000070115     c                   update    fNbrv007
057100090416    7c                   endif
057200070821     c
057300090416    6c                   endif
057400090416    5c                   endif
057500090416    4C                   ENDIF
057600090416    3C                   ENDIF
057700090416   2bC                   ENDIF
057800090416   2AC                   ENDIF
057900941214     C*
058000950303   X2C                   ELSE
058100020906     C                   MOVEL     BLTSPE        SAVSPE
058200950120    2C                   ENDIF
058300941214     C*
058400950120    1C                   ENDIF
058500941214     C*
058600941214     C                   ENDSR
058700941214     C*
058800941214     C*---------------------------------------------------------------*
058900051007     C* cerco se bolla con c.a.
059000051007     C*---------------------------------------------------------------*
059100051007     c     CERCA         BEGSR
059200090417     C                   CLEAR                   WCABLP
059300051007     c                   call      'FIDN12R'
059400051007     c                   parm                    fidn12ds
059500051007      *
059600051007      * se non ci sono errori
059700051007    2c                   if        o12err = ' '
059800051007      * se numero di CA trovate maggiore di zero
059900051007    3c                   if        o12nca > 0
060000051007     c                   z-add     1             ii                3 0
060100051007    4c                   dow       skdca(II)>0 and wcablp=' '
060200051007    5c                   if        skdca(II)>datsca
060300051007     c                   movel     'S'           Wcablp
060400051007    5c                   endif
060500051007     c                   add       1             ii
060600051007    4c                   enddo
060700051007     c
060800051007     c* Se tutte le c.a. con data apertura < chain su dtc per controllare
060900070822     c*  che siano tutte LE APERTE in fase gestione SEDE
061000051007    4c                   if        wcablp=' '
061100051007     c                   z-add     1             ii
061200051007    5c                   dow       skdca(II)>0  and wcablp=' '
061300051007     c                   movel     skkey(II)     dskey
061400051007     c     kdct          chain     fndct01l
061500070822    6c                   if        %found(fndct01l) and dctgfc<>46  and
061600070822     c                             dctdch=0
061700051007     c                   movel     'S'           Wcablp
061800051007    6c                   endif
061900051007     c                   add       1             ii
062000051007    5c                   enddo
062100051007     c
062200051007     c                   if        wcablp=' '
062300051007     c                   eval      wcablp='O'
062400051007     c                   endif
062500051007     c
062600051007    4c                   endif
062700051007      *
062800051007    3c                   endif
062900051007      *
063000051007    2c                   endif
063100051007     c                   ENDSR
063200941214     C*---------------------------------------------------------------*
063300941214     C*                    RICARR                                     *
063400941214     C* CERCO NELLE BOLLE IN ARRIVO                                   *
063500020926     c* indicatori 32 e 34 ad uso esclusivo della routine ricarr
063600941214     C*---------------------------------------------------------------*
063700941214     C     RICARR        BEGSR
063800051007     c* Se anomalie  senza segnac.  ma solo con la spedizione (70 e 71)
063900020926     c*  devo comunque entrare nell'if per chainare fnarb
064000020926     c                   if        wgir='A' and brvnsc=0
064100020926     c                   eval      savse=*hival
064200020926     c                   endif
064300950303     C*
064400950303     C* AGGANCIO IL COLLO PER PRENDERE IL NUMERO DI SPEDIZIONE
064500950303    0C     BRVSE         IFNE      SAVSE
064600960508     C*
064700960508    1C     WGIR          IFEQ      'S'
064800960508     C     WGIR          OREQ      'A'
064900960508     C     ANMNSP        ANDEQ     0
065000950303     C     KBLT          CHAIN     FNART000                           32
065100960508     C*
065200960508   X1C                   ELSE
065300960514     C                   SETOFF                                       32
065400960508     C                   Z-ADD     ANMAAS        ARTAAS
065500960514     C                   MOVEL     ANMLNP        ARTLNP
065600960508     C                   MOVEL     BRVNRS        ARTNRS
065700960508     C                   MOVEL     ANMNSP        ARTNSP
065800960508    1C                   ENDIF
065900941214     C*
066000941214     C* AGGANCIO LA BOLLA
066100990126    1C  N32ARTSPE        IFNE      SAVSP2
066200950303     C     KARB          CHAIN     FNARB000                           32
066300051007     C* SE C'E' UNA C.A. cancello dalla data apertura c.a.
066400040806      *
066500090416    2c                   If        not *in32
066600040806     c                   clear                   fidn12ds
066700040806     c                   eval      i12aas = artaas
066800040806     c                   eval      i12lnp = artlnp
066900040806     c                   eval      i12nrs = artnrs
067000040806     c                   eval      i12nsp = artnsp
067100040806     c                   eval      i12fel = 'E'
067200040806     c                   eval      i12fan = 'E'
067300051007     c
067400051007     c                   EXSR      CERCA
067500040806      *
067600090416    2c                   endif
067700950303    1C                   ENDIF
067800950303     C*
067900950303     C                   MOVEL     BRVSE         SAVSE
068000950303    0C                   ENDIF
068100950120     C*
068200950303    1C     *IN32         IFEQ      *OFF
068300020906     c*
068400020906     c                   eval      $Found = *on
068500941214     C*
068600941214     C* ELABORO LA BOLLA SOLO SE E' CONSEGNATA
068700990126     C* E SE NON CI SONO C.A.
068800950120    2C     ARBDCM        IFGT      0
068900051007     C     WCAblp        ANDEQ     ' '
069000051007     c*  opure se ha c.a. con data < alla data di pulizia. cancello
069100051007     c*  anche le non consegnate
069200051007     C     WCAblp        oreq      'O'
069300960509     C*
069400960509     C* SE LA DATA SPUNTA/ANOMALIA E' MAGGIORE DELLA DATA CONSEGNA
069500960509     C*   MERCE USO LA DATA PULIZIA DELLE SPUNTE/ANOMALIE SENZA BOLLA
069600061018     c*   se non ha c.a., altrimenti seguo la pulizia per data c.a.
069700980218   2AC     PUDCS         IFGT      ARBDCM
069800051007     c     arbdcm        andgt     0
069900061018     C     WCAblp        ANDEQ     ' '
070000960509     C*
070100090416     c                   select
070200090416     c                   when      wfileErr='S'
070300090416     c                   if        d53dfv<=datcan
070400090416     c                   delete    fnbrve
070500090416     c                   endif
070600090416     c                   when      WGIR='A'
070700090416     C                   EXSR      NOBOL2
070800090416     C                   other
070900090416     C                   EXSR      NOBOLL
071000090416     C                   ENDSL
071100960509     C*
071200960509  X2AC                   ELSE
071300090416     c*
071400090416     c* se spunta con "*" controllo direttamente la data consegna
071500090416     c*  con la data pulizia spunte con C.A.
071600090416   2bc                   if        brvnvr='*'
071700090416     c
071800090416    3c                   if        arbdcm<=datcan
071900090416     c                   exsr      BRVDEL
072000090416    3c                   endif
072100090416     c
072200090416  x2bc                   else
072300090416     C*
072400950303    3C     ARTSPE        IFNE      SAVSP2
072500941214     C*
072600941214     C* SE BOLLA NON HA: - CODICE CONSEGNA ANOMALA
072700941214     C*                  - DATA CONSEGNA PARZIALE
072800950303    4C     ARBCCA        IFEQ      *BLANKS
072900941214     C     ARBDCP        ANDEQ     *ZEROS
073000950303     C                   SETOFF                                       34
073100950303   X4C                   ELSE
073200950303     C                   SETON                                        34
073300950303    4C                   ENDIF
073400941214     C*
073500000614     C* NON HA EVENTI O SOLO QUELLI FASULLI
073600950303     C  N34              Z-ADD     ARTAAS        WAAS
073700000614     C     *IN34         IFEQ      *OFF
073800000614     C     KEVB2         SETLL     FNEVB000
073900000614     C     KEVB2         READE     FNEVB000                               98
074000000614     C     *IN98         DOWEQ     *OFF
074100000614     C     EVBCEV        LOOKUP    EFS                                    97
074200000614     C* SE NON C'E' NON DEVO PULIRE E ESCO DA LETTURA
074300000614     C     *IN97         IFEQ      *OFF
074400000614     C                   SETON                                        9834
074500000614     C                   ELSE
074600000614     C* VADO AVANTI A LEGGERE PER VEDERE SE CI SONO ALTRI EVENTI
074700000614     C     KEVB2         READE     FNEVB000                               98
074800000614     C                   ENDIF
074900000614     C                   ENDDO
075000000614     C                   ENDIF
075100941214     C*
075200941214     C* NON HA GIACENZA
075300050331     C  N34KARB          SETLL     Tigcp21l                               34
075400941214     C*
075500941214     C* NON HA LEGAMI BOLLA (MAMMA)
075600950303     C  N34KARB          SETLL     FNLBL000                               34
075700941214     C*
075800941214     C* NON HA LEGAMI BOLLA (FIGLIA)
075900950303     C  N34KARB          SETLL     FNLBL002                               34
076000950303     C*
076100020906     C                   MOVEL     ARTSPE        SAVSP2
076200950303    3C                   ENDIF
076300090416     c
076400090416     c* Imposto la data di pulizia :
076500090416    3c                   select
076600090416     c* 1) Spunte nel file degli errori
076700090416     c                   when      wfileErr='S'
076800090416     c                   eval      datapuliz=DATcan
076900090416     C* 2) 34 OFF -    BOLLA SENZA EVENTI ANOMALI
077000090416     c                   when      *in34 =*off
077100090416     c                   eval      datapuliz=DATBRC
077200090416     c                   other
077300090416     C* 2)  altrimenti BOLLA CON   EVENTI ANOMALI
077400090416     c                   eval      datapuliz=DATBRV
077500090416    3c                   ENDSL
077600950120     C*
077700941214     C* SE DATA CONSEGNA MINORE/UGUALE DATA PULIZIA, DELETO LA SPUNTA
077800090416    3C     ARBDCM        IFLE      Datapuliz
077900051007     c     arbdcm        andgt     0
078000051007     c
078100051007     c     arbdcm        oreq      0
078200051007     c     wcablp        andeq     'O'
078300051007     c
078400090416    4C     WGIR          IFEQ      'A'
078500990510     C                   DELETE    FNANM002
078600090416   x4C                   ELSE
078700061023     c* Se ha anomalia di spunta x danni, deleto solo se data <=data pulizia
078800061019     c*  spunte con anomalia
078900061023     c*
079000061023     c                   setoff                                       36
079100090416    5c                   if        brvcan<>' '
079200061023     c     brvcan        lookup    anom                                   36
079300090416    5c                   endif
079400061023     c
079500090416    5c                   if        not *in36   or Wcablp='O'
079600070821     c                   EXSR      BRVDEL
079700090416   x5c                   else
079800061019     c
079900090416    6c                   if        arbdcm<=datcan
080000070821     c                   EXSR      BRVDEL
080100090416   x6c                   else
080200061019     c* altrimenti flaggo la spunta come non valida ai fini
080300061019     c*  del calcolo della responsabilità
080400090416    7c                   if        wfileerr=' '
080500070115     c                   eval      brvnvr='*'
080600070115     c                   update    fnbrv007
080700090416    7c                   endif
080800070821     c
080900090416    6c                   endif
081000090416    5c                   endif
081100980114     C**
081200090416    4c                   ENDIF
081300090416    3C                   ENDIF
081400090416   2bC                   ENDIF
081500090416   2AC                   ENDIF
081600950120     C*
081700090416   x2C                   ELSE
081800020906     C                   MOVEL     ARTSPE        SAVSP2
081900950120    2C                   ENDIF
082000941214     C*
082100950120    1C                   ENDIF
082200941214     C*
082300941214     C                   ENDSR
082400941214     C*---------------------------------------------------------------*
082500941214     C*                    NOBOLL                                     *
082600941214     C* CANCELLAZIONE SPUNTE SENZA BOLLA                              *
082700941214     C*---------------------------------------------------------------*
082800941214     C     NOBOLL        BEGSR
082900051007     c
083000051007    1C     wfvv          IFNE      savfvv
083100051007     C                   CLEAR                   FNLV53DS
083200051007     C                   Z-ADD     BRvNPG        D53NPG
083300051007     C                   Z-ADD     BRvNFV        D53NFV
083400051007     C                   Z-ADD     BRvFGS        D53FGS
083500051007     C                   CALL      'FNLV53R'
083600051007     C                   PARM                    FNLV53DS
083700051007     C                   MOVE      wfvv          savfvv
083800051007     c                   movel     d53err        werr              1
083900051007     c                   movel     d53dfv        wDFV              8 0
084000051007    1C                   ENDIF
084100061023     c
084200061023     c                   setoff                                       36
084300061023     c                   if        brvcan<>' '
084400061023     c     brvcan        lookup    anom                                   36
084500061023     c                   endif
084600051007     c
084700051007    1c                   if        Werr=' '
084800061019     c*
084900051007    2C     Wdfv          IFLE      DATSPP
085000061019     c
085100061019     C                   MOVE      BRVSE         WBRVSE
085200061023    3c                   if        not *in36
085300070821     c                   EXSR      BRVDEL
085400061019   x3c                   else
085500061019    4c                   if        wdfv<=datcan
085600070821     c                   EXSR      BRVDEL
085700061019   x4c                   else
085800070821    5c                   if        wfileerr=' '
085900070115     c                   eval      brvnvr='*'
086000070115     c                   update    fnbrv007
086100070821    5c                   endif
086200070821     c
086300070821    4c                   endif
086400061019    3c                   endif
086500061019     c
086600051007    2C                   ENDIF
086700051007     C*
086800051007     c                   else
086900051007     c* se non trovato foglio pulisco in base alla data spunta
087000061019     c
087100061019    2C     PUDCS         IFLE      DATSPP
087200061019     C                   MOVE      BRVSE         WBRVSE
087300061019     c
087400061023    3c                   if        not *in36
087500070821     C****               DELETE    FNBRV007
087600070821     c                   EXSR      BRVDEL
087700061019   x3c                   else
087800061019    4c                   if        pudcs<=datcan
087900070821     C****               DELETE    FNBRV007
088000070821     c                   EXSR      BRVDEL
088100061019   x4c                   else
088200070821    5c                   if        wfileerr=' '
088300070115     c                   eval      brvnvr='*'
088400070115     c                   update    fnbrv007
088500070821    5c                   endif
088600070821     c
088700070821    4c                   endif
088800061019    3c                   endif
088900061019     c
089000051007    2C                   ENDIF
089100051007    1C                   ENDIF
089200941214     C*
089300941214     C                   ENDSR
089400970611     C*---------------------------------------------------------------*
089500970611     C* CANCELLAZIONE EVENTI SEGNACOLLI                               *
089600970611     C*---------------------------------------------------------------*
089700970611     C     DELEVS        BEGSR
089800970611     C* VERIFICO SE PER IL SEGNACOLLO ESISTONO ANCORA DELLE SPUNTE E
089900970611     C* SE E' PRESENTE ALMENO UNA SPUNTA NON CANCELLO GLI EVENTI
090000070115     C     KBRV          SETLL     FNBRV07L                               35
090100970611     C     *IN35         IFEQ      *OFF
090200970611     C     *IN30         DOUEQ     *ON
090300970611     C     KEVS          DELETE    FNEVS000                           30
090400970611     C                   ENDDO
090500970611     C                   END
090600970611     C* MI RIPOSIZIONO SULLA SPUNTA CHE STAVO LEGGENDO
090700070115     C     KBRV7         SETLL     FNBRV07L
090800070115     C     KBRV7         READE     FNBRV07L                               30
090900970611     C                   ENDSR
091000960506     C*
091100960506     C*---------------------------------------------------------------*
091200960506     C*                    NOBOL2                                     *
091300960506     C* CANCELLAZIONE:  - ANOMALIE SENZA BOLLA                        *
091400960506     C*                 - ANOMALIE SENZA SEGNACOLLO E SPEDIZIONE      *
091500960506     C*                 - ANOMALIE CON TIPO ANOMALIA "T"              *
091600960506     C*---------------------------------------------------------------*
091700960506     C     NOBOL2        BEGSR
091800960506     C* PRENDO LA MAGGIORE TRA DATA APERTURA E DATA CHIUSURA
091900960506     C     ANMDCH        IFGT      ANMDAO
092000020906     C                   Z-ADD     ANMDCH        W0080
092100960506     C                   ELSE
092200990510     C                   Z-ADD     ANMDAO        W0080
092300960506     C                   ENDIF
092400960506     C*
092500960508     C*
092600960903     C* SE DATA APERTURA = 0 ---> IMPOSTO 01011901 PER CERCARE DI
092700960903     C*                           CANCELLARE L'ANOMALIA
092800960903     C     ANMDAO        IFEQ      0
092900990510     C                   Z-ADD     19000101      W0080
093000960903     C                   ENDIF
093100960903     C*
093200990510    1C     W0080         IFLE      DATSPP
093300990510     C                   DELETE    FNANM002
093400960506    1C                   ENDIF
093500960506     C*
093600960506     C                   ENDSR
093700960506     C*
093800070821     C*---------------------------------------------------------------*
093900070821     c* Cancella record di FNBRV o FNBRVE
094000070821     C*---------------------------------------------------------------*
094100070821     c     BRVDEL        BEGSR
094200070821     c                   if        wfileerr=' '
094300070821     C                   DELETE    FNBRV007
094400070821     c                   else
094500070821     c                   delete    FNBRVE
094600070821     c                   endif
094700070821     C                   ENDSR
094800930519     C*---------------------------------------------------------------*
094900930521     C*                    CARTAB                                     *
095000930521     C* ROUTINE DI CARICAMENTO CATEGORIE FOGLI E FILIALI IN GESTIONE  *
095100930519     C*---------------------------------------------------------------*
095200930521     C     CARTAB        BEGSR
095300981008     C***
095400981008     C* EVENTI CHE SONO FASULLI DA NON CONSIDERARE PER LA PULIZIA
095500981008     C***
095600981008     C                   MOVEL     '2Z'          COD
095700020906     C                   Z-ADD     0             XI
095800981008     C     KTAB          SETLL     TABEL
095900981008     C     KTAB          READE     TABEL                                  31
096000981008     C*
096100981008DO  1C     *IN31         DOWEQ     '0'
096200981008IF  2C     TBLFLG        IFEQ      ' '
096300981008     C                   MOVEL     TBLUNI        DS2Z
096400981008     C*
096500981008IF  3C     §2ZEFS        IFEQ      'S'
096600020906     C                   ADD       1             XI
096700020906     C                   MOVEL     TBLKEY        EFS(XI)
096800981008E   3C                   ENDIF
096900981008E   2C                   ENDIF
097000981008     C*
097100981008     C     KTAB          READE     TABEL                                  31
097200981008E   1C                   ENDDO
097300930111     C*
097400960506     C***
097500960506     C* ANOMALIE  IDD
097600960506     C***
097700960506     C                   MOVEL     '7C'          COD
097800020906     C                   Z-ADD     0             XI
097900020906     C                   Z-ADD     0             XX
098000960506     C     KTAB          SETLL     TABEL
098100960506     C     KTAB          READE     TABEL                                  31
098200960506     C*
098300960506DO  1C     *IN31         DOWEQ     '0'
098400960506IF  2C     TBLFLG        IFEQ      ' '
098500960506     C                   MOVEL     TBLUNI        DS7C
098600960506     C*
098700960506IF  3C     §7CSAN        IFEQ      ' '
098800020906     C                   ADD       1             XI
098900020906     C                   MOVEL     TBLKEY        SAN(XI)
099000960506E   3C                   ENDIF
099100961106     C*
099200961106     C* CARICO LE ANOMALIE ESTERNE AUTOCHIUSE
099300961106IF  3C     §7CAIE        IFEQ      'E'
099400961106     C     §7CCHA        ANDEQ     'S'
099500020906     C                   ADD       1             XX
099600020906     C                   MOVEL     TBLKEY        CHA(XX)
099700961106E   3C                   ENDIF
099800960506E   2C                   ENDIF
099900960506     C*
100000960506     C     KTAB          READE     TABEL                                  31
100100960506E   1C                   ENDDO
100200061023     C***
100300061023     C* carico le anomalie di spunte per DANNI
100400061023     C***
100500061023     C                   MOVEL     '3E'          COD
100600061023     C                   Z-ADD     0             XI
100700061023     C     KTAB          SETLL     TABEL
100800061023     C     KTAB          READE     TABEL                                  31
100900061023     C*
101000061023DO  1C     *IN31         DOWEQ     '0'
101100061023IF  2C     TBLFLG        IFEQ      ' '
101200061023     C                   MOVEL     TBLUNI        DS3E
101300061023     C*
101400061023     c* Escludo le anomlie di rientro che servono per la
101500061023     c*  chiusura distinta
101600061023IF  3C     §3eFTA        IFne      'R'
101700061023     C                   ADD       1             XI
101800061023     C                   MOVEL     TBLKEY        ANOM(XI)
101900061023E   3C                   ENDIF
102000061023E   2C                   ENDIF
102100061023     C*
102200061023     C     KTAB          READE     TABEL                                  31
102300061023E   1C                   ENDDO
102400960506     C*
102500930524     C                   SETOFF                                       31
102600950403     C***
102700950403     C* DETERMINO LA DATA DI PULIZIA PIU' RECENTE
102800950403     C***
102900950403     C                   SELECT
103000950403     C     DATSPP        WHENGE    DATBRV
103100950403     C     DATSPP        ANDGE     DATBRC
103200950403     C                   Z-ADD     DATSPP        WDAT
103300950403     C*
103400950403     C     DATBRV        WHENGE    DATSPP
103500950403     C     DATBRV        ANDGE     DATBRC
103600950403     C                   Z-ADD     DATBRV        WDAT
103700950403     C*
103800950403     C                   OTHER
103900950403     C                   Z-ADD     DATBRC        WDAT
104000950403     C                   ENDSL
104100950403     C*
104200930111     C                   ENDSR
104300070821     C*---------------------------------------------------------------*
104400070821     C*                    CerDATbrv                                  *
104500070821     C* Cerco la data più recente della spunta letta                  *
104600070821     C*---------------------------------------------------------------*
104700070821     c     CerDATbrv     BEGSR
104800070821    3c                   Select
104900070821     c                   When      BrvDfs > *Date and BrvDcs > *Date
105000070821     c                   If        BrvDfs > BrvDcs
105100070821     c                   Eval      Pudcs = BrvDcs
105200070821     c                   Else
105300070821     c                   Eval      Pudcs = BrvDfs
105400070821     c                   EndIf
105500070821      * Prendo la data caricamento se data immissione maggiore di udate
105600070821     c                   When      BrvDfs > *Date
105700070821     c                   Eval      Pudcs = BrvDcs
105800070821      * Prendo la data immissione se data caricamento maggiore di udate
105900070821     c                   When      BrvDcs > *Date
106000070821     c                   Eval      Pudcs = BrvDfs
106100070821
106200070821     c                   Other
106300070821     C* PRENDO LA DATA PIÙ ALTA FRA QUELLA DI CARICAMENTO E QUELLA
106400070821     C* DI IMMISSIONE/VARIAZIONE
106500070821     C     BRVDFS        IFGE      BRVDCS
106600070821     C                   Z-ADD     BRVDFS        PUDCS
106700070821     C                   ELSE
106800070821     C                   Z-ADD     BRVDCS        PUDCS
106900070821     C                   ENDIF
107000070821    3c                   EndSl
107100070821     c                   ENDSR
107200930519     C*---------------------------------------------------------------*
107300930519     C*                    DEFVAR                                     *
107400930519     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
107500930519     C*---------------------------------------------------------------*
107600930519     C     DEFVAR        BEGSR
107700930519     C*
107800930519     C     *ENTRY        PLIST
107900930519     C                   PARM                    KPJBA
108000930519     C                   MOVEL     KPJBU         PARAM
108100051007     c
108200051007     c                   z-add     1             codut
108300930519     C*
108400941214     C* LETTURA TABEL00F
108500941214     C     KTAB          KLIST
108600051007     C                   KFLD                    CODUT             1 0
108700941214     C                   KFLD                    COD
108800941214     C* LETTURA FNBLT27L
108900941214     C     KBLT          KLIST
109000941214     C                   KFLD                    BRVLNP
109100941214     C                   KFLD                    BRVLNA
109200941214     C                   KFLD                    BRVNRS
109300941214     C                   KFLD                    BRVNSC
109400941214     C* LETTURA FNBLP01L
109500941214     C     KBLP          KLIST
109600941214     C                   KFLD                    BLTAAS
109700941214     C                   KFLD                    BLTLNP
109800941214     C                   KFLD                    BLTNRS
109900941214     C                   KFLD                    BLTNSP
110000941214     C* LETTURA FNEVB01L
110100941214     C     KEVB          KLIST
110200941214     C                   KFLD                    WAAS
110300941214     C                   KFLD                    BLTLNP
110400941214     C                   KFLD                    BLTNRS
110500941214     C                   KFLD                    BLTNSP
110600941214     C     KEVB2         KLIST
110700941214     C                   KFLD                    WAAS
110800941214     C                   KFLD                    ARTLNP
110900941214     C                   KFLD                    ARTNRS
111000941214     C                   KFLD                    ARTNSP
111100941214     C* LETTURA FNARB01L
111200941214     C     KARB          KLIST
111300941214     C                   KFLD                    ARTAAS
111400941214     C                   KFLD                    ARTLNP
111500941214     C                   KFLD                    ARTNRS
111600941214     C                   KFLD                    ARTNSP
111700970611     C* LETTURA FNEVS02L
111800970611     C     KEVS          KLIST
111900040324     C                   KFLD                    WRVLNP
112000040324     C                   KFLD                    WRVLNA
112100040324     C                   KFLD                    WRVNRS
112200040324     C                   KFLD                    WRVNSC
112300070115     C* POSIZIONAMENTO FNBRV07L
112400970611     C     KBRV          KLIST
112500970611     C                   KFLD                    WRVLNP
112600970611     C                   KFLD                    WRVLNA
112700970611     C                   KFLD                    WRVNRS
112800970611     C                   KFLD                    WRVNSC
112900970611     C     KBRV7         KLIST
113000970611     C                   KFLD                    BRVLNP
113100970611     C                   KFLD                    BRVLNA
113200970611     C                   KFLD                    BRVNRS
113300970611     C                   KFLD                    BRVNSC
113400051007     C     Kdct          KLIST
113500051007     C                   KFLD                    dctaac
113600051007     C                   KFLD                    dctfil
113700051007     C                   KFLD                    dctnca
113800930521     C*
113900930519     C                   ENDSR
