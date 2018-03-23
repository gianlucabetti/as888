000100141219     /*END
000200020402     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000300020402      * TNTA18R *----------------------------------------------------*
000400020402      *              LISTA SINTETICA TARIFFE  -PER FILIALE-   BATCH  *
000500020402      *--------------------------------------------------------------*
000600900406     FCNCLP01L  IF   E           K DISK
000700940715     FTNTAM01L  IF   E           K DISK
000800991001     FTITPT01L  IF   E           K DISK
000900991001     FTITGC01L  IF   E           K DISK
001000000000     FTABEL00F  IF   E           K DISK
001100130805     fAZCMM01L  if   e           k disk
001200000000     FCNACO00F  IF   E           K DISK
001300921012     FCNIND00F  IF   E           K DISK
001400020402     ftitad04l  if   e           k disk
001500020402     ftitpd01l  if   e           k disk
001600041007     FAZORG02L  IF   E           K DISK
001700921013     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001800921013     F                                     FORMLEN(066)
001900921013     F                                     FORMOFL(063)
002000041007
002100041007      * P.O. gestibili dall'utente
002200041007     d fig             s              3    dim(250) inz(*Zeros)
002300020402
002400161028     D TS              S             66    DIM(28) CTDATA PERRCD(1)
002500020402      * schiera righe dettaglio incongruenti
002600020405     d dts             s             66    dim(10) ctdata perrcd(1)
002700020524      *SCHIERA COMMERCIALI+RELATIVI UNIFICANTI
002800141219     d c_Max_01        c                   const(9000)
002900100202     D AG1             S              7  0 DIM(c_Max_01)
003000100202     D AG2             S              7  0 DIM(c_Max_01)
003100020524      *SCHIERA SINGOLI COMM. UNIFICANTI ORDINATA
003200100202     D AGU             S              7  0 DIM(c_Max_01) INZ(*HIVAL)
003300940715     D*
003400950406     D* PARAMETRI PASSATI DAL FILTRO
003500000000     D PARAM           DS
003600940715     D* CODICE FILIALE
003700891005     D  VIDFIL                 1      3  0
003800940715     D* DATA  DAL / AL   (GG/MM/AAAA)
003900940719     D  VIDDAL                 4     11  0
004000940719     D  VIDAL                 12     19  0
004100940715     D* CODICE COMMERCIALE
004200940719     D  VIDAGE                20     26  0
004300940715     D* DATA  DAL / AL   (AAAA/MM/GG)
004400940719     D  COMDAL                27     34  0
004500940719     D  COMAL                 35     42  0
004600940715     D* RICHIESTA STAMPA: S=IN SCADENZA / V=VARIATE / D=CON DECORRENZA
004700940719     D  FLAG                  43     43
004800950406     D* SALTO PAGINA A ROTTURA CODICE COMMERCIALE (S/ )
004900950406     D  VIDRCC                44     44
005000960621     D* COPIA FIRMATA OFFERTA: S=FIRMATE / N=NON FIRMATE / ' '=TUTTE
005100960621     D  VIDCFO               105    105
005200970407     D* SCELTA CLIENTI_: BLOCCATI NON BLOCCATI TUTTI
005300970407     D  V1CTCL               106    106
005400020402      * cod.tassazioni incongruenti con tariffa
005500020402     d  v1ccts               107    107
005600020405      * tar.partic. senza cod.tass. IT o EE
005700020405     d  v1cpar               108    108
005800020524     D* COMMERCIALE UNIFICANTE
005900020524     D  VIDAGU               109    115  0
006000020524     D* TIPO ORDINAMENTO
006100020524     D  VIDTOR               116    116
006200041007      * dati necessari x il caricamento della sk fig
006300041007     d  prmabi               117    118
006400041007     d  prmcdi               119    119
006500041007     d  prmcar               120    122  0
006600041007     d  prmcpo               123    125  0
006700041007      * aree
006800041007     d  prmare               126    155    dim(10)
006900041007     d  prmflg               156    156
006901161025      * solo preferenziali
006902161025     d  v1cftp               157    157
007000940715     D*
007100940719     D                 DS
007200940719     D  AA                     1      4  0
007300940719     D  MM                     5      6  0
007400940719     D  GG                     7      8  0
007500940719     D  DATA                   1      8  0
007600020402
007700991019     D DSTA01        E DS
007800020402     d ds1p          e ds
007900020402     d dssp          e ds
008000020524     D DS05          E DS
008100000000     D KPJBA         E DS
008200020524      *
008300020524      * Ds di riferimento al file esterno AzUte00F
008400020524     D AzuteDs       E DS                  ExtName(AzUte00F)
008500020524      *
008600020524      * Ds per dati organigramma
008700020524     D DDatiUte      E DS
008800020524      *
008900020524      * Parametri per richiamo al pgm di controllo profilo utenti
009000020524     D Tibs34Ds      E DS                  Inz
009100021120      * Parametri per richiamo al pgm trul31r
009200021120     D Trul31Ds      E DS                  Inz
009300130805      *
009400130805      * DEFINIZIONE VARIABILI
009500130805      *
009600130805     d xx              s              3  0
009700130805     d yy              s              3  0
009800130805      *
009900130805     d wokdett         s              1    inz('0')
010000130805     d wokage          s              1    inz('0')
010100130805     d wokcts          s              1    inz('1')
010200130805     d kcar            s                   like(Orgcar)
010300130805     d wcts            s                   like(tpdcts)
010400130805     d sav_tpdftc      s                   like(tpdftc)
010500130805     d wfie            s                   like(tamfie)
010600130805     d SavCMMuni       s                   like(CMMuni)  inz
010700960621     D*
010800960621     D* DEFINIZIONE COSTANTI
010900020604     D CFIRM           C                   CONST('RICHIESTE SOLO COP-
011000020604     D                                     IE FIRMATE')
011100020604     D CNFIRM          C                   CONST('RICHIESTE SOLO COP-
011200020604     D                                     IE NON FIRMATE')
011300970407     D CBLOC           C                   CONST('CLIENTI BLOCCATI ')
011400970407     D CNBLOC          C                   CONST('CLIENTI NON BLOCCA-
011500970407     D                                     TI')
011600921012     C*****************************************************************
011700921012     C* RIEPILOGO INDICATORI
011800921012     C*****************************************************************
011900921013     C* 01    - E' STATA RICHIESTA LA STAMPA DELLE TARIFFE IN SCADENZA
012000921013     C* 02    - E' STATA RICHIESTA LA STAMPA DELLE TARIFFE VARIATE
012100921013     C* 03    - E' STATA RICHIESTA LA STAMPA DELLE TARIFFE CON DECORR.
012200020402      * 04    - è stata richiesta la stampa delle incongruenze
012300020405      * 05    - stp.tar.particolari senza cod.tassazione IT o EE
012301161028     C* 06    - SOLO TARIFFE PREFERENZIALI
012400960621     C* 07    - STAMPO PARAMETRI RICHIESTI A VIDEO
012500921012     C* 10    - IMMESSO CODICE COMMERCIALE
012600921012     C* 11    - IMMESSA FILIALE
012700020524     C* 13    - NON IMMESSO SPECIFICO CODICE AGENTE UNIFICANTE
012800950612     C* 17    - USATO PER ESEGUIRE UNA ISTRUZIONE UNA SOLA VOLTA
012900921013     C* 20    - OFF  SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA
013000921013     C* 30/32 - DI COMODO
013100950720     C* 35    - USATO PER LOKUP
013200020524     C* 80    - RICHIESTO ORDINAMENTO COD.COMM.UNIFICANTE
013300921012     C*****************************************************************
013400000000     C     *ENTRY        PLIST
013500000000     C                   PARM                    KPJBA
013600921013     C                   MOVEL     KPJBU         PARAM
013700020524     C                   Z-ADD     1             CODUT             1 0
013800020524      *
013900020524      * Reperisco dati job
014000020524     C                   EXSR      DatiJob
014100940719     C*
014200921013     C                   SETON                                        OF
014300020524     C                   Z-ADD     DUTKCI        CCC
014400020524      *
014500020524      * IMPOSTO IL FLAG PER STAMPARE CORRETTAMENTE LA TESTATA A SECONDA DEL
014600020524      * TIPO ORDINAMENTO RICHIESTO
014700020524     C                   IF        VIDTOR = 'U'
014800020524     C                   EVAL      *IN80 = *ON
014900020524     C                   ELSE
015000020524     C                   EVAL      *IN80 = *OFF
015100020524     C                   ENDIF
015200020524      *
015300020524      * NON IMMESSA NESSUNA SCELTA COMMERCIALE UNIFICANTE
015400020524     C     VIDAGU        IFEQ      0
015500020524     C                   SETON                                        13
015600020524     C                   ELSE
015700020524     C                   SETOFF                                       13
015800020524     C                   ENDIF
015900020524      * CARICO IN SCHIERA TUTTI I CODICI
016000130805      * COMMERCIALI UNIFICANTI PRESENTI IN ANAGRAFICA
016100020524     C     VIDTOR        IFEQ      'U'
016200020524     C                   Z-ADD     1             X                 4 0
016300130805     C     *loval        SETLL     AZCMM000
016400130805     C                   READ      AZCMM000
016500020524      *
016600130805     c                   DOW       Not %eof(AZCMM01L)
016700130805     C     CMMuni        LOOKUP    AGU                                    35
016800130805     C  N35              MOVE      CMMuni        AGU(X)
016900020524     C  N35              ADD       1             X
017000130805     C                   READ      AZCMM000
017100020524     C                   ENDDO
017200020524      *
017300020524     C                   Z-ADD     X             W                 4 0
017400020524      * ORDINO LA SCHIERA IN MODO CRESCENTE
017500020524     C                   SORTA     AGU
017600020524      * CARICO ANCHE TUTTI I COMMERCIALI CON RELATIVI UNIFICANTI
017700130805     C                   Z-ADD     1             X
017800130805     C     *loval        SETLL     AZCMM000
017900130805     C                   READ      AZCMM000
018000020524      *
018100130805     c                   DOW       Not %eof(AZCMM01L)
018200130805     C                   MOVEL     CMMcod        AG1(X)
018300130805     C                   MOVE      CMMuni        AG2(X)
018400020524     C                   ADD       1             X
018500130805     C                   READ      AZCMM000
018600020524     C                   ENDDO
018700020524     C                   ENDIF
018800020524      *
018900041007     c                   Reset                   fig
019000041007      * se ho le aree carico una sk con tutti i p.o. relativi alle aree
019100041007      * richieste
019200041007if  1c                   If        prmflg <> *Blanks
019300041007do  2c                   do        10            xx
019400041007if  3c                   If        prmare(xx) <> *Blanks
019500041007     c                   move      prmare(xx)    kcar
019600041007     c     kAzorg        Setll     Azorg02l
019700041007do  4c                   Do        *Hival
019800041007     c     kAzorg        Reade     Azorg02l
019900041007     c                   If        %Eof(Azorg02l)
020000041007     c                   Leave
020100041007     c                   EndIf
020200041007     c                   If        OrgFva <> *Blanks or
020300041007     c                             (OrgFag <> 'A' and OrgFag <> 'F')
020400041007     c                   Iter
020500041007     c                   EndIf
020600041007     c                   Move      OrgFil        w003a             3
020700041007     c     w003a         Lookup    Fig                                    30
020800041007if  5c                   If        Not *In30
020900041007     c                   Eval      yy = 1
021000041007     c     *Zeros        Lookup    fig(yy)                                30
021100041007if  6c                   If        *In30
021200041007     c                   Eval      fig(yy) = w003a
021300041007e   6c                   EndIf
021400041007e   5c                   EndIf
021500041007e   4c                   EndDo
021600041007e   3c                   EndIf
021700041007e   2c                   EndDo
021800041007
021900041007   x1c                   Else
022000041007      * Reperimento dei P.O. gestibili dall'utente
022100041007     c                   clear                   TRUL31DS
022200041007     c                   eval      I31abi = prmabi
022300041007     c                   eval      I31cdi = prmcdi
022400041007     c                   eval      I31car = prmcar
022500041007     c                   eval      I31cpo = prmcpo
022600041007     c                   call      'TRUL31R'
022700041007     c                   parm                    KPJBA
022800041007     c                   parm                    TRUL31DS
022900041007     c                   if        O31pog > *zeros
023000041007     c                   movea     O31pog        fig
023100041007     c                   endif
023200041007e   1c                   EndIf
023300921012     C*
023400921013     C* CONTROLLO LA RICHIESTA STAMPA
023500921013     C     FLAG          IFEQ      'S'
023600921013     C                   SETON                                        01
023700921013     C                   ELSE
023800921013     C*
023900921013     C     FLAG          IFEQ      'V'
024000921013     C                   SETON                                        02
024100921013     C                   ELSE
024200921013     C*
024300921013     C     FLAG          IFEQ      'D'
024400921013     C                   SETON                                        03
024500921013     C                   END
024600921013     C*
024700921013     C                   END
024800921013     C                   END
024900020402
025000020402     c                   if        v1ccts = 'S'
025100020402     c                   eval      *in04 = *on
025200020402     c                   endif
025300020405
025400020405     c                   if        v1cpar = 'S'
025500020405     c                   eval      *in05 = *on
025600020405     c                   endif
025601161028      * solo tariffe preferenziali
025602161028     c                   if        v1cFTP = 'P'
025603161028     c                   eval      *in06 = *on
025604161028     c                   endif
025700020402
025800900406     C*
025900960621     C* CONTROLLO SE RICHIESTE COPIE FIRMATE OFFERTA
026000960621     C     VIDCFO        IFNE      ' '
026100960621     C     VIDCFO        IFEQ      'S'
026200020604     C                   MOVEL     CFIRM         WCFO             33
026300960621     C                   ELSE
026400960621     C                   MOVEL     CNFIRM        WCFO
026500960621     C                   ENDIF
026600960621     C                   ENDIF
026700970407     C* CLIENTI
026800970407     C     V1CTCL        IFNE      ' '
026900970407     C     V1CTCL        IFEQ      'B'
027000970407     C                   MOVEL     CBLOC         WCCL             20
027100970407     C                   ELSE
027200970407     C                   MOVEL     CNBLOC        WCCL
027300970407     C                   ENDIF
027400970407     C                   ENDIF
027500960621     C*
027600921012     C* 10 ON - E' STATO IMMESSO IL CODICE AGENTE
027700900406     C     VIDAGE        IFGT      0
027800940719     C                   Z-ADD     VIDAGE        WAGE
027900900406     C                   SETON                                        10
028000900406     C                   ELSE
028100921012     C* 11 ON - E' STATA IMMESSA LA FILIALE
028200900406     C     VIDFIL        IFGT      0
028300900406     C                   SETON                                        11
028400940719     C                   MOVEL     VIDFIL        WAGE
028500900406     C                   END
028600900406     C                   END
028700921012     C*
028800900406     C* POSIZIONAMENTO
028900020524      * SE RICHIESTO ORDINAMENTO PER CODICE COMMERCIALE
029000020524      * LEGGO LE VISITE UTILIZZANDO VIDAGE COSI' COME GIA' IMPOSTATO
029100020524   1aC     VIDTOR        IFEQ      'C'
029200020527     C                   IF        *IN10=*ON OR *IN11=*ON
029300020527     C     KCLP          SETLL     CNCLP000
029400020527     C                   ENDIF
029500020527     C                   IF        *IN10=*OFF AND  *IN11=*OFF
029600020527     C     KCLP1         SETLL     CNCLP000
029700020527     C                   ENDIF
029800020524   1eC                   ELSE
029900020524      *
030000020524      * SE RICHIESTO ORDINAMENTO PER CODICE COMMERCIALE UNIFICANTE
030100020524      * LEGGO LE VISITE DEI CODICI COMMERCIALI CARICATI IN SCHIERA
030200020524     C                   Z-ADD     1             X                 4 0
030300020524     C                   Z-ADD     1             Y                 4 0
030400020524     C  N13              DO
030500020524     C     VIDAGU        LOOKUP    AG2(Y)                                 35
030600020524     C     VIDAGU        LOOKUP    AGU(X)                                 35
030700020524     C                   ENDDO
030800020524      *
030900020524     C   13              DO
031000020524     C     AGU(X)        LOOKUP    AG2(Y)                                 35
031100020524     C                   ENDDO
031200020524     C                   MOVE      AG1(Y)        WAGE
031300020524     C     KCLP          SETLL     CNCLP000
031400020524   1-C                   ENDIF
031500020524     C
031600900406     C*
031700900406     C* LETTURA
031800900406     C     START         TAG
031900020524   1aC     VIDTOR        IFEQ      'C'
032000900406     C   10KCLP          READE     CNCLP000                               30
032100900406     C  N10              READ      CNCLP000                               30
032200020524   1eC                   ELSE
032300020524     C     KCLP          READE     CNCLP000                               30
032400020524      *
032500020524      * DISTINGUO I CASI DI SCELTA PER SINGOLO AGENTE UNIFICANTE O MENO
032600020524   2aC     *IN13         IFEQ      *ON
032700020524      *
032800020524      * CICLO SINO A CHE NON HO ESAURITO LA SCHIRA DEGLI UNIFICANTI
032900020524   3aC     *IN30         DOWEQ     *ON
033000020524     C     X             ANDLT     W
033100020524     C                   SETON                                        31
033200020524      *
033300020524      * CICLO SULLA SCHIERA DI TUTTI I CODICI, SCEGLIENDO SOLO QUELLI
033400020524      * CON CODICE UNIFICANTE UGUALE A QUELLO IN LETTURA
033500020524   4AC     *IN31         DOWEQ     *ON
033600020524     C     AG2(Y)        ANDNE     *ZEROS
033700020524     C                   ADD       1             Y
033800020524   5aC     AGU(X)        IFEQ      AG2(Y)
033900020524     C                   MOVE      AG1(Y)        WAGE
034000020524     C     KCLP          SETLL     CNCLP000
034100020524     C                   SETOFF                                       31
034200020524   5-C                   ENDIF
034300020524   4-C                   ENDDO
034400020524      *
034500020524      * SE NON HO LETTO NULLA PASSO AL CODICE UNIFICANTE
034600020524      * SUCCESSIVO
034700020524   4AC     *IN31         IFEQ      *ON
034800020524     C                   ADD       1             X
034900020524     C                   Z-ADD     1             Y
035000020524     C     AGU(X)        LOOKUP    AG2(Y)                                 35
035100020524     C                   MOVE      AG1(Y)        WAGE
035200020524     C     KCLP          SETLL     CNCLP000
035300020524   4-C                   ENDIF
035400020524      *
035500020524      * MI SONO RIPOSIZIONATA
035600020524     C                   GOTO      START
035700020524   3-C                   ENDDO
035800020524      *
035900020524      * HO SCELTO UN SINGOLO UNIFICANTE
036000020524   2eC                   ELSE
036100020524   3aC     *IN30         DOWEQ     *ON
036200020524     C     AG1(Y)        ANDNE     *ZEROS
036300020524     C                   ADD       1             Y
036400020524   4aC     AGU(X)        IFEQ      AG2(Y)
036500020524     C                   MOVE      AG1(Y)        WAGE
036600020524     C     KCLP          SETLL     CNCLP000
036700020524     C     KCLP          READE     CNCLP000                               30
036800020524   4-C                   ENDIF
036900020524   3-C                   ENDDO
037000020524   2-C                   ENDIF
037100020524   1-C                   ENDIF
037200020524     C*
037300020524     C* SE IMMESSA FILIALE CONTROLLO CHE CODICE LETTO SIA
037400020524     C*   DI QUELLA FILIALE
037500020524     C   11
037600020524     CANN30              DO
037700020524     C                   MOVEL     CLPAGE        FILAGE            3 0
037800020528     C     FILAGE        IFNE      VIDFIL
037900020528     C                   GOTO      START
038000020524     C                   END
038100020524     C                   END
038200020524     C* FINE LETTURA
038300020524     C   30              GOTO      FINE
038400020524      *
038500130805     c                   clear                   SavCMMuni
038600130805     C     CLPage        CHAIN     AZCMM000                           32
038700130805     C   32              clear                   CMMuni
038800130805     c                   eval      SavCMMuni = CMMuni
038900040507      *
039000020524      * SE L'ORDINAMENTO E' DI TIPO 'C' MA HO SCELTO UN CODICE
039100020524      * UNIFICANTE DEVO VERIFICARE CHE IL CODICE LETTO ABBIA IL
039200020524      * CORRISPONDENTE UNIFICANTE UGUALE A QUELLO SELEZIONATO
039300020524   1aC     VIDTOR        IFEQ      'C'
039400020524     C     *IN13         ANDEQ     *OFF
039500130805     C     SavCMMuni     IFNE      VIDAGU
039600020524     C                   GOTO      START
039700020524     C                   ENDIF
039800020524     C                   ENDIF
039900020524      *
040000020524      * SE L'ORDINAMENTO E' DI TIPO 'U' ED HO SCELTO UN CODICE
040100020524      * DEVO CONTROLLARE CHE SIA UGUALE A QUELLO LETTO
040200020524   1aC     VIDTOR        IFEQ      'U'
040300020524     C     *IN10         ANDEQ     *ON
040400020524     C     CLPAGE        ANDNE     VIDAGE
040500020524     C                   GOTO      START
040600020524     C                   ENDIF
040700020524      *
040800041007     C* SE NON SONO RICHIESTI NE' LA FILIALE NE' IL
040900950802     C*   CODICE COMMERCIALE, ELABORO SOLO LE FILIALI GESTITE DALLA
041000950802     C*   FILIALE COLLEGATA
041100041007     C     *IN10         ifeq      *OFF
041200950720     C     *IN11         ANDEQ     *OFF
041300041007     C                   MOVEL     CLPAGE        WFIL              3
041400950720     C     WFIL          LOOKUP    FIG                                    35
041500040507     c*
041600040507     c                   if        not *in35
041700130805     c                   movel     SavCMMuni     wporagru          3
041800040507     c     wporagru      lookup    fig                                    35
041900040507     C  N35              GOTO      START
042000950720     C                   ENDIF
042100040507     C                   ENDIF
042200950720     C*
042300900620     C     KACO          CHAIN     CNACO000                           31
042400970407     C* ESCLUDO I BLOCCATI
042500970407     C  N31V1CTCL        IFEQ      'N'
042600130315     C     ACOABL        ANDne     *blanks
042700900620     C                   GOTO      START
042800900620     C                   END
042900970407     C* ESCLUDO I NON BLOCCATI
043000970407     C  N31V1CTCL        IFEQ      'B'
043100130315     C     ACOABL        ANDeq     *blanks
043200970407     C                   GOTO      START
043300970407     C                   END
043400970407     C*
043500900620     C   31              GOTO      START
043600970407     C*
043700130315     C     ACOABL        IFne      *blanks
043800970407     C                   MOVEL     'BL'          WBLOC             2
043900970407     C                   ELSE
044000970407     C                   CLEAR                   WBLOC
044100970407     C                   ENDIF
044200020524     C*
044300900406     C* RICERCA TARIFFA
044400900406     C                   EXSR      RICTAR
044500900406     C                   GOTO      START
044600900406     C*
044700900406     C     FINE          TAG
044800921013     C* SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA
044900960621     C     *IN20         IFEQ      *OFF
045000960621     C                   SETON                                        07
045100020528     C                   CLEAR                   CLPAGE
045200020528     C                   CLEAR                   RAGCOM
045300020528     C                   CLEAR                   CLPAGU
045400020528     C                   CLEAR                   DESAGU
045500020528     C* SE IMMESSO SPECIFICO COD AGENTE LO DECODIFICO
045600020528     C     *IN10         IFEQ      *ON
045700020528     C                   MOVEL     VIDAGE        CLPAGE
045800020528     c                   CLEAR                   RAGCOM
045900130805     c     CLPage        CHAIN     AZCMM000
046000130805     c                   IF        %FOUND(AZCMM01L)
046100130805     c                   MOVEL     CMMdes        RAGCOM
046200020613     c                   ELSE
046300020613     c                   MOVEL     *BLANKS       RAGCOM
046400020528     c                   ENDIF
046500020528     C                   ENDIF
046600020528     C*
046700020528     C* SE IMMESSO SPECIFICO COD AGENTE UNIFICANTE LO DECODIFICO
046800020528     C     *IN13         IFEQ      *OFF
046900020528     C                   MOVEL     VIDAGU        CLPAGU
047000130805     C                   MOVEL     VIDAGU        SavCMMuni
047100130805     c     SavCMMuni     CHAIN     AZCMM01L
047200130805     c                   IF        %FOUND(AZCMM01L)
047300130805     c*//                MOVEL     SavCMMuni     CLPAGU
047400130805     c                   MOVEL     CMMdes        DESAGU
047500020613     c                   ELSE
047600130805     c*//                MOVEL     SavCMMuni     CLPAGU
047700020613     c                   MOVEL     *BLANKS       DESAGU
047800020528     C                   ENDIF
047900020528     C                   ENDIF
048000020528      *
048100960621     C                   EXCEPT    TESTA
048200960621     C                   EXCEPT    TESTA2
048300960621     C                   ENDIF
048400921013     C*
048500900406     C                   SETON                                        LR
048600921012     C*
048700921012     C*--- RICERCA TARIFFE DI QUEL AGENTE NEI LIMITI RICHIESTI -------*
048800900406     C     RICTAR        BEGSR
048900950612     C                   SETOFF                                       17
049000950406     C*
049100940715     C     CLPKSC        SETLL     TNTAM000
049200950406     C     CLPKSC        READE     TNTAM000                               32
049300950406     C*
049400950406     C     *IN32         DOWEQ     *OFF
049500900406     C* SE ANNULLATO NON CONSIDERO
049600950406     C     TAMATB        IFEQ      ' '
049700950612     C*
049800950612     C* SALVO SOLO LA PRIMA VOLTA
049900950612     C  N17              Z-ADD     TAMCTR        SAVCTR
050000950612     C  N17              SETON                                        17
050100950612     C*
050200950612     C* SE RICHIESTE LE TARIFFE IN SCADENZA BISOGNA CONSIDERARE SOLO
050300950612     C*   L'ULTIMO PROGRESSIVO DELLA TARIFFA E VEDERE SE E' IN SCADENZA
050400950612     C   01TAMCTR        IFNE      SAVCTR
050500950612     C                   EXSR      STAMPA
050600950612     C                   Z-ADD     TAMCTR        SAVCTR
050700950612     C                   ENDIF
050800950612     C*
050900950612     C                   Z-ADD     TAMKSC        WKSC
051000950612     C                   Z-ADD     TAMCTR        WCTR
051100950612     C                   Z-ADD     TAMPRG        WPRG
051200950612     C                   Z-ADD     TAMDDT        WDDT
051300950612     C                   Z-ADD     TAMDST        WDST
051400020409
051500020409     c                   eval      wfie = tamfie
051600020409
051700991019     C* RECUPERO LA DIVISA
051800991019     C                   MOVEL     TAMFLO        DSTA01
051900991019     C                   MOVEL     §TADIV        WDIV
052000991001     C*
052100991001     C* CERCO LA DATA ULTIMA VARIAZIONE PIU' ALTA DI TUTTI
052200991001     C                   EXSR      CERDUV
052300950612     C                   Z-ADD     TAMDUV        WDUV
052400960723     C                   MOVEL     TAMSIS        WSIS
052500960726     C                   MOVEL     TAMBAP        WBAP
052501161025     C                   MOVEL     TAMFTP        WFTP
052600950612     C*
052700950612     C* SE RICHIESTE LE TARIFFE VARIATO O LE TARIFFE IN DECORRENZA
052800950612     C*   STAMPO TUTTI I PROGRESSIVI DI TUTTI I CODICI TARIFFA
052900950612     C  N01              EXSR      STAMPA
053000900406     C                   END
053100900406     C*
053200950406     C     CLPKSC        READE     TNTAM000                               32
053300950406     C                   ENDDO
053400950612     C*
053500950612     C* SE RICHIESTE LE TARIFFE IN SCADENZA STAMPO ULTIMA LETTA
053600950612     C   01
053700950612     CAN 17
053800950612     CAN 32              EXSR      STAMPA
053900900406     C                   ENDSR
054000921012     C*
054100921012     C*--- STAMPA TARIFFE --------------------------------------------*
054200900406     C     STAMPA        BEGSR
054300020402
054400921013     C*
054500921013     C* CONTROLLO CHE LA DATA SCADENZA NON SUPERI I LIMITI
054600950612     C  N02
054700950612     CANN03WDST          IFLT      COMDAL
054800950612     C     WDST          ORGT      COMAL
054900921013     C                   GOTO      ENDSTA
055000921013     C                   END
055100921013     C* CONTROLLO CHE LA DATA DECORRENZA NON SUPERI I LIMITI
055200950612     C  N01
055300950612     CANN02WDDT          IFLT      COMDAL
055400950612     C     WDDT          ORGT      COMAL
055500921013     C                   GOTO      ENDSTA
055600921013     C                   END
055700921013     C* CONTROLLO CHE LA DATA ULTIMA VARIAZIONE NON SUPERI I LIMITI
055800950612     C  N01
055900950612     CANN03WDUV          IFLT      COMDAL
056000950612     C     WDUV          ORGT      COMAL
056100921013     C                   GOTO      ENDSTA
056200921013     C                   END
056300921013     C*
056400960726     C* SOLO PER LE TARIFFE IN SCADENZA ESCLUDO LE TARIFFE BLOCCATE
056500960726     C     *IN01         IFEQ      *ON
056600960726     C     WBAP          ANDEQ     'B'
056700960726     C                   GOTO      ENDSTA
056800960726     C                   ENDIF
056801161025      * se richieste solo le prefenziali escudo quelle che non lo sono
056802161025     c     v1cftp        ifeq      'P'
056803161025     c     WFTP          andne     'P'
056804161025     c                   goto      endsta
056805161025     c                   endif
056900960726     C*
057000960621     C* SE RICHIESTE CONTROLLO LE COPIE FIRMATE OFFERTA
057100960621     C     VIDCFO        IFNE      ' '
057200960621     C* RICHIESTE SOLO LE FIRMATE
057300960621     C     VIDCFO        IFEQ      'S'
057400960723     C     WSIS          ANDNE     'S'
057500960621     C                   GOTO      ENDSTA
057600960621     C                   ENDIF
057700960621     C* RICHIESTE SOLO LE NON FIRMATE
057800960621     C     VIDCFO        IFEQ      'N'
057900960723     C     WSIS          ANDNE     ' '
058000960621     C                   GOTO      ENDSTA
058100960621     C                   ENDIF
058200960621     C                   ENDIF
058300960621     C*
058400921013     C* CERCO ANAGRAFICA
058500900406     C                   EXSR      RICANA
058600020405      * se non sono state richieste le stampe di controllo
058700020405      * per i codici tassazioni generici IT o EE
058800020405     c                   if        not *in04 and not *in05
058900921013     C*
059000950406     C* SE CAMBIA COMMERCIALE DECODIFICO E SALTO PAGINA SE RICHIESTO
059100900406     C     CLPAGE        IFNE      PREAGE
059200020402     C                   MOVEL     CLPAGE        PREAGE
059300020402     c                   exsr      stpage
059400900406     C                   END
059500921013     C*
059600921013     C* STAMPO TESTATA
059700950406     C     *INOF         IFEQ      *ON
059800960621     C                   SETON                                        07
059900950406     C                   EXCEPT    TESTA
060000950406     C                   EXCEPT    TESTA2
060100960621     C                   SETOFF                                       OF07
060200950406     C                   ENDIF
060300020402
060400020402     c                   endif
060500020402
060600921012     C*
060700921012     C* GIRO DATA SCADENZA
060800950612     C                   Z-ADD     WDST          DATA
060900940719     C                   Z-ADD     MM            GGMM              4 0
061000940719     C                   MOVEL     GG            GGMM
061100940719     C                   Z-ADD     AA            DATSCA
061200940719     C                   MOVEL     GGMM          DATSCA
061300900406     C*
061400921012     C* GIRO DATA DECORRENZA
061500950612     C                   Z-ADD     WDDT          DATA
061600940719     C                   Z-ADD     MM            GGMM
061700940719     C                   MOVEL     GG            GGMM
061800940719     C                   Z-ADD     AA            DATDEC
061900940719     C                   MOVEL     GGMM          DATDEC
062000921012     C*
062100921013     C* GIRO DATA ULTIMA VARIAZIONE
062200950612     C                   Z-ADD     WDUV          DATA
062300940719     C                   Z-ADD     MM            GGMM
062400940719     C                   MOVEL     GG            GGMM
062500940719     C                   Z-ADD     AA            DATVAR
062600940719     C                   MOVEL     GGMM          DATVAR
062700921013     C*
062800921013     C* STAMPO DETTAGLIO
062900020405     c                   if        not *in04 and not *in05
063000000000     C                   EXCEPT    DETT
063100020402     c                   endif
063200020402
063300020405      * se richiesto il controllo dei codici tassazione IT o EE
063400020405      * stampo solo quelle errate
063500020405     c                   if        *in04 or *in05
063600020402     c                   exsr      ctrcts
063700020402     c                   endif
063800020402
063900921014     C*
064000921014     C                   SETON                                        20
064100900406     C     ENDSTA        ENDSR
064200921012     C*
064300921012     C*--- RICERCA DATI ANAGRAFICI -----------------------------------*
064400900406     C     RICANA        BEGSR
064500900406     C     KCLI          CHAIN     CNACO000                           15
064600900406     C     KCLI          CHAIN     CNIND000                           15
064700900406     C   15              DO
064800900406     C                   MOVEL     *BLANKS       ACORAG
064900900406     C                   MOVEL     *BLANKS       INDCIT
065000900406     C                   END
065100991019     C                   MOVEL     ACORAG        WRAG             38
065200960621     C                   MOVEL     INDCIT        WCIT             25
065300900406     C                   ENDSR
065400991001     C*
065500991001     C*---------------------------------------------------------------*
065600991001     C*--- RICERCA ULTIMA DATA VARIAZIONE TARIFFA --------------------*
065700991001     C     CERDUV        BEGSR
065800991001     C*
065900991001     C* LEGGO TUTTE LE TESTATE TARIFFE PARTICOLARI  CERCANDO LA DATA
066000991001     C* MAGGIORE DI ULTIMA MODIFICA
066100991001     C     KTPT          SETLL     TITPT01L
066200991001     C     KTPT          READE     TITPT01L                               33
066300991001     C*
066400991001     C     *IN33         DOWEQ     *OFF
066500991001     C* VERIFICO SE LA DATA ULTIMA VARIAZIONE E' MAGGIORE DI QUELLA
066600991001     C* DELLA TESTATA
066700991001     C     TPTDUV        IFGT      TAMDUV
066800991001     C     TPTATB        ANDEQ     ' '
066900991001     C                   Z-ADD     TPTDUV        TAMDUV
067000991001     C                   ENDIF
067100991001     C*
067200991001     C     KTPT          READE     TITPT01L                               33
067300991001     C*
067400991001     C                   ENDDO
067500991001     C* VERIFICO ANCHE L'ULTIMA DATA VARIAZIONE TARIFFA CHE C'E' IN
067600991001     C* TITGC TARIFFE GIACENZE  (SE ESISTE)
067700991001     C     KTPT          CHAIN     TITGC01L                           33
067800991001     C     *IN33         IFEQ      *OFF
067900991001     C     TGCATB        ANDEQ     ' '
068000991001     C     TGCDUV        ANDGT     TAMDUV
068100991001     C                   Z-ADD     TGCDUV        TAMDUV
068200991001     C                   ENDIF
068300991001     C*
068400991001     C                   ENDSR
068500020402      *---------------------------------------------------------------*
068600020402      * STPAGE - stampa codice commerciale
068700020402      *---------------------------------------------------------------*
068800020402     c     stpage        begsr
068900020524      *
069000020524      * decodifica commerciale
069100161028     c                   clear                   ragcom           18
069200130805     c     CLPage        chain     AZCMM000
069300130805     c                   if        %found(AZCMM01L)
069400130805     c                   movel     CMMdes        ragcom
069500130805     c                   movel     CMMuni        SavCMMuni
069600020613     c                   else
069700130805     c                   movel     clpage        SavCMMuni
069800020402     c                   endif
069900020524      * decodifica unificante
070000161028     C                   clear                   desagu           18
070100130805     C     SavCMMuni     ifne      0
070200130805     c     SavCMMuni     chain     AZCMM000
070300130805     c                   if        %found(AZCMM01L)
070400130805     c                   movel     SavCMMuni     clpagu
070500130805     c                   movel     CMMdes        desagu
070600020524     c                   endif
070700020524     c                   endif
070800020529      *
070900020528     C     vidtor        ifeq      'C'
071000020402      * IL SALTO PAGINA LO ESEGUO SOLO SE RICHIESTO NEI PARAMETRI
071100020402     c                   if        vidrcc = 'S'
071200020402     c                   eval      *inof = *on
071300020402     c                   else
071400020402     c                   if        not *inof
071500020402     c                   except    fitt
071600020402     c                   except    testa2
071700020402     c                   endif
071800020402     c                   endif
071900020529      *
072000020528     c                   else
072100020528      * SE ORDINAMENTO 'U' PER FARE IL SALTO PAGINA DEVE ESSERE
072200020528      * CAMBIATO L'UNIFICANTE
072300020528     c                   if        vidrcc = 'S'
072400020529     c                   if        clpagu <> PreAgu
072500020528     c                   eval      *inof = *on
072600020529     c                   eval      PreAgu = clpagu
072700020528     c                   else
072800020528     c                   if        not *inof
072900020528     c                   except    fitt
073000020528     c                   except    testa2
073100020528     c                   endif
073200020528     c                   endif
073300020528     c                   else
073400020528     c                   if        not *inof
073500020528     c                   except    fitt
073600020528     c                   except    testa2
073700020528     c                   endif
073800020528     c                   endif
073900020528     C                   endif
074000020402
074100020402     c                   endsr
074200020402      *---------------------------------------------------------------*
074300020402      * CTRCTS - controllo codici tassazione
074400020402      *---------------------------------------------------------------*
074500020402     c     ctrcts        begsr
074600020409
074700020409     c                   if        wfie = 'I'
074800020409     c                   eval      wcts = 'IT'
074900020409     c                   endif
075000020409     c                   if        wfie = 'E'
075100020409     c                   eval      wcts = 'EE'
075200020409     c                   endif
075300020402
075400020402     c                   eval      wokdett = *off
075500020402     c                   eval      wokage  = *off
075600020405      * leggo il dettaglio tariffario solo se richiesta la stampa delle
075700020405      * incongruenze
075800020408 1b  c                   if        *in04
075900020405
076000020402      * dettaglio tariffe
076100020409     c     kwtpt         setll     titad04l
076200020408 2b  c                   do        *hival
076300020409     c     kwtpt         reade     titad04l
076400020402     c                   if        %eof(titad04l)
076500020402     c                   leave
076600020402     c                   endif
076700020410     c                   if        tadatb <> *blanks
076800020410     c                   iter
076900020410     c                   endif
077000020402
077100020409 3b  c                   if        wfie = 'I' and tadcts = 'EE'
077200020402      * rottura codice commerciale
077300020402     c                   if        clpage <> preage and wokage = *off
077400020402     c                   eval      preage = clpage
077500020402     c                   exsr      stpage
077600020402     c                   eval      wokage = *on
077700020402     c                   endif
077800020402      * testata
077900020402     c                   if        *inof
078000020402     c                   eval      *in07 = *on
078100020402     c                   except    testa
078200020402     c                   except    testa2
078300020402     c                   eval      *in07 = *off
078400020402     c                   eval      *inof = *off
078500020402     c                   endif
078600020402      * dettaglio
078700020402     c                   if        wokdett = *off
078800020402     c                   except    dett
078900020402     c                   eval      wokdett = *on
079000020402     c                   endif
079100020402     c                   except    dett1
079200020408 3e  c                   endif
079300020402
079400020409 3b  c                   if        wfie = 'E' and tadcts = 'IT'
079500020402      * rottura codice commerciale
079600020402     c                   if        clpage <> preage and wokage = *off
079700020402     c                   eval      preage = clpage
079800020402     c                   exsr      stpage
079900020402     c                   eval      wokage = *on
080000020402     c                   endif
080100020402      * testata
080200020402     c                   if        *inof
080300020402     c                   eval      *in07 = *on
080400020402     c                   except    testa
080500020402     c                   except    testa2
080600020402     c                   eval      *in07 = *off
080700020402     c                   eval      *inof = *off
080800020402     c                   endif
080900020402      * dettaglio
081000020402     c                   if        wokdett = *off
081100020402     c                   except    dett
081200020402     c                   eval      wokdett = *on
081300020402     c                   endif
081400020402     c                   except    dett2
081500020408 3e  c                   endif
081600020408 2e  c                   enddo
081700020405
081800020408 1e  c                   endif
081900020402
082000020409      * leggo le tariffe particolari
082100020409     c     kwtpt         setll     titpd01l
082200020408 1b  c                   do        *hival
082300020409     c     kwtpt         reade     titpd01l
082400020402     c                   if        %eof(titpd01l)
082500020402     c                   leave
082600020402     c                   endif
082700020417      * record annullato
082800020417     c                   if        tpdatb <> *blanks
082900020417     c                   iter
083000020417     c                   endif
083100020409      * se devo controllare le mancanze
083200020409 2b  c                   if        *in05
083300020409      * a rottura di tariffa particolare stampo l'errore
083400020409 3b  c                   if        tpdftc <> sav_tpdftc and
083500020409     c                             sav_tpdftc <> *blanks
083600020409 4b  c                   if        wokcts = *on
083700020409      * rottura codice commerciale
083800020409     c                   if        clpage <> preage and wokage = *off
083900020409     c                   eval      preage = clpage
084000020409     c                   exsr      stpage
084100020409     c                   eval      wokage = *on
084200020409     c                   endif
084300020409      * testata
084400020409     c                   if        *inof
084500020409     c                   eval      *in07 = *on
084600020409     c                   except    testa
084700020409     c                   except    testa2
084800020409     c                   eval      *in07 = *off
084900020409     c                   eval      *inof = *off
085000020409     c                   endif
085100020409      * dettaglio
085200020409     c                   if        wokdett = *off
085300020409     c                   except    dett
085400020409     c                   eval      wokdett = *on
085500020409     c                   endif
085600020409     c                   movel     sav_tpdftc    codftc
085700020409     c                   exsr      ricftc
085800020409     c                   except    dett5
085900020409 4e  c                   endif
086000020409     c                   eval      sav_tpdftc = tpdftc
086100020409     c                   eval      wokcts = *on
086200020409 3e  c                   endif
086300020409 2e  c                   endif
086400020402
086500020405      * se richiesto controllo le incongruenze
086600020408 2b  c                   if        *in04
086700020405
086800020409 3b  c                   if        wfie = 'I' and tpdcts = 'EE'
086900020402      * rottura codice commerciale
087000020402     c                   if        clpage <> preage and wokage = *off
087100020402     c                   eval      preage = clpage
087200020402     c                   exsr      stpage
087300020402     c                   eval      wokage = *on
087400020402     c                   endif
087500020402      * testata
087600020402     c                   if        *inof
087700020402     c                   eval      *in07 = *on
087800020402     c                   except    testa
087900020402     c                   except    testa2
088000020402     c                   eval      *in07 = *off
088100020402     c                   eval      *inof = *off
088200020402     c                   endif
088300020402      * dettaglio
088400020402     c                   if        wokdett = *off
088500020402     c                   except    dett
088600020402     c                   eval      wokdett = *on
088700020402     c                   endif
088800020408     c                   movel     tpdftc        codftc            1
088900020402     c                   exsr      ricftc
089000020402     c                   except    dett3
089100020408 3e  c                   endif
089200020402
089300020409 3b  c                   if        wfie = 'E' and tpdcts = 'IT'
089400020402      * rottura codice commerciale
089500020402     c                   if        clpage <> preage and wokage = *off
089600020402     c                   eval      preage = clpage
089700020402     c                   exsr      stpage
089800020402     c                   eval      wokage = *on
089900020402     c                   endif
090000020402      * testata
090100020402     c                   if        *inof
090200020402     c                   eval      *in07 = *on
090300020402     c                   except    testa
090400020402     c                   except    testa2
090500020402     c                   eval      *in07 = *off
090600020402     c                   eval      *inof = *off
090700020402     c                   endif
090800020402      * dettaglio
090900020402     c                   if        wokdett = *off
091000020402     c                   except    dett
091100020402     c                   eval      wokdett = *on
091200020402     c                   endif
091300020408     c                   movel     tpdftc        codftc
091400020402     c                   exsr      ricftc
091500020402     c                   except    dett4
091600020408 3e  c                   endif
091700020405
091800020408 2e  c                   endif
091900020405
092000020408      * se richiesto controllo le mancanze
092100020408 2b  c                   if        *in05
092200020417      * per tariffe FedEx non devo controllare
092300020417     c                   if        §tafed = 'S'
092400020417     c                   iter
092500020417     c                   endif
092600020408
092700020405     c                   if        sav_tpdftc = *blanks
092800020405     c                   eval      sav_tpdftc = tpdftc
092900020405     c                   endif
093000020408
093100020409 3b  c                   if        sav_tpdftc = tpdftc
093200020408     c                   select
093300020409     c                   when      wokcts = *on and wfie = 'I'
093400020409     c                             and tpdcts = 'IT' and tpdsgl >= 99999
093500020408     c                   eval      wokcts = *off
093600020409     c                   when      wokcts = *on and wfie = 'E'
093700020409     c                             and tpdcts =  'EE' and tpdsgl >= 99999
093800020408     c                   eval      wokcts = *off
093900020408     c                   endsl
094000020409 3e  c                   endif
094100020408
094200020408 2e  c                   endif
094300020405
094400020408 1e  c                   enddo
094500020408
094600020408      * stampo ultima tariffa sbagliata
094700020408 1b  c                   if        sav_tpdftc <> *blanks
094800020409 2b  c                   if        wokcts = *on
094900020408      * rottura codice commerciale
095000020408     c                   if        clpage <> preage and wokage = *off
095100020408     c                   eval      preage = clpage
095200020408     c                   exsr      stpage
095300020408     c                   eval      wokage = *on
095400020408     c                   endif
095500020408      * testata
095600020408     c                   if        *inof
095700020408     c                   eval      *in07 = *on
095800020408     c                   except    testa
095900020408     c                   except    testa2
096000020408     c                   eval      *in07 = *off
096100020408     c                   eval      *inof = *off
096200020408     c                   endif
096300020408      * dettaglio
096400020408     c                   if        wokdett = *off
096500020408     c                   except    dett
096600020408     c                   eval      wokdett = *on
096700020408     c                   endif
096800020408     c                   movel     sav_tpdftc    codftc
096900020408     c                   exsr      ricftc
097000020408     c                   except    dett5
097100020408 2e  c                   endif
097200020408     c                   clear                   sav_tpdftc
097300020408     c                   eval      wokcts = *on
097400020408 1e  c                   endif
097500020402
097600020402     c                   endsr
097700020402      *---------------------------------------------------------------*
097800020402      * RICFTC - cerca descrizione tariffa particolare
097900020402      *---------------------------------------------------------------*
098000020402     c     ricftc        begsr
098100020402
098200020402     c                   clear                   desftc           20
098300020402
098400020402     c                   clear                   ds1p
098500020402     c                   clear                   key
098600020402     c                   movel     '1P'          cod
098700020408     c                   movel     codftc        key
098800020402     c     ktab2         chain     tabel00f
098900020402     c                   if        %found(tabel00f)
099000020402     c                   movel     tbluni        ds1p
099100020402     c                   endif
099200020402
099300020402     c                   movel     §1pdes        desftc
099400020402
099500020402      * Per tariffa FedEx controllo se devo recupare la descrizione
099600020402      * dalla tabella SP
099700020402     c                   if        §tafed = 'S' and §1pdfe = 'S'
099800020402     c                   clear                   dssp
099900020402     c                   clear                   key
100000020402     c                   movel     'SP'          cod
100100020408     c                   movel     codftc        key
100200020402     c     ktab2         chain     tabel00f
100300020402     c                   if        %found(tabel00f)
100400020402     c                   movel     tbluni        dssp
100500020402     c                   endif
100600020402     c                   if        §spdfe <> *blanks
100700020402     c                   movel     §spdfe        desftc
100800020402     c                   endif
100900020402
101000020402     c                   endif
101100020402
101200020402     c                   endsr
101300921012     C*
101400950720     C*--- INZSR INIZIALE --------------------------------------------*
101500940719     C     *INZSR        BEGSR
101600950406     C*
101700950406     C* ACCESSO CNACO00F/CNIND00F
101800940719     C     KCLI          KLIST
101900940719     C                   KFLD                    CODUT
102000940719     C                   KFLD                    CCC               4 0
102100950612     C                   KFLD                    WKSC
102200950406     C* ACCESSO CNACO00F
102300940719     C     KACO          KLIST
102400940719     C                   KFLD                    CODUT
102500940719     C                   KFLD                    CCC
102600940719     C                   KFLD                    CLPKSC
102700950406     C* ACCESSO CNCLP01L
102800940719     C     KCLP          KLIST
102900940719     C                   KFLD                    CODUT
103000940719     C                   KFLD                    CCC
103100940719     C                   KFLD                    WAGE
103200020527     C* ACCESSO CNCLP01L
103300020527     C     KCLP1         KLIST
103400020527     C                   KFLD                    CODUT
103500020527     C                   KFLD                    CCC
103600940719     C** ACCESSO CNACO-CNIND
103700940719     C     KTAB2         KLIST
103800940719     C                   KFLD                    CODUT
103900940719     C                   KFLD                    COD
104000940719     C                   KFLD                    KEY
104100991001     C* ACCESSO TITPT01L
104200991001     C     KTPT          KLIST
104300991001     C                   KFLD                    TAMKSC
104400991001     C                   KFLD                    TAMCTR
104500991001     C                   KFLD                    TAMPRG
104600020409
104700020409     c     Kwtpt         klist
104800020409     c                   kfld                    wksc
104900020409     c                   kfld                    wctr
105000020409     c                   kfld                    wprg
105100041007
105200041007     c     kAzorg        klist
105300041007     c                   kfld                    prmcdi
105400041007     c                   kfld                    kcar
105500020409
105600940719     C*
105700940719     C***
105800940719     C* DEFINIZIONE CAMPI
105900940719     C***
106000940719     C     *LIKE         DEFINE    TBLCOD        COD
106100940719     C     *LIKE         DEFINE    TBLKEY        KEY
106200950612     C     *LIKE         DEFINE    TAMCTR        SAVCTR
106300950612     C     *LIKE         DEFINE    TAMCTR        WCTR
106400991019     C     *LIKE         DEFINE    §TADIV        WDIV
106500950612     C     *LIKE         DEFINE    TAMKSC        WKSC
106600950612     C     *LIKE         DEFINE    TAMPRG        WPRG
106700950612     C     *LIKE         DEFINE    TAMDDT        WDDT
106800950612     C     *LIKE         DEFINE    TAMDST        WDST
106900950612     C     *LIKE         DEFINE    TAMDUV        WDUV
107000960723     C     *LIKE         DEFINE    TAMSIS        WSIS
107100960726     C     *LIKE         DEFINE    TAMBAP        WBAP
107101161025     C     *LIKE         DEFINE    TAMFTP        WFTP
107200950612     C     *LIKE         DEFINE    TAMDDT        DATDEC
107300940719     C     *LIKE         DEFINE    TAMDST        DATSCA
107400940719     C     *LIKE         DEFINE    TAMDUV        DATVAR
107500940719     C     *LIKE         DEFINE    CLPAGE        WAGE
107600950406     C     *LIKE         DEFINE    CLPAGE        PREAGE
107700020529     C     *LIKE         DEFINE    CLPAGE        PREAGU
107800020524     C     *LIKE         DEFINE    CLPAGE        CLPAGU
107900020524     C     *LIKE         DEFINE    CLPAGE        ROTAGE
108000020524     C     *LIKE         DEFINE    RAGCOM        DESAGU
108100940719     C*
108200940719     C***
108300940719     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
108400940719     C***
108500950406     C                   TIME                    W0140            14 0
108600950406     C                   MOVE      W0140         UDATE8            8 0
108601161028     c                   move      udate8        anno              4 0
108602161028     c                   move      anno          anno2             2 0
108603161028     c                   movel     udate8        udate6            6 0
108604161028     c                   move      anno2         udate6
108700940719     C                   ENDSR
108800020524      *----------------------------------------------------------------
108900020524      *  Reperisco Dati del job (Utente/Operativi)
109000020524      *----------------------------------------------------------------
109100020524     C     DatiJob       BegSr
109200020524      *
109300020524     C     *dtaara       define    §azute        azuteds
109400020524     C     *dtaara       define    §datiute      ddatiute
109500020524      *
109600020524     C                   in(E)     *dtaara
109700020524     C                   IF        %Error
109800020524     C                   call      'TIBS34R'
109900020524     C                   parm                    Tibs34Ds
110000020524     C                   in        *dtaara
110100020524     C                   ENDIF
110200020524      *
110300020524     C                   endsr
110400940719     C*
110500900406     O*--------------------------------------------------------------*
110600900406     O*     TESTATA                                                  *
110700900406     O*--------------------------------------------------------------*
110800161028     OQSYSPRT   E            TESTA            02
110900161028     O               01N06   TS(1)               66
111000161028     O               02N06   TS(13)              66
111100161028     O               03N06   TS(1)               66
111200161028     O                 N06   TS(2)              132
111201161028     O                  06   TS(21)              66
111204161028     O                  06   TS(22)             132
111300900406     O                       RSUT                20
111400020717     O***                                        21 '/'
111500020717     O***                    SIMFEL              24
111600161028     O                       UDATE6             122 '  /  /  '
111700900406     O                       PAGE          Z    132
111800161028     O          E   N06      TESTA            03
111900921014     O               01      TS(3)               66
112000921014     O                  02   TS(14)              66
112100921014     O                     03TS(15)              66
112200921013     O                       TS(4)              132
112300020402     O                       VIDDAL              90 '  /  /    '
112400020402     O                       VIDAL              105 '  /  /    '
112401161028     O          E    06      TESTA            03
112402161028     O               01      TS(23)              66
112403161028     O               02      TS(27)              66
112404161028     O               03      TS(28)              66
112405161028     O                       TS(24)             132
112406161028     O                       VIDDAL              96 '  /  /    '
112407161028     O                       VIDAL              110 '  /  /    '
112500161028     O          E   N06      TESTA          1 04
112600921014     O               01      TS(5)               66
112700921014     O                  02   TS(13)              66
112800921014     O                     03TS(5)               66
112900921013     O                       TS(6)              132
113000900406     O                       KNMUS              132
113001161028     O          E    06      TESTA          1 04
113002161028     O                       TS(25)              66
113005161028     O                       TS(26)             132
113006161028     O                       KNMUS              132
113100020402     O          E    04      TESTA       1  1
113200020402     O                       TS(16)              66
113300020402     O                       TS(17)             132
113400020409     O          E    05      TESTA       1  1
113500020409     O                       TS(18)              66
113600020409     O                       TS(19)             132
113700950406     O          E            TESTA2      1  1
113800020527     O              N80                          12 'COMMERCIALE:'
113900020527     O              N80      CLPAGE            +  0
114000020527     O              N80                        +  0 '-'
114100020527     O              N80      RAGCOM            +  0
114200020527     O              N80                        +  1 'UNIFICANTE:'
114300020527     O              N80      CLPAGU            +  0
114400020527     O              N80                        +  0 '-'
114500020527     O              N80      DESAGU            +  0
114600020527     O               80                          12 'UNIFICANTE:'
114700020527     O               80      CLPAGU            +  0
114800020527     O               80                        +  0 '-'
114900020527     O               80      DESAGU            +  0
115000020527     O               80                        +  1 'COMMERCIALE :'
115100020527     O               80      CLPAGE            +  0
115200020527     O               80                        +  0 '-'
115300020527     O               80      RAGCOM            +  0
115400161028     O                     07WCFO               112
115500020527     O                     07WCCL               132
115600950406     O          E            TESTA2      1
115700921013     O                       TS(7)               66
115800921013     O                       TS(10)             132
115900950406     O          E            TESTA2      1  1
116000921013     O                       TS(8)               66
116100921013     O                       TS(11)             132
116200950406     O          E            FITT           2
116300950406     O                                          132 ' '
116400900406     O*--------------------------------------------------------------*
116500900406     O*     DETTAGLIO                                                *
116600900406     O*--------------------------------------------------------------*
116700900406     O          E            DETT        1
116800950612     O                       WKSC                 7
116900970407     O                       WBLOC               10
117000991019     O                       WRAG                49
117100991019     O                       WCIT                75
117200991019     O                       WCTR                79
117300991019     O                       WPRG          Z     83
117400991019     O                       WDIV                87
117500970407     O                       DATDEC              98 '  /  /    '
117600970407     O                       DATSCA             109 '  /  /    '
117700970407     O                       DATVAR             120 '  /  /    '
117800970407     O                       WSIS               124
117801161025     O                       WFTP               130
117900020402     o          e            dett1       1
118000020402     o                       dts(1)              66
118100020402     o                       dts(2)             132
118200020402     o          e            dett2       1
118300020402     o                       dts(3)              66
118400020402     o                       dts(4)             132
118500020402     o          e            dett3       1
118600020402     o                       dts(5)              66
118700020402     o                       codftc              36
118800020402     o                       desftc              59
118900020402     o                       dts(6)             132
119000020402     o          e            dett4       1
119100020402     o                       dts(7)              66
119200020402     o                       codftc              36
119300020402     o                       desftc              59
119400020402     o                       dts(8)             132
119500020405     o          e            dett5       1
119600020405     o                       dts(9)              66
119700020409     o                       codftc              36
119800020409     o                       desftc              59
119900020405     o                       dts(10)            132
120000020409     o                       wcts                87
120100921014**
120200161028XXXXXXXXXXXXXXXXXXXX     ***--------------------------------------   1
120300161028-----------------------------------------***    XX.XX.XX Pag. XXXX   2
120400020402                         **      ELENCO TARIFFE IN SCADENZA PER CO   3
120500020402MMERCIALE DAL XX/XX/XXXX  AL XX/XX/XXXX   **    Pgm...: TNTA18R      4
120600020402                         ***--------------------------------------   5
120700020402-----------------------------------------***    Utente:              6
120800970407----------------------   C    L    I    E    N    T    E   -------   7
120900991019CODICE BLOC RAGIONE SOCIALE                       LOCALITA'          8
121000991019XXXXXXX BL XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXX   9
121100161025---------   TARIFFA      DATA      DATA       DATA     COPIA  TAR.  10
121200161025          COD PRG DIV DECORRENZA SCADENZA   ULT.VARIAZ FIRM. PREF.
121300991019XXXXXXXXX XXX XXX XXX XX/XX/XXXX XX/XX/XXXX XX/XX/XXXX   X     X    12
121400020402                         ***--------------------------------------  13
121500020402                         **          ELENCO TARIFFE VARIATE PER CO  14
121600020402                         **   ELENCO TARIFFE CON DECORRENZA PER CO  15
121700020402                         Elenco tariffe che presentano codici tass  16
121800020402azione IT o EE incongruenti con la tariffa                          17
121900020409                            Elenco tariffe particolari prive del c  18
122000020409odice tassazione generico IT o EE                                   19
122001161028                                                                    20
122002161028XXXXXXXXXXXXXXXXXXXX***-------------------------------------------  21
122003161028---------------------------------------------*  XX.XX.XX Pag. XXXX  22
122004161028                    *    ELENCO TARIFFE PREFERENZIALI IN SCADENZA   23
122005161028PER COMMERCIALE DAL XX/XX/XXXX AL XX/XX/XXXX *  Pgm...: TNTA18R     24
122006161028                    ***-------------------------------------------  25
122007161028-------------------------------------------***  Utente:             26
122008161028                    *        ELENCO TARIFFE PREFERENZIALI VARIATE   27
122009161028                    * ELENCO TARIFFE PREFERENZIALI CON DECORRENZA   28
122100020402**      dts
122200020402   Presente in dett.tariffario cod.tassazione EE per tariffa itali   1
122300020402a o DPD: correggere con IT                                           2
122400020402   Presente in dett.tariffario cod.tassazione IT per tariffa impor   3
122500020402t/export: correggere con EE                                          4
122600020402   Presente in tariffa particolare x - xxxxxxxxxxxxxxxxxxxx cod.ta   5
122700020402ssazione EE per tariffa italia o DPD: correggere con IT              6
122800020402   Presente in tariffa particolare x - xxxxxxxxxxxxxxxxxxxx cod.ta   7
122900020402ssazione IT per tariffa import/export: correggere con EE             8
123000020409   Tariffa particolare             x - xxxxxxxxxxxxxxxxxxxx priva    9
123100020409del cod.tassazione xx                                               10
