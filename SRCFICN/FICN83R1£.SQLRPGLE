000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940128     F*----------------------------------------------------*
000500091223     Ffitgt01l  if   E           k disk    infds(dsfitgt)
000600090311      *
000700091223     FFICN83D1  CF   E             WORKSTN
000800940607     F                                     SFILE(S1:S1NRR)
000900940201     F                                     INFDS(DSFMT)
001000091223     D*--------------------
001100091223     D dsfitgt         DS
001200091223     D  rrnfitgt             397    400b 0
001300091223     D*--------------------
001400940211     D DSFMT           DS           512
001500940506     D  $TASTO               369    369
001600940211     D  NRG                  370    370
001700940211     D  NCL                  371    371
001800940211     D  SFLNRR               378    379B 0
001900940207     D*-------------
002000940127     D* Reperimento nome PGM
002100940127     D STATUS         SDS           333
002200090312     D* Nome PGM a video
002300940127     D  DSPGM            *PROC
002400090311     D*-------------
002500090316     D posizionamento  S              1    inz('N')
002600090316     D Wlen            S              3i 0
002700090316     D Wpos            S              3i 0
002800090311      *
002900090311     D dataiso         s               d   datfmt(*iso)
003000090311     D dataeur         s               d   datfmt(*eur)
003100090311     D dataDMY         s               d   datfmt(*dmy)
003200090311      *
003300030113     D*-------------
003400030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
003500100108$003 D rigasfl         S                   Like(C1rcd)
003600030113$003 D S1NRR           S                   Like(C1rcd)
003700030113$003 D WSfl            S                   Like(C1nrr)
003800030113$003 D Wmax            S                   Like(C1rcd)
003900030113$003 D Wpag            S                   Like(C1rcd)
004000030113$003 D Winzs1          S                   Like($inzs1)
004100940207     D*-------------
004200940211     D* COSTANTI
004300940211     D*-------------
004400090326     D SFLPAG          C                   CONST(11)
004500940314     D* dimensione della schiera $MS1
004600940506     D*
004700940506     D* Tasti di funzione
004800940506     D F01             C                   CONST(X'31')
004900940506     D F02             C                   CONST(X'32')
005000940506     D F03             C                   CONST(X'33')
005100940506     D F04             C                   CONST(X'34')
005200940506     D F05             C                   CONST(X'35')
005300940506     D F06             C                   CONST(X'36')
005400940506     D F07             C                   CONST(X'37')
005500940506     D F08             C                   CONST(X'38')
005600940506     D F09             C                   CONST(X'39')
005700940506     D F10             C                   CONST(X'3A')
005800940506     D F11             C                   CONST(X'3B')
005900940506     D F12             C                   CONST(X'3C')
006000940506     D F13             C                   CONST(X'B1')
006100940506     D F14             C                   CONST(X'B2')
006200940506     D F15             C                   CONST(X'B3')
006300940506     D F16             C                   CONST(X'B4')
006400940506     D F17             C                   CONST(X'B5')
006500940506     D F18             C                   CONST(X'B6')
006600940506     D F19             C                   CONST(X'B7')
006700940506     D F20             C                   CONST(X'B8')
006800940506     D F21             C                   CONST(X'B9')
006900940506     D F22             C                   CONST(X'BA')
007000940506     D F23             C                   CONST(X'BB')
007100940506     D F24             C                   CONST(X'BC')
007200940506     D ENTER           C                   CONST(X'F1')
007300940506     D ROLDWN          C                   CONST(X'F4')
007400940506     D ROLLUP          C                   CONST(X'F5')
007500090311      **********************************************************************
007600090316      *?     MAIN LINE PROGRAM     ?
007700091223      **********************************************************************
007800940223     C* inizializzazione variabili
007900940223     C                   EXSR      INZVAR
008000940223     C*
008100940223     C     $FINE         DOWEQ     *OFF
008200940131     C     $GEST         CASEQ     'S1'          GESS1
008300940117     C                   END
008400940117     C                   END
008500940325     C* fine programma
008600940325     C                   SETON                                            LR
008700030113     C************************************************************
008800030113     C* INIZIALIZZAZIONE VARIABILI
008900030113     C************************************************************
009000030113     C     INZVAR        BEGSR
009100030113     C*
009200030113     C* Pulizia campi e indicatori
009300030113     C                   MOVE      *ALL'0'       IN4049           10
009400030113     C                   MOVEA     IN4049        *IN(40)
009500030113     C                   CLEAR                   S1OPZ
009600030113     C* Variabili per gestione videate
009700030113     C*
009800030113     C                   MOVE      *OFF          $FINE
009900030113     C                   MOVE      *OFF          $INZS1
010000030113     C                   MOVE      *OFF          $EFILE
010100030113     C                   MOVE      *OFF          $ESCI
010200030113     C                   MOVE      *OFF          $RCDOK
010300030113     C                   Z-ADD     0             $ULKS1            3 0
010400030113     C*
010500030113     C                   MOVE      *ON           $INZS1
010600030113     C                   MOVE      'S1'          $GEST
010700030113     C*
010800030113     C* Variabili appoggio
010900030114     C                   Z-ADD     1             WPAG
011000030113     c*
011100030113     C                   ENDSR
011200940127     C************************************************************
011300940131     C* GESTIONE LISTA
011400940127     C************************************************************
011500940127     C     GESS1         BEGSR
011600030113     C*
011700940223     C* inizializzazione videata
011800940223     C     $INZS1        IFEQ      *ON
011900940127     C                   EXSR      INZS1
012000940223     C                   MOVE      *OFF          $INZS1
012100940127     C                   ENDIF
012200030113     C*
012300030113     C* emissione piede videata
012400030113     C                   WRITE     Z1
012500030113     C* Non ci sono records
012600940223     C     WMAX          IFEQ      0
012700940607     C                   WRITE     D1
012800090316     c                   setoff                                       30
012900030114     C                   Else
013000090316     c                   seton                                        30
013100090316      *
013200090316      * se si è effettuato un Posizionamento
013300030114     C     Wsfl          IFgt      0
013400030114     C                   Z-ADD     wsfl          C1RCD
013500030114     C                   Else
013600030114     C     Wpag          IFgt      0
013700030114     C                   Z-ADD     wpag          C1RCD
013800030114     C                   EndIF
013900090316     C                   EndIF
014000030114     C                   ENDIF
014100090316      *
014200030113     C*              *------------------*
014300940607     C                   EXFMT     C1
014400030113     C*              *------------------*
014500030113     C*
014600940204     C     C1NRR         IFNE      0
014700940204     C                   Z-ADD     C1NRR         WSFL
014800090313     C                   Else
014900090313     C                   Z-ADD     SFLNRR        wsfl
015000090313     C                   ENDIF
015100090313      *
015200940127     C                   Z-ADD     SFLNRR        C1RCD
015300030113     C* Selezioni
0154009401271    C                   SELECT
015500090311     C* F12=Ritorno
015600090311     C     $TASTO        WHENEQ    F12
015700090311     C                   EXSR      F12S1
0158009401271O   C                   OTHER
015900940127     C* CONTROLLO DATI
016000940131     C                   EXSR      CTRC1
016100940201     C     *IN99         IFEQ      *OFF
016200940131     C                   EXSR      CTRS1
016300940131     C                   END
0164009401271-   C                   ENDSL
016500940127     C*
016600940127     C                   ENDSR
016700940224     C/EJECT
016800940127     C************************************************************
016900940131     C* INIZIALIZZAZIONE LISTA
017000940127     C************************************************************
017100090312     C     INZC1         BEGSR
017200940302     C* pulizia SFL
017300090312     C                   SETOFF                                         3031
017400090312     C                   WRITE     C1
017500090312     C                   SETON                                          31
017600090312     C*
017700090312     C* CARICAMENTO SFL totale
017800090312     C                   Z-ADD     1             C1RCD
017900090312     c                   movel     DSPGM         c1pgm
018000090312     C*
018100090312     C                   ENDSR
018200090312     C/EJECT
018300090312     C************************************************************
018400090312     C* INIZIALIZZAZIONE LISTA
018500090312     C************************************************************
018600090312     C     INZS1         BEGSR
018700090312     C*
018800090312     C* pulizia SFL
018900090312    >C                   EXSR      INZC1
019000090312     C*
019100090312     C                   Z-ADD     0             S1NRR
019200090312     C                   Z-ADD     0             WMAX
019300090312     C                   Z-ADD     0             wsfl
019400940224     C*
019500940224     C* Posizionamento su file pilota
019600091223     c     KPDRSML       setll     fitgt01l
019700940608    >C                   EXSR      REDANA
019800030113     C* Carico il SFL
019900940127     C                   EXSR      ROLS1
020000030113     C*
020100030114     C                   Z-ADD     1             WPAG
020200940127     C*
020300940127     C                   ENDSR
020400091223     C************************************************************
020500091223     C* LETTURA RCD ARCHIVIO PILOTA
020600091223     C************************************************************
020700091223     C     REDANA        BEGSR
020800091223     C*
020900091223     C                   MOVEL     *OFF          $EFILE
021000091223     C*
0211000912231    C     $EFILE        DOUEQ     *ON
021200091223     C                   MOVEL     *ON           $RCDOK
021300091223     c     KPDRSML       Reade     fiTGT01L
021400091223     C*
021500091223     c                   if        %eof(fiTGT01L)
021600091223     C                   MOVEL     *ON           $EFILE
021700091223     C                   MOVEL     *OFF          $RCDOK
021800091223     C                   MOVE      $EFILE        *IN33
021900091223     c                   else
022000091223     C*
022100091223      *  Esclude gli annullati
022200091223     c                   if         tgtATB <>' '
022300091223     C                   MOVE      *OFF          $RCDOK
022400091223     c                   end
022500091223     C*
022600091223     C* se il record è giusto esce dal ciclo
022700091223     C     $RCDOK        ifeq      *On
022800091223     c                   Leave
022900091223     c                   end
023000091223     C*
023100091223     c                   end
023200091223     C*
0233000912231-   C                   ENDDO
023400091223     C*
023500091223     C                   ENDSR
023600940127     C************************************************************
023700940131     C* CARICAMENTO PAGINA LISTA
023800940127     C************************************************************
023900940127     C     ROLS1         BEGSR
024000940127     C*
024100940128     C                   SETOFF                                       32
024200940223     C                   Z-ADD     0             Y
024300940127     C                   Z-ADD     WMAX          S1NRR
024400090316     c                   clear                   Wpos
024500940127     C*
024600940127     C* Leggo dal file anagrafico per caricare la lista
0247009401311    C     $EFILE        DOWEQ     *OFF
024800940127     C*
024900030113     c                   clear                   s1opz
025000090311     C*
025100091223     c                   eval      s1DES   = tgtDCV
025200091223     c                   eval      s1PRG   = tgtPRG
025300090311     c*
025400091223     c                   z-add     0             s1da
025500100303     c                   z-add     TGTDDT        h1da
025600100303     c                   move      TGTDDT        dataiso
025700090311     c                   move      dataiso       dataDMY
025800100303     c                   move      dataiso       dataEUR
025900100303     c                   move      dataEUR       s1Da8
026000091223     c                   move      dataDMY       S1Da
026100090311     c*
026200091223     c                   z-add     0             s1A
026300100303     c                   z-add     TGTDST        h1A
026400100303     c                   move      TGTDST        dataiso
026500090311     c                   move      dataiso       dataDMY
026600100303     c                   move      dataiso       dataEUR
026700100303     c                   move      dataEUR       s1a8
026800091223     c                   move      dataDMY       S1A
026900940127     C*
027000940127     C                   ADD       1             S1NRR
027100940127     C                   ADD       1             Y
027200100224     c*********          seton                                        55
027300100224     c*********          move      '1'           Hin55
027400940127     C*
027500940607     C                   WRITE     S1
027600090313     C*
027700090313     C* esce x il max. records consentiti
027800090313     c                   if        s1nrr = 9999
027900090313     c                   leave
028000090313     c                   end
028100940131     C*
028200940316    >C                   EXSR      REDANA
0283009401271-   C                   ENDDO
028400940127     C*
028500100224     C*
028600100108     C* L'ultimo record deve essere quello cancellabile
028700100108     c                   if        s1nrr > 0
028800100108     C*
028900100108     C*   deve aggiornare il sfl per aprire il campo di selezione
029000100108     c                   z-add     s1nrr         rigasfl
029100100108     c                   if        s1nrr > 1
029200100108     c                   sub       1             rigasfl
029300100108     c                   end
029400100108     C     s1nrr         chain     S1
029500100108     c                   setoff                                       55
029600100108     c                   move      '0'           Hin55
029700100224      *IMPOSTA IL FLAG DI CANCELLABILITA'
029800100224     c                   move      '4'           HULTIMA
029900100108     c                   update    s1
030000100108     C*
030100100108     c                   end
030200100108     C*
030300100108     C*
030400940223     C                   Z-ADD     S1NRR         WMAX                 30
030500090316     C*
030600940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
030700030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
030800940127     C                   MVR                     RESTO             3 0
030900030114     C     PAGINE        MULT      SFLPAG        C1RCD
0310000301141    C     RESTO         IFGT      0
031100030114     C                   ADD       1             C1RCD
0312000301141E   C                   ELSE
031300030114     C                   SUB       SFLPAG        C1RCD
031400030114     C                   ADD       1             C1RCD
0315000301141-   C                   ENDIF
031600940128     C*
031700940127     C                   ENDSR
031800940224     C************************************************************
031900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
032000940224     C************************************************************
032100940224     C     CLCPAG        BEGSR
032200940224     C* Input :
032300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
032400940224     C*          cursore
032500940224     C* - SFLPAG = numero rcd per pagina sfl
032600940224     C* Output :
032700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
032800940224     C*
032900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
033000940224     C                   MVR                     RESTO             3 0
033100940224     C     RESTO         IFGT      0
033200940224     C                   ADD       1             PAGINE
033300940224     C                   ENDIF
033400940224     C     PAGINE        MULT      SFLPAG        WPAG
033500940224     C*
033600940224     C                   ENDSR
033700090311     C************************************************************
033800090311     C* GESTIONE F12 VIDEO S1
033900090311     C************************************************************
034000090311     C     F12S1         BEGSR
034100090311     C*
034200090311     C                   MOVE      *ON           $FINE
034300090311     C* fine programma
034400090311     C                   ENDSR
034500090311     C/EJECT
034600940128     C************************************************************
034700940131     C* CONTROLLO TESTATA LISTA
034800940128     C************************************************************
034900940131     C     CTRC1         BEGSR
035000940128     C*
035100940201     C                   MOVE      *OFF          *IN99
035200090316     C*
035300940202     C                   ENDSR
035400090316     C************************************************************
035500090316     C* CONTROLLO OPZIONI LISTA
035600090316     C************************************************************
035700090316     C     CTRS1         BEGSR
035800090316     C*
035900940202     C                   MOVEL     *OFF          $ESCI
036000940201     C                   SETOFF                                       99
036100090311     C                   clear                   S1OPZ
036200940131     C*
036300940127     C* Leggo il sfl solo se ci sono rcd
0364009401311    C     WMAX          IFGT      0
036500940607     C                   READC     S1                                     21
036600940127     C*
036700940131     C* esce se fine sfl o errore che richiede l'uscita
0368009401312    C     *IN21         DOWEQ     *OFF
036900940131     C     $ESCI         ANDEQ     *OFF
037000940201     C                   Z-ADD     S1NRR         C1RCD
037100940131     C* ctrl su riga
037200940131     C                   EXSR      RECS1
037300090311      *
037400940131     C* gestione opzioni
0375000903113    C     S1OPZ         IFNE      *blank
037600940201     C     *IN99         ANDEQ     *OFF
037700940131     C                   EXSR      OPZS1
0378009401273-   C                   ENDIF
037900090311      *
038000940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0381009402013    C     *IN99         IFEQ      *ON
038200940201     C     $ESCI         OREQ      *ON
038300940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038400940204     C* la ripristinerò a conclusione del ciclo di READC
038500940223     C                   MOVE      *OFF          $INZS1
0386009402233-   C                   ENDIF
038700940223     C*
038800100107     C                   MOVE      *ON           *IN32
038900100107     C   kf              clear                   S1OPZ
039000940607     C                   UPDATE    S1
039100940223     C*
039200940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0393009401313    C     $ESCI         IFEQ      *OFF
039400940607     C                   READC     S1                                     21
039500940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0396009402014    C     *IN21         IFEQ      *ON
0397000912234    C     $tasto        andne     F06
039800940201     C                   MOVE      WINZS1        $INZS1
039900940204     C* calcolo pagina a cui deve posizionarsi
040000940224     C                   EXSR      CLCPAG
0401009402014-   C                   ENDIF
0402009402013-   C                   ENDIF
040300940131     C*
0404009401272-   C                   ENDDO
040500940127     C*
0406009401311-   C                   ENDIF
040700940131     C*
040800940127     C                   ENDSR
040900940127     C/EJECT
041000940127     C************************************************************
041100940131     C* CONTROLLO CAMPI I/O RIGA LISTA
041200940127     C************************************************************
041300940131     C     RECS1         BEGSR
041400940131     C*
041500940201     C* reset indicatori DSPATR
041600940201     C                   MOVE      *ALL'0'       IN4049           10
041700940201     C                   MOVEA     IN4049        *IN(40)
041800100224     C*
041900100224     C                   if        s1opz = '4' AND Hultima =' '
042000100224     c                   seton                                        99  40
042100100224     C                   END
042200100224     C*
042300090415     C*
042400940131     C                   ENDSR
042500940131     C************************************************************
042600940131     C* GESTIONE OPZIONI LISTA
042700940131     C************************************************************
042800940131     C     OPZS1         BEGSR
042900940201     C*
043000100224     C                   if        s1opz = '1'
043100100224     C                   MOVEL     *ON           $ESCI
043200100303     C                   MOVEL     *ON           $FINE
043300100224     C                   move      s1prg         Prg
043400100303     C                   move      s1da8         daDat
043500100303     C                   move      s1a8          aDat
0436001002242-   C                   ENDIF
043700940225     C*
043800091223     C*  cancella i records
043900091223     C     $TASTO        ifEQ      F06
044000100224     C                   if        s1opz = '4' AND Hultima ='4'
0441000912232-   C                   exsr      Elimina
0442000912232-   C                   ENDIF
044300091223     c                   end
044400091223     C*
044500940131     C                   ENDSR
044600940223     C/EJECT
044700940131     C************************************************************
044800940131     C* OPERAZIONI INIZIALI
044900940131     C************************************************************
045000940131     C     *INZSR        BEGSR
045100030113     C*
045200030113     C* Reperimento parametri
045300030113     C     *ENTRY        PLIST
045400091223     C                   PARM                    Aut               7
045500100224     C                   PARM                    Prg               3
045600100224     C                   PARM                    daDat             8
045700100224     C                   PARM                    aDat              8
045800091223     C                   move      aut           Autista           7 0
045900091223      *
046000091223      * Variabili per gestione videate
046100030113     C                   MOVE      *BLANK        $GEST             2
046200030113     C                   MOVE      *BLANK        $FINE             1
046300030113     C                   MOVE      *BLANK        $INZS1            1
046400030113     C                   MOVE      *BLANK        $EFILE            1
046500030113     C                   MOVE      *BLANK        $ESCI             1
046600030113     C                   MOVE      *BLANK        $RCDOK            1
046700091223      * Indici
046800030113     C                   Z-ADD     0             X                 3 0
046900030113     C                   Z-ADD     0             Y                 3 0
047000091223      *
047100091223      * Reperimento tasti di funzione
047200091223      *
047300091223     c     KPDRSML       klist
047400091223     c                   kfld                    autista
047500091223     c                   kfld                    simula
047600091223     c                   move      'S'           simula            1
047700091223      *
047800940117     C                   ENDSR
047900090311      * ?-------------------------------------------------------------*?
048000091223      *  ELIMINA il progressivo della Tariffa di simulazione
048100091223      * ?-------------------------------------------------------------*?
048200091223     C     Elimina       BEGSR
048300091223      *
048400091223     C/EXEC SQL
048500091223     C+ DELETE FROM FIFGT00F WHERE FGTSML = 'S' and FGTPRG= :s1prg
048600091223     C+    and FGTPDR = :Autista
048700091223     C/END-EXEC
048800091223      *
048900091223     C/EXEC SQL
049000091223     C+ DELETE FROM FIFPT00F WHERE FPTSML = 'S' and FPTPRG= :s1prg
049100091223     C+    and FPTPDR = :Autista
049200091223     C/END-EXEC
049300091223      *
049400091223     C/EXEC SQL
049500091223     C+ DELETE FROM FIFPD00F WHERE FPDSML = 'S' and FPDPRG= :s1prg
049600091223     C+    and FPDPDR = :Autista
049700091223     C/END-EXEC
049800091223      *
049900091223     C/EXEC SQL
050000091223     C+ DELETE FROM FITGT00F WHERE TGTSML = 'S' and TGTPRG= :s1prg
050100091223     C+    and TGTPDR = :Autista
050200091223     C/END-EXEC
050300091223      *
050400091223     C                   exsr      inzVAR
050500091223      *
050600091223     C                   ENDSR
050700091223      * ?-------------------------------------------------------------*?
