000100020208     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200020208      *--------------------------------------------------------------------------------------------*
000300020208      *     MANUTENZIONE TABELLA 3NN PER AUTORIZZAZIONI VCONFERMA BOLLE                            *
000400020208      *--------------------------------------------------------------------------------------------*
000500020208      * Data base                                                                                  *
000600020208      *--------------------------------------------------------------------------------------------*
000700020211     Ftntbe02l  UF   E           K DISK
000800020208     FAzOrg01l  IF   E           K DISK
000900020212     FTabel00f  IF   E           K DISK
001000020211     FTntbe01l  IF A E           K Disk    rename(Tntbe000:tntbe01)
001100020208     FTnTb23d   CF   E             WORKSTN
001200020208      *--------------------------------------------------------------------------------------------*
001300020208      * Data structure                                                                             *
001400020208      *--------------------------------------------------------------------------------------------*
001500020208      *---
001600020208      * Schiere memorizzazione dati correnti
001700020208      *---
001800020211     D Pou             S             03    DIM(19)
001900020208     D Opz             S             15    DIM(4) CTDATA PERRCD(1)              *Messaggi video
002000020320     D msg             S             78    DIM(6) CTDATA PERRCD(1)              *Messaggi video
002100020208      *---
002200020208      * Variabili
002300020208      *---
002400020208     D $Fine           S              1    INZ('N')                             *fine programma
002500020208     D TipVid          S              1    INZ('0')                             *video in gestione
002600020208     D $CarVd1         S              1    INZ('0')                             *video in gestione
002700020208     D $CarVd2         S              1    INZ('0')                             *video in gestione
002800020208      *---
002900020208      * Indici
003000020208      *---
003100020208     D i               S              5  0 INZ                                  *indice
003200020208     D j               S              5  0 INZ                                  *indice
003300020208     D k               S              5  0 INZ                                  *indice
003400020208     D g               S              5  0 INZ                                  *indice
003500020208     D v               S              5  0 INZ                                  *indice
003600020208     D m               S              5  0 INZ                                  *indice
003700020208      *---
003800020208      * Ds
003900020208      *---
004000020208     D Tntb22Ds      E DS                                                       *ds di procedura
004100020208     D Tibs69Ds      E DS                                                       *ds di procedura
004200020208     D Og143         E DS                                                       *ds di procedura
004300020212     D Ds3C          E DS                                                       *ds di procedura
004400020320     D Ds1B          E DS                                                       *ds di procedura
004500020208     D DsAco         E DS                  ExtName(CnAco00f)                    *ds di procedura
004600020208     D DsInd         E DS                  ExtName(CnInd00f)                    *ds di procedura
004700020208     D DsClp         E DS                  ExtName(CnClp00f)                    *ds di procedura
004800020208     D DsCls         E DS                  ExtName(FnCls00f)                    *ds di procedura
004900020208      *
005000020208     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005100020208     D  TCU                  398    697
005200020208     D                                     DIM(50)
005300020208     D  KCU                  698    847P 0
005400020208     D                                     DIM(50)
005500020208      *
005600020208     D TCUDS           DS
005700020208     D  F1                     1      1
005800020208     D  F3                     3      3
005900020208     D  F2                     2      2
006000020208     D  F4                     4      4
006100020208     D  F56                    5      6
006200020208      *
006300020208      *---
006400020208      * architettura
006500020208      *---
006600020208     D kpjba         E DS                                                       *architettura
006700020208      *--------------------------------------------------------------------------------------------*
006800020208      * Main lines
006900020208      *--------------------------------------------------------------------------------------------*
007000020208      *
007100020208      * Operazioni iniziali
007200020208      *
007300020208     C                   EXSR      RutInz
007400020208      *
007500020208      * Gestione video
007600020208      *
007700020208     C     $Fine         DOUEQ     'S'
007800020208     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile
007900020208     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile
008000020208     C                   ENDCS
008100020208     C                   ENDDO
008200020208      *
008300020208     C                   SETON                                        LR
008400020208      *--------------------------------------------------------------------------------------------*
008500020208      * GesVd1 - Gestione videata immissione nuova tabella -
008600020208      *--------------------------------------------------------------------------------------------*
008700020208     C     GesVd1        BEGSR
008800020208      *
008900020208      * Inizializzazione videata
009000020208     C     $CarVd1       ifeq      *on
009100020208     C                   EXSR      CarVd1
009200020208     C                   move      *off          $CarVd1
009300020208     C                   endif
009400020208      *
009500020208     C                   EXFMT     Tb23v1
009600020208      *
009700020208      *
009800020208     C                   select
009900020208      * F3=Fine
010000020208     C     *inKc         wheneq    *on
010100020208     C                   EXSR      F03Ges
010200020208      *
010300020208     C     *inKl         wheneq    *on
010400020208     C                   EXSR      F12Ges
010500020208     C                   other
010600020208      *
010700020208      * Controlli e decodifiche
010800020208     C                   EXSR      CtrV1
010900020208      *
011000020208     C     *IN28         ifeq      *off
011100020208     C     *inKf         andeq     *on
011200020208     C                   EXSR      GesAgg
011300020208     C                   endif
011400020208     C                   endsl
011500020208      *
011600020208     C                   endsr
011700020208      *--------------------------------------------------------------------------------------------*
011800020208      * CarVd1 - Carica primo video
011900020208      *--------------------------------------------------------------------------------------------*
012000020208     C     CarVd1        BEGSR
012100020208      *
012200020208     C                   clear                   Tb23v1
012300020208     C                   movel     Ragut         V1CRag           20
012400020208     C                   movel     Knmus         V1CUte           10
012500020208     C                   movel     KnSif         V1CSif           10
012600020214      * imposto il campo solo se valorizzato e in questo caso
012700020214      * proteggo il campo
012800020214     C     b22ke1        ifne      *blanks
012900020214     C                   seton                                        38
013000020214     C                   movel     b22ke1        $Cod
013100020214     C                   move      $Cod          V1CCod
013200020214     C                   endif
013300020212      * decodifica
013400020212     C                   move      *blanks       V1DCod
013500020214     C     v1ccod        ifne      *zeros
013600020212     C                   move      V1CCod        $Cod              7 0
013700020212     C                   EXSR      DeCod
013800020212      *
013900020212 2a  C                   if        o69err = *blanks and acoflg =' '
014000020212     C                   movel     acorag        V1DCod
014100020212 2e  C                   endif
014200020214 2e  C                   endif
014300020208      *
014400020208     C                   ENDSR
014500020208      *--------------------------------------------------------------------------------------------*
014600020208      * Controllo e decodifica prima videata
014700020208      *--------------------------------------------------------------------------------------------*
014800020208     C     CtrV1         BEGSR
014900020208      *
015000020214     C                   setoff                                       283940
015100020212     C                   setoff                                       414243
015200020208     C                   setoff                                       515253
015300020208     C                   setoff                                       545556
015400020208     C                   setoff                                       575859
015500020208     C                   setoff                                       606162
015600020208     C                   setoff                                       636465
015700020208     C                   setoff                                       666768
015800020215     C                   setoff                                       6944
015900020208     C                   clear                   V1CMsg
016000020208      *
016100020214      * Ricerca estesa cu codice cliente se non protetto
016200020214 1a  C     v1dcod        ifne      *blanks
016300020214 1a  C     v1ccod        andeq     *zeros
016400020214     C     *in38         andne     *on
016500020208      *
016600020208      * Ricerca estesa
016700020208     C                   movel     ragut         paxdut           30
016800020214     C                   move      v1ccod        $campo7           7
016900020214     C                   movel(P)  $campo7       paxdmt           48
017000020208      *
017100020208     C                   z-add     0             paxsta
017200020208     C                   move      $Kci          paxccc            4 0
017300020208     C                   clear                   paxdit
017400020208     C                   z-add     1             paxnum
017500020208      *
017600020208     C                   CALL      'XALFA3BR'
017700020208     C                   PARM                    PAXDUT
017800020208     C                   PARM                    CODUT
017900020208     C                   PARM                    PAXDMT
018000020208     C                   PARM                    PAXCCC
018100020208     C                   PARM                    PAXSTA            1 0
018200020208     C                   PARM                    PAXFLR           90
018300020208     C                   PARM                    PAXDIT            3
018400020208     C                   PARM                    PAXNUM            2 0
018500020208     C                   PARM                    PAXKCM           80
018600020208     C                   PARM                    PAXKSM          140
018700020208     C                   PARM                    PAXKDM           60
018800020208      *
018900020208 2a  C     PaxSta        ifgt      -1
019000020214     C                   movel     paxksm        $campo7
019100020214     C                   move      $campo7       v1ccod
019200020214     C                   movel     paxdmt        v1dcod
019300020208 2-  C                   endif
019400020208      *
019500020208     C                   goto      Finesr
019600020214 1-  C                   endif
019700020208      *
019800020212      * Controllo validità codice
019900020214     C                   move      *blanks       v1dcod
020000020214 1a  C     v1ccod        ifne      *zeros
020100020208      *
020200020214     C                   move      v1ccod        $Cod              7 0
020300020208     C                   EXSR      DeCod
020400020208      *
020500020208 2a  C                   if        o69err = *blanks and acoflg =' '
020600020214     C                   movel     acorag        v1dcod
020700020212      *
020800020208 2e  C                   else
020900020214     C                   seton                                        2839
021000020212     C                   goto      Finesr
021100020208 2-  C                   endif
021200020208      *
021300020208 1-  C                   endif
021400020214      *
021500020214      * Nuovo codice cliente con cui confermare
021600020214 1a  C     v1dcli        ifne      *blanks
021700020214 1a  C     v1ccli        andeq     *zeros
021800020214      *
021900020214      * Ricerca estesa
022000020214     C
022100020214     C                   movel     ragut         paxdut           30
022200020214     C                   movel(P)  v1dcli        paxdmt           48
022300020214      *
022400020214     C                   z-add     0             paxsta
022500020214     C                   move      $Kci          paxccc            4 0
022600020214     C                   clear                   paxdit
022700020214     C                   z-add     1             paxnum
022800020214      *
022900020214     C                   CALL      'XALFA3BR'
023000020214     C                   PARM                    PAXDUT
023100020214     C                   PARM                    CODUT
023200020214     C                   PARM                    PAXDMT
023300020214     C                   PARM                    PAXCCC
023400020214     C                   PARM                    PAXSTA            1 0
023500020214     C                   PARM                    PAXFLR           90
023600020214     C                   PARM                    PAXDIT            3
023700020214     C                   PARM                    PAXNUM            2 0
023800020214     C                   PARM                    PAXKCM           80
023900020214     C                   PARM                    PAXKSM          140
024000020214     C                   PARM                    PAXKDM           60
024100020214      *
024200020214 2a  C     PaxSta        ifgt      -1
024300020214     C                   movel     paxksm        $campo7
024400020214     C                   move      $campo7       v1ccli
024500020214     C                   movel     paxdmt        v1dcli
024600020214 2-  C                   endif
024700020214      *
024800020214     C                   goto      Finesr
024900020214 1-  C                   endif
025000020214      *
025100020214      * Controllo validità codice
025200020214     C                   move      *blanks       v1dcli
025300020214 1a  C     v1ccli        ifne      *zeros
025400020214      *
025500020214     C                   move      v1ccli        $Cod              7 0
025600020214     C                   EXSR      DeCod
025700020214      *
025800020214 2a  C                   if        o69err = *blanks and acoflg =' '
025900020214     C                   movel     acorag        v1dcli
026000020214      *
026100020214      * Controllo che i due codici cliente sino diversi
026200020214 3a  C     v1ccli        ifeq      v1ccod
026300020214     C                   seton                                        2843
026400020214     C                   goto      Finesr
026500020214 3-  C                   endif
026600020214      *
026700020214      * Controllo che i due codici cliente siano entrambi
026800020214      * poste/non poste
026900020215     C     v1ccod        div       10000         $Pou1
027000020215     C     v1ccli        div       10000         $Pou2
027100020214     C                   exsr      CtrPos
027200020214 3a  C   24              do
027300020215     C                   seton                                        2841
027400020214     C                   goto      FineSr
027500020214 3-  C                   enddo
027600020214      *
027700020214 2e  C                   else
027800020214     C                   seton                                        2840
027900020214     C                   goto      Finesr
028000020214 2-  C                   endif
028100020214      *
028200020214 1-  C                   endif
028300020214      *
028400020214      * Controllo che i due codici siano poste/non poste
028500020214     C     v1ccli        ifne      *zeros
028600020214     C     v1ccod        div       10000         $Pou1
028700020214     C     v1ccli        div       10000         $Pou2
028800020214     C                   exsr      CtrPos
028900020214     C   24              do
029000020214     C                   seton                                        2841
029100020214     C                   goto      FineSr
029200020214     C                   enddo
029300020214     C                   endif
029400020214      *
029500020214     C
029600020211      *
029700020211      * Ci deve essere almeno un P.O diverso da zero
029800020212 1a  C     v1cp01        ifeq      *zeros
029900020212     C     v1cp02        andeq     *zeros
030000020212     C     v1cp03        andeq     *zeros
030100020212     C     v1cp04        andeq     *zeros
030200020212     C     v1cp05        andeq     *zeros
030300020212     C     v1cp06        andeq     *zeros
030400020212     C     v1cp07        andeq     *zeros
030500020212     C     v1cp08        andeq     *zeros
030600020212     C     v1cp09        andeq     *zeros
030700020212     C     v1cp10        andeq     *zeros
030800020212     C     v1cp11        andeq     *zeros
030900020212     C     v1cp12        andeq     *zeros
031000020212     C     v1cp13        andeq     *zeros
031100020212     C     v1cp14        andeq     *zeros
031200020212     C     v1cp15        andeq     *zeros
031300020212     C     v1cp16        andeq     *zeros
031400020212     C     v1cp17        andeq     *zeros
031500020212     C     v1cp18        andeq     *zeros
031600020212     C     v1cp19        andeq     *zeros
031700020212      *
031800020215     C                   seton                                        2844
031900020212     C                   movel     msg(1)        V1cmsg
032000020212     C                   goto      FineSr
032100020212     C                   endif
032200020208      *
032300020208      * Controllo P.O
032400020215      *
032500020215      * Mi imposto il pou del codice
032600020215     C     v1ccod        div       10000         $Pou1             3 0
032700020212      *
032800020215 1a  C     v1cp01        ifne      *zeros
032900020215     C     v1cp01        ifne      999
033000020215     C     v1cp01        chain     Azorg01l                           21
033100020215 2a  C   21              do
033200020215     C                   seton                                        2851
033300020215     C                   movel     msg(2)        V1cmsg
033400020215     C                   goto      FineSr
033500020215 2-  C                   enddo
033600020215     C                   endif
033700020215      *
033800020215 2a  C  n21              do
033900020215      * Controllo che il record non esista già in tabella
034000020215     C                   movel     *blanks       TbeKe1
034100020215     C                   movel     *blanks       TbeKe2
034200020215     C                   move      v1ccod        $Campo7
034300020215     C                   movel     $Campo7       TbeKe1
034400020215     C                   move      v1cp01        $Campo3           3
034500020215     C                   movel     $Campo3       TbeKe2
034600020215     C                   exsr      ChnTbe
034700020215 3a  C  n23              do
034800020215     C                   seton                                        2851
034900020215     C                   movel     msg(3)        V1cmsg
035000020215     C                   goto      FineSr
035100020215 3-  C                   enddo
035200020215      *
035300020215      * Controllo P.O e cliente poste o non poste
035400020215     C                   move      v1cp01        $Pou2             3 0
035500020215     C                   exsr      CtrPos
035600020215 3a  C   24              do
035700020215     C                   seton                                        285142
035800020215     C                   movel     msg(5)        V1cmsg
035900020215     C                   goto      FineSr
036000020215 3-  C                   enddo
036100020215 2-  C                   enddo
036200020215 1-  C                   endif
036300020215      *
036400020215 1a  C     v1cp01        ifeq      999
036500020215      * Controllo che il cliente non mantenga il numero spedizione
036600020215     C                   move      v1ccod        $Campo7
036700020215     C                   Exsr      CtrSpe
036800020320      *
036900020215 2a  C   25              do
037000020215     C                   seton                                        2851
037100020215     C                   movel     msg(4)        V1cmsg
037200020215     C                   goto      FineSr
037300020215 2-  C                   enddo
037400020320      *
037500020320     C     §3CCtm        ifne      *blanks
037600020320      * Controllo anche che il cliente non abbia un codice trattamento
037700020320      * tipo DISK 'B'
037800020320     C                   Exsr      CtrCdT
037900020320 2a  C   25              do
038000020320     C                   seton                                        2851
038100020320     C                   movel     msg(6)        V1cmsg
038200020320     C                   goto      FineSr
038300020320 2-  C                   enddo
038400020215 1-  C                   endif
038500020320 1-  C                   endif
038600020212      *
038700020212 1a  C     v1cp02        ifne      *zeros
038800020215     C     v1cp02        ifne      999
038900020212     C     v1cp02        chain     Azorg01l                           21
039000020212 2a  C   21              do
039100020212     C                   seton                                        2852
039200020212     C                   movel     msg(2)        V1cmsg
039300020212     C                   goto      FineSr
039400020212 2-  C                   enddo
039500020215     C                   endif
039600020212      *
039700020212 2a  C  n21              do
039800020212      * Controllo che il record non esista già in tabella
039900020212     C                   movel     *blanks       TbeKe1
040000020212     C                   movel     *blanks       TbeKe2
040100020212     C                   move      v1ccod        $Campo7
040200020212     C                   movel     $Campo7       TbeKe1
040300020212     C                   move      v1cp02        $Campo3           3
040400020212     C                   movel     $Campo3       TbeKe2
040500020212     C                   exsr      ChnTbe
040600020212 3a  C  n23              do
040700020212     C                   seton                                        2852
040800020212     C                   movel     msg(3)        V1cmsg
040900020212     C                   goto      FineSr
041000020212 3-  C                   enddo
041100020212      *
041200020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
041300020214     C                   move      v1cp02        $Pou2             3 0
041400020212     C                   exsr      CtrPos
041500020212 3a  C   24              do
041600020212     C                   seton                                        285242
041700020215     C                   movel     msg(5)        V1cmsg
041800020212     C                   goto      FineSr
041900020212 3-  C                   enddo
042000020212 2-  C                   enddo
042100020215 1-  C                   endif
042200020212      *
042300020215 1a  C     v1cp02        ifeq      999
042400020212      * Controllo che il cliente non mantenga il numero spedizione
042500020212     C                   move      v1ccod        $Campo7
042600020212     C                   Exsr      CtrSpe
042700020215 2a  C   25              do
042800020212     C                   seton                                        2852
042900020212     C                   movel     msg(4)        V1cmsg
043000020212     C                   goto      FineSr
043100020215 2-  C                   enddo
043200020320      *
043300020320     C     §3CCtm        ifne      *blanks
043400020320      * Controllo anche che il cliente non abbia un codice trattamento
043500020320      * tipo DISK 'B'
043600020320     C                   Exsr      CtrCdT
043700020320 2a  C   25              do
043800020320     C                   seton                                        2852
043900020320     C                   movel     msg(6)        V1cmsg
044000020320     C                   goto      FineSr
044100020320 2-  C                   enddo
044200020320 1-  C                   endif
044300020215 1-  C                   endif
044400020212      *
044500020212      *
044600020212 1a  C     v1cp03        ifne      *zeros
044700020215     C     v1cp03        ifne      999
044800020212     C     v1cp03        chain     Azorg01l                           21
044900020212 2a  C   21              do
045000020212     C                   seton                                        2853
045100020212     C                   movel     msg(2)        V1cmsg
045200020212     C                   goto      FineSr
045300020212 2-  C                   enddo
045400020215     C                   endif
045500020212      *
045600020212 2a  C  n21              do
045700020212      * Controllo che il record non esista già in tabella
045800020212     C                   movel     *blanks       TbeKe1
045900020212     C                   movel     *blanks       TbeKe2
046000020212     C                   move      v1ccod        $Campo7
046100020212     C                   movel     $Campo7       TbeKe1
046200020212     C                   move      v1cp03        $Campo3           3
046300020212     C                   movel     $Campo3       TbeKe2
046400020212     C                   exsr      ChnTbe
046500020212 3a  C  n23              do
046600020212     C                   seton                                        2853
046700020212     C                   movel     msg(3)        V1cmsg
046800020212     C                   goto      FineSr
046900020212 3-  C                   enddo
047000020212      *
047100020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
047200020214     C                   move      v1cp03        $Pou2             3 0
047300020212     C                   exsr      CtrPos
047400020212 3a  C   24              do
047500020212     C                   seton                                        285342
047600020215     C                   movel     msg(5)        V1cmsg
047700020212     C                   goto      FineSr
047800020212 3-  C                   enddo
047900020212 2-  C                   enddo
048000020215     C                   endif
048100020212      *
048200020215 1a  C     v1cp03        ifeq      999
048300020212      * Controllo che il cliente non mantenga il numero spedizione
048400020212     C                   move      v1ccod        $Campo7
048500020212     C                   Exsr      CtrSpe
048600020215 2a  C   25              do
048700020212     C                   seton                                        2853
048800020212     C                   movel     msg(4)        V1cmsg
048900020212     C                   goto      FineSr
049000020215 2-  C                   enddo
049100020320      *
049200020320     C     §3CCtm        ifne      *blanks
049300020320      * Controllo anche che il cliente non abbia un codice trattamento
049400020320      * tipo DISK 'B'
049500020320     C                   Exsr      CtrCdT
049600020320 2a  C   25              do
049700020320     C                   seton                                        2853
049800020320     C                   movel     msg(6)        V1cmsg
049900020320     C                   goto      FineSr
050000020320 2-  C                   enddo
050100020320 1-  C                   endif
050200020212 1-  C                   endif
050300020212      *
050400020212 1a  C     v1cp04        ifne      *zeros
050500020215     C     v1cp04        ifne      999
050600020212     C     v1cp04        chain     Azorg01l                           21
050700020212 2a  C   21              do
050800020212     C                   seton                                        2854
050900020212     C                   movel     msg(2)        V1cmsg
051000020212     C                   goto      FineSr
051100020212 2-  C                   enddo
051200020215     C                   endif
051300020212      *
051400020212 2a  C  n21              do
051500020212      * Controllo che il record non esista già in tabella
051600020212     C                   movel     *blanks       TbeKe1
051700020212     C                   movel     *blanks       TbeKe2
051800020212     C                   move      v1ccod        $Campo7
051900020212     C                   movel     $Campo7       TbeKe1
052000020212     C                   move      v1cp04        $Campo3           3
052100020212     C                   movel     $Campo3       TbeKe2
052200020212     C                   exsr      ChnTbe
052300020212 3a  C  n23              do
052400020212     C                   seton                                        2854
052500020212     C                   movel     msg(3)        V1cmsg
052600020212     C                   goto      FineSr
052700020212 3-  C                   enddo
052800020212      *
052900020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
053000020214     C                   move      v1cp04        $Pou2             3 0
053100020212     C                   exsr      CtrPos
053200020212 3a  C   24              do
053300020212     C                   seton                                        285442
053400020215     C                   movel     msg(5)        V1cmsg
053500020212     C                   goto      FineSr
053600020212 3-  C                   enddo
053700020212 2-  C                   enddo
053800020215     C                   endif
053900020212      *
054000020215 1a  C     v1cp04        ifeq      999
054100020212      * Controllo che il cliente non mantenga il numero spedizione
054200020212     C                   move      v1ccod        $Campo7
054300020212     C                   Exsr      CtrSpe
054400020215 2a  C   25              do
054500020212     C                   seton                                        2854
054600020212     C                   movel     msg(4)        V1cmsg
054700020212     C                   goto      FineSr
054800020215 2-  C                   enddo
054900020320      *
055000020320     C     §3CCtm        ifne      *blanks
055100020320      * Controllo anche che il cliente non abbia un codice trattamento
055200020320      * tipo DISK 'B'
055300020320     C                   Exsr      CtrCdT
055400020320 2a  C   25              do
055500020320     C                   seton                                        2854
055600020320     C                   movel     msg(6)        V1cmsg
055700020320     C                   goto      FineSr
055800020320 2-  C                   enddo
055900020320 1-  C                   endif
056000020215 1-  C                   endif
056100020212      *
056200020212 1a  C     v1cp05        ifne      *zeros
056300020215     C     v1cp05        ifne      999
056400020212     C     v1cp05        chain     Azorg01l                           21
056500020212 2a  C   21              do
056600020212     C                   seton                                        2855
056700020212     C                   movel     msg(2)        V1cmsg
056800020212     C                   goto      FineSr
056900020212 2-  C                   enddo
057000020215     C                   endif
057100020212      *
057200020212 2a  C  n21              do
057300020212      * Controllo che il record non esista già in tabella
057400020212     C                   movel     *blanks       TbeKe1
057500020212     C                   movel     *blanks       TbeKe2
057600020212     C                   move      v1ccod        $Campo7
057700020212     C                   movel     $Campo7       TbeKe1
057800020212     C                   move      v1cp05        $Campo3           3
057900020212     C                   movel     $Campo3       TbeKe2
058000020212     C                   exsr      ChnTbe
058100020212 3a  C  n23              do
058200020212     C                   seton                                        2855
058300020212     C                   movel     msg(3)        V1cmsg
058400020212     C                   goto      FineSr
058500020212 3-  C                   enddo
058600020212      *
058700020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
058800020214     C                   move      v1cp05        $Pou2             3 0
058900020212     C                   exsr      CtrPos
059000020212 3a  C   24              do
059100020212     C                   seton                                        285542
059200020215     C                   movel     msg(5)        V1cmsg
059300020212     C                   goto      FineSr
059400020212 3-  C                   enddo
059500020212 2-  C                   enddo
059600020215     C                   endif
059700020212      *
059800020215 1a  C     v1cp05        ifeq      999
059900020212      * Controllo che il cliente non mantenga il numero spedizione
060000020212     C                   move      v1ccod        $Campo7
060100020212     C                   Exsr      CtrSpe
060200020215 2a  C   25              do
060300020212     C                   seton                                        2855
060400020212     C                   movel     msg(4)        V1cmsg
060500020212     C                   goto      FineSr
060600020215 2-  C                   enddo
060700020320      *
060800020320     C     §3CCtm        ifne      *blanks
060900020320      * Controllo anche che il cliente non abbia un codice trattamento
061000020320      * tipo DISK 'B'
061100020320     C                   Exsr      CtrCdT
061200020320 2a  C   25              do
061300020320     C                   seton                                        2855
061400020320     C                   movel     msg(6)        V1cmsg
061500020320     C                   goto      FineSr
061600020320 2-  C                   enddo
061700020320 1-  C                   endif
061800020215 1-  C                   endif
061900020212      *
062000020212 1a  C     v1cp06        ifne      *zeros
062100020215     C     v1cp06        ifne      999
062200020212     C     v1cp06        chain     Azorg01l                           21
062300020212 2a  C   21              do
062400020212     C                   seton                                        2856
062500020212     C                   movel     msg(2)        V1cmsg
062600020212     C                   goto      FineSr
062700020212 2-  C                   enddo
062800020215     C                   endif
062900020212      *
063000020212 2a  C  n21              do
063100020212      * Controllo che il record non esista già in tabella
063200020212     C                   movel     *blanks       TbeKe1
063300020212     C                   movel     *blanks       TbeKe2
063400020212     C                   move      v1ccod        $Campo7
063500020212     C                   movel     $Campo7       TbeKe1
063600020212     C                   move      v1cp06        $Campo3           3
063700020212     C                   movel     $Campo3       TbeKe2
063800020212     C                   exsr      ChnTbe
063900020212 3a  C  n23              do
064000020212     C                   seton                                        2856
064100020212     C                   movel     msg(3)        V1cmsg
064200020212     C                   goto      FineSr
064300020212 3-  C                   enddo
064400020212      *
064500020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
064600020214     C                   move      v1cp06        $Pou2             3 0
064700020212     C                   exsr      CtrPos
064800020212 3a  C   24              do
064900020212     C                   seton                                        285642
065000020215     C                   movel     msg(5)        V1cmsg
065100020212     C                   goto      FineSr
065200020212 3-  C                   enddo
065300020212 2-  C                   enddo
065400020215 1-  C                   endif
065500020215      *
065600020215 1a  C     v1cp06        ifeq      999
065700020212      * Controllo che il cliente non mantenga il numero spedizione
065800020212     C                   move      v1ccod        $Campo7
065900020212     C                   Exsr      CtrSpe
066000020215 2a  C   25              do
066100020212     C                   seton                                        2856
066200020212     C                   movel     msg(4)        V1cmsg
066300020212     C                   goto      FineSr
066400020215 2-  C                   enddo
066500020320      *
066600020320     C     §3CCtm        ifne      *blanks
066700020320      * Controllo anche che il cliente non abbia un codice trattamento
066800020320      * tipo DISK 'B'
066900020320     C                   Exsr      CtrCdT
067000020320 2a  C   25              do
067100020320     C                   seton                                        2856
067200020320     C                   movel     msg(6)        V1cmsg
067300020320     C                   goto      FineSr
067400020320 2-  C                   enddo
067500020320 1-  C                   endif
067600020215 1-  C                   endif
067700020212      *
067800020212 1a  C     v1cp07        ifne      *zeros
067900020215     C     v1cp07        ifne      999
068000020212     C     v1cp07        chain     Azorg01l                           21
068100020212 2a  C   21              do
068200020212     C                   seton                                        2857
068300020212     C                   movel     msg(2)        V1cmsg
068400020212     C                   goto      FineSr
068500020212 2-  C                   enddo
068600020215     C                   endif
068700020212      *
068800020212 2a  C  n21              do
068900020212      * Controllo che il record non esista già in tabella
069000020212     C                   movel     *blanks       TbeKe1
069100020212     C                   movel     *blanks       TbeKe2
069200020212     C                   move      v1ccod        $Campo7
069300020212     C                   movel     $Campo7       TbeKe1
069400020212     C                   move      v1cp07        $Campo3           3
069500020212     C                   movel     $Campo3       TbeKe2
069600020212     C                   exsr      ChnTbe
069700020212 3a  C  n23              do
069800020212     C                   seton                                        2857
069900020212     C                   movel     msg(3)        V1cmsg
070000020212     C                   goto      FineSr
070100020212 3-  C                   enddo
070200020212      *
070300020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
070400020214     C                   move      v1cp07        $Pou2             3 0
070500020212     C                   exsr      CtrPos
070600020212 3a  C   24              do
070700020212     C                   seton                                        285742
070800020215     C                   movel     msg(5)        V1cmsg
070900020212     C                   goto      FineSr
071000020212 3-  C                   enddo
071100020212 2-  C                   enddo
071200020215     C                   endif
071300020215      *
071400020215 1a  C     v1cp07        ifeq      999
071500020212      * Controllo che il cliente non mantenga il numero spedizione
071600020212     C                   move      v1ccod        $Campo7
071700020212     C                   Exsr      CtrSpe
071800020215 2a  C   25              do
071900020212     C                   seton                                        2857
072000020212     C                   movel     msg(4)        V1cmsg
072100020212     C                   goto      FineSr
072200020215 2-  C                   enddo
072300020320      *
072400020320     C     §3CCtm        ifne      *blanks
072500020320      * Controllo anche che il cliente non abbia un codice trattamento
072600020320      * tipo DISK 'B'
072700020320     C                   Exsr      CtrCdT
072800020320 2a  C   25              do
072900020320     C                   seton                                        2857
073000020320     C                   movel     msg(6)        V1cmsg
073100020320     C                   goto      FineSr
073200020320 2-  C                   enddo
073300020320 1-  C                   endif
073400020215 1-  C                   endif
073500020212      *
073600020212 1a  C     v1cp08        ifne      *zeros
073700020215     C     v1cp08        ifne      999
073800020212     C     v1cp08        chain     Azorg01l                           21
073900020212 2a  C   21              do
074000020212     C                   seton                                        2858
074100020212     C                   movel     msg(2)        V1cmsg
074200020212     C                   goto      FineSr
074300020212 2-  C                   enddo
074400020215     C                   endif
074500020212      *
074600020212 2a  C  n21              do
074700020212      * Controllo che il record non esista già in tabella
074800020212     C                   movel     *blanks       TbeKe1
074900020212     C                   movel     *blanks       TbeKe2
075000020212     C                   move      v1ccod        $Campo7
075100020212     C                   movel     $Campo7       TbeKe1
075200020212     C                   move      v1cp08        $Campo3           3
075300020212     C                   movel     $Campo3       TbeKe2
075400020212     C                   exsr      ChnTbe
075500020212 3a  C  n23              do
075600020212     C                   seton                                        2858
075700020212     C                   movel     msg(3)        V1cmsg
075800020212     C                   goto      FineSr
075900020212 3-  C                   enddo
076000020212      *
076100020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
076200020214     C                   move      v1cp08        $Pou2             3 0
076300020212     C                   exsr      CtrPos
076400020212 3a  C   24              do
076500020212     C                   seton                                        285842
076600020215     C                   movel     msg(5)        V1cmsg
076700020212     C                   goto      FineSr
076800020212 3-  C                   enddo
076900020212 2-  C                   enddo
077000020215 1-  C                   endif
077100020215      *
077200020215 1a  C     v1cp08        ifeq      999
077300020212      * Controllo che il cliente non mantenga il numero spedizione
077400020212     C                   move      v1ccod        $Campo7
077500020212     C                   Exsr      CtrSpe
077600020215 2a  C   25              do
077700020212     C                   seton                                        2858
077800020212     C                   movel     msg(4)        V1cmsg
077900020212     C                   goto      FineSr
078000020215 2-  C                   enddo
078100020320      *
078200020320     C     §3CCtm        ifne      *blanks
078300020320      * Controllo anche che il cliente non abbia un codice trattamento
078400020320      * tipo DISK 'B'
078500020320     C                   Exsr      CtrCdT
078600020320 2a  C   25              do
078700020320     C                   seton                                        2858
078800020320     C                   movel     msg(6)        V1cmsg
078900020320     C                   goto      FineSr
079000020320 2-  C                   enddo
079100020320 1-  C                   endif
079200020215 1-  C                   endif
079300020212      *
079400020212 1a  C     v1cp09        ifne      *zeros
079500020215     C     v1cp09        ifne      999
079600020212     C     v1cp09        chain     Azorg01l                           21
079700020212 2a  C   21              do
079800020212     C                   seton                                        2859
079900020212     C                   movel     msg(2)        V1cmsg
080000020212     C                   goto      FineSr
080100020212 2-  C                   enddo
080200020215 1-  C                   endif
080300020212      *
080400020212 2a  C  n21              do
080500020212      * Controllo che il record non esista già in tabella
080600020212     C                   movel     *blanks       TbeKe1
080700020212     C                   movel     *blanks       TbeKe2
080800020212     C                   move      v1ccod        $Campo7
080900020212     C                   movel     $Campo7       TbeKe1
081000020212     C                   move      v1cp09        $Campo3           3
081100020212     C                   movel     $Campo3       TbeKe2
081200020212     C                   exsr      ChnTbe
081300020212 3a  C  n23              do
081400020212     C                   seton                                        2859
081500020212     C                   movel     msg(3)        V1cmsg
081600020212     C                   goto      FineSr
081700020212 3-  C                   enddo
081800020212      *
081900020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
082000020214     C                   move      v1cp09        $Pou2             3 0
082100020212     C                   exsr      CtrPos
082200020212 3a  C   24              do
082300020212     C                   seton                                        285942
082400020215     C                   movel     msg(5)        V1cmsg
082500020212     C                   goto      FineSr
082600020212 3-  C                   enddo
082700020212 2-  C                   enddo
082800020215 1-  C                   endif
082900020215      *
083000020215 1a  C     v1cp09        ifeq      999
083100020212      * Controllo che il cliente non mantenga il numero spedizione
083200020212     C                   move      v1ccod        $Campo7
083300020212     C                   Exsr      CtrSpe
083400020215 2a  C   25              do
083500020212     C                   seton                                        2859
083600020212     C                   movel     msg(4)        V1cmsg
083700020212     C                   goto      FineSr
083800020215 2-  C                   enddo
083900020320      *
084000020320     C     §3CCtm        ifne      *blanks
084100020320      * Controllo anche che il cliente non abbia un codice trattamento
084200020320      * tipo DISK 'B'
084300020320     C                   Exsr      CtrCdT
084400020320 2a  C   25              do
084500020320     C                   seton                                        2859
084600020320     C                   movel     msg(6)        V1cmsg
084700020320     C                   goto      FineSr
084800020320 2-  C                   enddo
084900020320 1-  C                   endif
085000020215 1-  C                   endif
085100020212      *
085200020212 1a  C     v1cp10        ifne      *zeros
085300020215     C     v1cp10        ifne      999
085400020212     C     v1cp10        chain     Azorg01l                           21
085500020212 2a  C   21              do
085600020212     C                   seton                                        2860
085700020212     C                   movel     msg(2)        V1cmsg
085800020212     C                   goto      FineSr
085900020212 2-  C                   enddo
086000020215     C                   endif
086100020212      *
086200020212 2a  C  n21              do
086300020212      * Controllo che il record non esista già in tabella
086400020212     C                   movel     *blanks       TbeKe1
086500020212     C                   movel     *blanks       TbeKe2
086600020212     C                   move      v1ccod        $Campo7
086700020212     C                   movel     $Campo7       TbeKe1
086800020212     C                   move      v1cp10        $Campo3           3
086900020212     C                   movel     $Campo3       TbeKe2
087000020212     C                   exsr      ChnTbe
087100020212 3a  C  n23              do
087200020212     C                   seton                                        2860
087300020212     C                   movel     msg(3)        V1cmsg
087400020212     C                   goto      FineSr
087500020212 3-  C                   enddo
087600020212      *
087700020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
087800020214     C                   move      v1cp10        $Pou2             3 0
087900020212     C                   exsr      CtrPos
088000020212 3a  C   24              do
088100020212     C                   seton                                        286042
088200020215     C                   movel     msg(5)        V1cmsg
088300020212     C                   goto      FineSr
088400020212 3-  C                   enddo
088500020212 2-  C                   enddo
088600020215     C                   endif
088700020215      *
088800020215 1a  C     v1cp10        ifeq      999
088900020212      * Controllo che il cliente non mantenga il numero spedizione
089000020212     C                   move      v1ccod        $Campo7
089100020212     C                   Exsr      CtrSpe
089200020215 2a  C   25              do
089300020212     C                   seton                                        2860
089400020212     C                   movel     msg(4)        V1cmsg
089500020212     C                   goto      FineSr
089600020215 2-  C                   enddo
089700020320      *
089800020320     C     §3CCtm        ifne      *blanks
089900020320      * Controllo anche che il cliente non abbia un codice trattamento
090000020320      * tipo DISK 'B'
090100020320     C                   Exsr      CtrCdT
090200020320 2a  C   25              do
090300020320     C                   seton                                        2860
090400020320     C                   movel     msg(6)        V1cmsg
090500020320     C                   goto      FineSr
090600020320 2-  C                   enddo
090700020320 1-  C                   endif
090800020215 1-  C                   endif
090900020212      *
091000020212 1a  C     v1cp11        ifne      *zeros
091100020215     C     v1cp11        ifne      999
091200020212     C     v1cp11        chain     Azorg01l                           21
091300020212 2a  C   21              do
091400020212     C                   seton                                        2861
091500020212     C                   movel     msg(2)        V1cmsg
091600020212     C                   goto      FineSr
091700020212 2-  C                   enddo
091800020215 1-  C                   endif
091900020212      *
092000020212 2a  C  n21              do
092100020212      * Controllo che il record non esista già in tabella
092200020212     C                   movel     *blanks       TbeKe1
092300020212     C                   movel     *blanks       TbeKe2
092400020212     C                   move      v1ccod        $Campo7
092500020212     C                   movel     $Campo7       TbeKe1
092600020212     C                   move      v1cp11        $Campo3           3
092700020212     C                   movel     $Campo3       TbeKe2
092800020212     C                   exsr      ChnTbe
092900020212 3a  C  n23              do
093000020212     C                   seton                                        2861
093100020212     C                   movel     msg(3)        V1cmsg
093200020212     C                   goto      FineSr
093300020212 3-  C                   enddo
093400020212      *
093500020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
093600020214     C                   move      v1cp11        $Pou2             3 0
093700020212     C                   exsr      CtrPos
093800020212 3a  C   24              do
093900020212     C                   seton                                        286142
094000020215     C                   movel     msg(5)        V1cmsg
094100020212     C                   goto      FineSr
094200020212 3-  C                   enddo
094300020212 2-  C                   enddo
094400020215     C                   endif
094500020215      *
094600020215 1a  C     v1cp11        ifeq      999
094700020212      * Controllo che il cliente non mantenga il numero spedizione
094800020212     C                   move      v1ccod        $Campo7
094900020212     C                   Exsr      CtrSpe
095000020215 2a  C   25              do
095100020212     C                   seton                                        2861
095200020212     C                   movel     msg(4)        V1cmsg
095300020212     C                   goto      FineSr
095400020215 2-  C                   enddo
095500020320      *
095600020320     C     §3CCtm        ifne      *blanks
095700020320      * Controllo anche che il cliente non abbia un codice trattamento
095800020320      * tipo DISK 'B'
095900020320     C                   Exsr      CtrCdT
096000020320 2a  C   25              do
096100020320     C                   seton                                        2861
096200020320     C                   movel     msg(6)        V1cmsg
096300020320     C                   goto      FineSr
096400020320 2-  C                   enddo
096500020320 1-  C                   endif
096600020215 1-  C                   endif
096700020212      *
096800020212 1a  C     v1cp12        ifne      *zeros
096900020215     C     v1cp12        ifne      999
097000020212     C     v1cp12        chain     Azorg01l                           21
097100020212 2a  C   21              do
097200020212     C                   seton                                        2862
097300020212     C                   movel     msg(2)        V1cmsg
097400020212     C                   goto      FineSr
097500020212 2-  C                   enddo
097600020215     C                   endif
097700020212      *
097800020212 2a  C  n21              do
097900020212      * Controllo che il record non esista già in tabella
098000020212     C                   movel     *blanks       TbeKe1
098100020212     C                   movel     *blanks       TbeKe2
098200020212     C                   move      v1ccod        $Campo7
098300020212     C                   movel     $Campo7       TbeKe1
098400020212     C                   move      v1cp12        $Campo3           3
098500020212     C                   movel     $Campo3       TbeKe2
098600020212     C                   exsr      ChnTbe
098700020212 3a  C  n23              do
098800020212     C                   seton                                        2862
098900020212     C                   movel     msg(3)        V1cmsg
099000020212     C                   goto      FineSr
099100020212 3-  C                   enddo
099200020212      *
099300020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
099400020214     C                   move      v1cp12        $Pou2             3 0
099500020212     C                   exsr      CtrPos
099600020212 3a  C   24              do
099700020212     C                   seton                                        286242
099800020215     C                   movel     msg(5)        V1cmsg
099900020212     C                   goto      FineSr
100000020212 3-  C                   enddo
100100020212 2-  C                   enddo
100200020215     C                   endif
100300020215      *
100400020215 1a  C     v1cp12        ifeq      999
100500020212      * Controllo che il cliente non mantenga il numero spedizione
100600020212     C                   move      v1ccod        $Campo7
100700020212     C                   Exsr      CtrSpe
100800020215 2a  C   25              do
100900020212     C                   seton                                        2862
101000020212     C                   movel     msg(4)        V1cmsg
101100020212     C                   goto      FineSr
101200020215 2-  C                   enddo
101300020320      *
101400020320     C     §3CCtm        ifne      *blanks
101500020320      * Controllo anche che il cliente non abbia un codice trattamento
101600020320      * tipo DISK 'B'
101700020320     C                   Exsr      CtrCdT
101800020320 2a  C   25              do
101900020320     C                   seton                                        2862
102000020320     C                   movel     msg(6)        V1cmsg
102100020320     C                   goto      FineSr
102200020320 2-  C                   enddo
102300020320 1-  C                   endif
102400020215 1-  C                   endif
102500020212      *
102600020212 1a  C     v1cp13        ifne      *zeros
102700020215     C     v1cp13        ifne      999
102800020212     C     v1cp13        chain     Azorg01l                           21
102900020212 2a  C   21              do
103000020212     C                   seton                                        2863
103100020212     C                   movel     msg(2)        V1cmsg
103200020212     C                   goto      FineSr
103300020212 2-  C                   enddo
103400020215 1-  C                   endif
103500020212      *
103600020212 2a  C  n21              do
103700020212      * Controllo che il record non esista già in tabella
103800020212     C                   movel     *blanks       TbeKe1
103900020212     C                   movel     *blanks       TbeKe2
104000020212     C                   move      v1ccod        $Campo7
104100020212     C                   movel     $Campo7       TbeKe1
104200020212     C                   move      v1cp13        $Campo3           3
104300020212     C                   movel     $Campo3       TbeKe2
104400020212     C                   exsr      ChnTbe
104500020212 3a  C  n23              do
104600020212     C                   seton                                        2863
104700020212     C                   movel     msg(3)        V1cmsg
104800020212     C                   goto      FineSr
104900020212 3-  C                   enddo
105000020212      *
105100020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
105200020214     C                   move      v1cp13        $Pou2             3 0
105300020212     C                   exsr      CtrPos
105400020212 3a  C   24              do
105500020212     C                   seton                                        286342
105600020215     C                   movel     msg(5)        V1cmsg
105700020212     C                   goto      FineSr
105800020212 3-  C                   enddo
105900020212 2-  C                   enddo
106000020215     C                   endif
106100020215      *
106200020215 1a  C     v1cp13        ifeq      999
106300020212      * Controllo che il cliente non mantenga il numero spedizione
106400020212     C                   move      v1ccod        $Campo7
106500020212     C                   Exsr      CtrSpe
106600020215 2a  C   25              do
106700020212     C                   seton                                        2863
106800020212     C                   movel     msg(4)        V1cmsg
106900020212     C                   goto      FineSr
107000020215 2-  C                   enddo
107100020320      *
107200020320     C     §3CCtm        ifne      *blanks
107300020320      * Controllo anche che il cliente non abbia un codice trattamento
107400020320      * tipo DISK 'B'
107500020320     C                   Exsr      CtrCdT
107600020320 2a  C   25              do
107700020320     C                   seton                                        2863
107800020320     C                   movel     msg(6)        V1cmsg
107900020320     C                   goto      FineSr
108000020320 2-  C                   enddo
108100020320 1-  C                   endif
108200020215 1-  C                   endif
108300020212      *
108400020212 1a  C     v1cp14        ifne      *zeros
108500020215     C     v1cp14        ifne      999
108600020212     C     v1cp14        chain     Azorg01l                           21
108700020212 2a  C   21              do
108800020212     C                   seton                                        2864
108900020212     C                   movel     msg(2)        V1cmsg
109000020212     C                   goto      FineSr
109100020212 2-  C                   enddo
109200020215     C                   endif
109300020212      *
109400020212 2a  C  n21              do
109500020212      * Controllo che il record non esista già in tabella
109600020212     C                   movel     *blanks       TbeKe1
109700020212     C                   movel     *blanks       TbeKe2
109800020212     C                   move      v1ccod        $Campo7
109900020212     C                   movel     $Campo7       TbeKe1
110000020212     C                   move      v1cp14        $Campo3           3
110100020212     C                   movel     $Campo3       TbeKe2
110200020212     C                   exsr      ChnTbe
110300020212 3a  C  n23              do
110400020212     C                   seton                                        2864
110500020212     C                   movel     msg(3)        V1cmsg
110600020212     C                   goto      FineSr
110700020212 3-  C                   enddo
110800020212      *
110900020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
111000020214     C                   move      v1cp14        $Pou2             3 0
111100020212     C                   exsr      CtrPos
111200020212 3a  C   24              do
111300020212     C                   seton                                        286442
111400020215     C                   movel     msg(5)        V1cmsg
111500020212     C                   goto      FineSr
111600020212 3-  C                   enddo
111700020212 2-  C                   enddo
111800020215     C                   endif
111900020215      *
112000020215 1a  C     v1cp14        ifeq      999
112100020212      * Controllo che il cliente non mantenga il numero spedizione
112200020212     C                   move      v1ccod        $Campo7
112300020212     C                   Exsr      CtrSpe
112400020215 2a  C   25              do
112500020212     C                   seton                                        2864
112600020212     C                   movel     msg(4)        V1cmsg
112700020212     C                   goto      FineSr
112800020215 2-  C                   enddo
112900020320      *
113000020320     C     §3CCtm        ifne      *blanks
113100020320      * Controllo anche che il cliente non abbia un codice trattamento
113200020320      * tipo DISK 'B'
113300020320     C                   Exsr      CtrCdT
113400020320 2a  C   25              do
113500020320     C                   seton                                        2864
113600020320     C                   movel     msg(6)        V1cmsg
113700020320     C                   goto      FineSr
113800020320 2-  C                   enddo
113900020320 1-  C                   endif
114000020215 1-  C                   endif
114100020212      *
114200020212 1a  C     v1cp15        ifne      *zeros
114300020215     C     v1cp15        ifne      999
114400020212     C     v1cp15        chain     Azorg01l                           21
114500020212 2a  C   21              do
114600020212     C                   seton                                        2865
114700020212     C                   movel     msg(2)        V1cmsg
114800020212     C                   goto      FineSr
114900020212 2-  C                   enddo
115000020215     C                   endif
115100020212      *
115200020212 2a  C  n21              do
115300020212      * Controllo che il record non esista già in tabella
115400020212     C                   movel     *blanks       TbeKe1
115500020212     C                   movel     *blanks       TbeKe2
115600020212     C                   move      v1ccod        $Campo7
115700020212     C                   movel     $Campo7       TbeKe1
115800020212     C                   move      v1cp15        $Campo3           3
115900020212     C                   movel     $Campo3       TbeKe2
116000020212     C                   exsr      ChnTbe
116100020212 3a  C  n23              do
116200020212     C                   seton                                        2865
116300020212     C                   movel     msg(3)        V1cmsg
116400020212     C                   goto      FineSr
116500020212 3-  C                   enddo
116600020212      *
116700020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
116800020214     C                   move      v1cp15        $Pou2             3 0
116900020212     C                   exsr      CtrPos
117000020212 3a  C   24              do
117100020212     C                   seton                                        286542
117200020215     C                   movel     msg(5)        V1cmsg
117300020212     C                   goto      FineSr
117400020212 3-  C                   enddo
117500020212 2-  C                   enddo
117600020215     C                   endif
117700020215      *
117800020215 1a  C     v1cp15        ifeq      999
117900020212      * Controllo che il cliente non mantenga il numero spedizione
118000020212     C                   move      v1ccod        $Campo7
118100020212     C                   Exsr      CtrSpe
118200020215 2a  C   25              do
118300020212     C                   seton                                        2865
118400020212     C                   movel     msg(4)        V1cmsg
118500020212     C                   goto      FineSr
118600020215 2-  C                   enddo
118700020320      *
118800020320     C     §3CCtm        ifne      *blanks
118900020320      * Controllo anche che il cliente non abbia un codice trattamento
119000020320      * tipo DISK 'B'
119100020320     C                   Exsr      CtrCdT
119200020320 2a  C   25              do
119300020320     C                   seton                                        2865
119400020320     C                   movel     msg(6)        V1cmsg
119500020320     C                   goto      FineSr
119600020320 2-  C                   enddo
119700020320 1-  C                   endif
119800020215 1-  C                   endif
119900020212      *
120000020212 1a  C     v1cp16        ifne      *zeros
120100020215     C     v1cp16        ifne      999
120200020212     C     v1cp16        chain     Azorg01l                           21
120300020212 2a  C   21              do
120400020212     C                   seton                                        2866
120500020212     C                   movel     msg(2)        V1cmsg
120600020212     C                   goto      FineSr
120700020212 2-  C                   enddo
120800020215     C                   endif
120900020212      *
121000020212 2a  C  n21              do
121100020212      * Controllo che il record non esista già in tabella
121200020212     C                   movel     *blanks       TbeKe1
121300020212     C                   movel     *blanks       TbeKe2
121400020212     C                   move      v1ccod        $Campo7
121500020212     C                   movel     $Campo7       TbeKe1
121600020212     C                   move      v1cp16        $Campo3           3
121700020212     C                   movel     $Campo3       TbeKe2
121800020212     C                   exsr      ChnTbe
121900020212 3a  C  n23              do
122000020212     C                   seton                                        2866
122100020212     C                   movel     msg(3)        V1cmsg
122200020212     C                   goto      FineSr
122300020212 3-  C                   enddo
122400020212      *
122500020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
122600020214     C                   move      v1cp16        $Pou2             3 0
122700020212     C                   exsr      CtrPos
122800020212 3a  C   24              do
122900020212     C                   seton                                        286642
123000020215     C                   movel     msg(5)        V1cmsg
123100020212     C                   goto      FineSr
123200020212 3-  C                   enddo
123300020212 2-  C                   enddo
123400020215 1-  C                   endif
123500020215      *
123600020215 1a  C     v1cp16        ifeq      999
123700020212      * Controllo che il cliente non mantenga il numero spedizione
123800020212     C                   move      v1ccod        $Campo7
123900020212     C                   Exsr      CtrSpe
124000020215 2a  C   25              do
124100020212     C                   seton                                        2866
124200020212     C                   movel     msg(4)        V1cmsg
124300020212     C                   goto      FineSr
124400020215 2-  C                   enddo
124500020320      *
124600020320     C     §3CCtm        ifne      *blanks
124700020320      * Controllo anche che il cliente non abbia un codice trattamento
124800020320      * tipo DISK 'B'
124900020320     C                   Exsr      CtrCdT
125000020320 2a  C   25              do
125100020320     C                   seton                                        2866
125200020320     C                   movel     msg(6)        V1cmsg
125300020320     C                   goto      FineSr
125400020320 2-  C                   enddo
125500020320 1-  C                   endif
125600020215 1-  C                   endif
125700020212      *
125800020212 1a  C     v1cp17        ifne      *zeros
125900020215     C     v1cp17        ifne      999
126000020212     C     v1cp17        chain     Azorg01l                           21
126100020212 2a  C   21              do
126200020212     C                   seton                                        2867
126300020212     C                   movel     msg(2)        V1cmsg
126400020212     C                   goto      FineSr
126500020212 2-  C                   enddo
126600020215 1-  C                   endif
126700020212      *
126800020212 2a  C  n21              do
126900020212      * Controllo che il record non esista già in tabella
127000020212     C                   movel     *blanks       TbeKe1
127100020212     C                   movel     *blanks       TbeKe2
127200020212     C                   move      v1ccod        $Campo7
127300020212     C                   movel     $Campo7       TbeKe1
127400020212     C                   move      v1cp17        $Campo3           3
127500020212     C                   movel     $Campo3       TbeKe2
127600020212     C                   exsr      ChnTbe
127700020212 3a  C  n23              do
127800020212     C                   seton                                        2867
127900020212     C                   movel     msg(3)        V1cmsg
128000020212     C                   goto      FineSr
128100020212 3-  C                   enddo
128200020212      *
128300020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
128400020214     C                   move      v1cp17        $Pou2             3 0
128500020212     C                   exsr      CtrPos
128600020212 3a  C   24              do
128700020212     C                   seton                                        286742
128800020215     C                   movel     msg(5)        V1cmsg
128900020212     C                   goto      FineSr
129000020212 3-  C                   enddo
129100020212 2-  C                   enddo
129200020215     C                   endif
129300020215      *
129400020215 1a  C     v1cp17        ifeq      999
129500020212      * Controllo che il cliente non mantenga il numero spedizione
129600020212     C                   move      v1ccod        $Campo7
129700020212     C                   Exsr      CtrSpe
129800020215 2a  C   25              do
129900020212     C                   seton                                        2867
130000020212     C                   movel     msg(4)        V1cmsg
130100020212     C                   goto      FineSr
130200020215 2-  C                   enddo
130300020320      *
130400020320     C     §3CCtm        ifne      *blanks
130500020320      * Controllo anche che il cliente non abbia un codice trattamento
130600020320      * tipo DISK 'B'
130700020320     C                   Exsr      CtrCdT
130800020320 2a  C   25              do
130900020320     C                   seton                                        2867
131000020320     C                   movel     msg(6)        V1cmsg
131100020320     C                   goto      FineSr
131200020320 2-  C                   enddo
131300020320 1-  C                   endif
131400020215 1-  C                   endif
131500020212      *
131600020212 1a  C     v1cp18        ifne      *zeros
131700020215     C     v1cp18        ifne      999
131800020212     C     v1cp18        chain     Azorg01l                           21
131900020212 2a  C   21              do
132000020212     C                   seton                                        2868
132100020212     C                   movel     msg(2)        V1cmsg
132200020212     C                   goto      FineSr
132300020212 2-  C                   enddo
132400020215 1-  C                   endif
132500020212      *
132600020212 2a  C  n21              do
132700020212      * Controllo che il record non esista già in tabella
132800020212     C                   movel     *blanks       TbeKe1
132900020212     C                   movel     *blanks       TbeKe2
133000020212     C                   move      v1ccod        $Campo7
133100020212     C                   movel     $Campo7       TbeKe1
133200020212     C                   move      v1cp18        $Campo3           3
133300020212     C                   movel     $Campo3       TbeKe2
133400020212     C                   exsr      ChnTbe
133500020212 3a  C  n23              do
133600020212     C                   seton                                        2868
133700020212     C                   movel     msg(3)        V1cmsg
133800020212     C                   goto      FineSr
133900020212 3-  C                   enddo
134000020212      *
134100020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
134200020214     C                   move      v1cp18        $Pou2             3 0
134300020212     C                   exsr      CtrPos
134400020212 3a  C   24              do
134500020212     C                   seton                                        286842
134600020215     C                   movel     msg(5)        V1cmsg
134700020212     C                   goto      FineSr
134800020212 3-  C                   enddo
134900020212 2-  C                   enddo
135000020215     C                   endif
135100020215      *
135200020215 1a  C     v1cp18        ifeq      999
135300020212      * Controllo che il cliente non mantenga il numero spedizione
135400020212     C                   move      v1ccod        $Campo7
135500020212     C                   Exsr      CtrSpe
135600020215 2a  C   25              do
135700020212     C                   seton                                        2868
135800020212     C                   movel     msg(4)        V1cmsg
135900020212     C                   goto      FineSr
136000020215 2-  C                   enddo
136100020320      *
136200020320     C     §3CCtm        ifne      *blanks
136300020320      * Controllo anche che il cliente non abbia un codice trattamento
136400020320      * tipo DISK 'B'
136500020320     C                   Exsr      CtrCdT
136600020320 2a  C   25              do
136700020320     C                   seton                                        2868
136800020320     C                   movel     msg(6)        V1cmsg
136900020320     C                   goto      FineSr
137000020320 2-  C                   enddo
137100020320 1-  C                   endif
137200020215 1-  C                   endif
137300020208      *
137400020212 1a  C     v1cp19        ifne      *zeros
137500020215     C     v1cp19        ifne      999
137600020212     C     v1cp19        chain     Azorg01l                           21
137700020212 2a  C   21              do
137800020212     C                   seton                                        2869
137900020212     C                   movel     msg(2)        V1cmsg
138000020212     C                   goto      FineSr
138100020212 2-  C                   enddo
138200020215     C                   endif
138300020212      *
138400020212 2a  C  n21              do
138500020212      * Controllo che il record non esista già in tabella
138600020212     C                   movel     *blanks       TbeKe1
138700020212     C                   movel     *blanks       TbeKe2
138800020212     C                   move      v1ccod        $Campo7
138900020212     C                   movel     $Campo7       TbeKe1
139000020212     C                   move      v1cp19        $Campo3           3
139100020212     C                   movel     $Campo3       TbeKe2
139200020212     C                   exsr      ChnTbe
139300020212 3a  C  n23              do
139400020212     C                   seton                                        2869
139500020212     C                   movel     msg(3)        V1cmsg
139600020212     C                   goto      FineSr
139700020212 3-  C                   enddo
139800020212      *
139900020212      * Controllo P.O e cliente poste o non poste, accendo anche il 42
140000020214     C                   move      v1cp19        $Pou2             3 0
140100020212     C                   exsr      CtrPos
140200020212 3a  C   24              do
140300020212     C                   seton                                        286942
140400020215     C                   movel     msg(5)        V1cmsg
140500020212     C                   goto      FineSr
140600020212 3-  C                   enddo
140700020212 2-  C                   enddo
140800020215     C                   endif
140900020215      *
141000020215 1a  C     v1cp19        ifeq      999
141100020212      * Controllo che il cliente non mantenga il numero spedizione
141200020212     C                   move      v1ccod        $Campo7
141300020212     C                   Exsr      CtrSpe
141400020215 2a  C   25              do
141500020212     C                   seton                                        2869
141600020212     C                   movel     msg(4)        V1cmsg
141700020212     C                   goto      FineSr
141800020215 2-  C                   enddo
141900020320      *
142000020320     C     §3CCtm        ifne      *blanks
142100020320      * Controllo anche che il cliente non abbia un codice trattamento
142200020320      * tipo DISK 'B'
142300020320     C                   Exsr      CtrCdT
142400020320 2a  C   25              do
142500020320     C                   seton                                        2869
142600020320     C                   movel     msg(6)        V1cmsg
142700020320     C                   goto      FineSr
142800020320 2-  C                   enddo
142900020320 1-  C                   endif
143000020215 1-  C                   endif
143100020212      *
143200020208     C     FineSr        ENDSR
143300020208      *--------------------------------------------------------------------------------------------*
143400020215      * GesVd2 - Gestione seconda videata con opzioni: '2', '5', '4', '7'
143500020208      *--------------------------------------------------------------------------------------------*
143600020208     C     GesVd2        BEGSR
143700020208      *
143800020208      * Inizializzazione videata
143900020208     C     $CarVd2       ifeq      *on
144000020208     C                   EXSR      CarVd2
144100020208     C                   move      *off          $CarVd2
144200020208     C                   endif
144300020208      *
144400020215     C                   EXFMT     Tb23v2
144500020208      *
144600020212      * Tasti funzione se non si è in visualizza
144700020212     C     b22Opz        ifne      '5'
144800020212      *
144900020212     C                   select
145000020212     C     *inKl         wheneq    *on
145100020212     C                   EXSR      F12Ges
145200020212     C                   other
145300020212      *
145400020212      * Controlli solo se si è in modifica
145500020212     C     b22Opz        ifeq      '2'
145600020208     C                   EXSR      CtrV2
145700020212     C                   endif
145800020212     C                   endsl
145900020212     C                   endif
146000020208      *
146100020212     C     b22Opz        ifne      '5'
146200020208     C     *IN28         ifeq      *off
146300020212     C     *inKf         andeq     *on
146400020208     C                   EXSR      GesAgg
146500020212     C                   endif
146600020212     C                   else
146700020212     C                   move      'S'           $Fine
146800020212     C                   endif
146900020211      *
147000020208     C                   endsr
147100020208      *--------------------------------------------------------------------------------------------*
147200020208      * CarVd2 - Carica primo video
147300020208      *--------------------------------------------------------------------------------------------*
147400020208     C     CarVd2        BEGSR
147500020208      *
147600020215     C                   clear                   Tb23v2
147700020215     C                   movel     Ragut         V2CRag           20
147800020215     C                   movel     Knmus         V2CUte           10
147900020215     C                   movel     KnSif         V2CSif           10
148000020211      * Leggo la tabella
148100020211     C                   move      '3NN'         TbeCod
148200020211     C                   movel     b22lin        TbeLin
148300020211     C                   movel     b22sif        TbeSif
148400020211     C                   movel     *blanks       TbeKe1
148500020211     C                   movel     b22ke1        TbeKe1
148600020211     C                   movel     *blanks       TbeKe2
148700020211     C                   movel     b22ke2        TbeKe2
148800020211     C     k02tbe5       chain     tntbe02l                           21
148900020211     C
149000020211      *
149100020215      * Codice e decodifica
149200020215     C                   movel     b22ke1        $campo7           7
149300020215     C                   move      $campo7       V2ccod
149400020215     C                   move      $Campo7       $Cod              7 0
149500020215      *
149600020215     C                   EXSR      DeCod
149700020215     C                   if        o69err = *blanks and acoflg =' '
149800020215     C                   movel     acorag        v2dcod
149900020215     C                   endif
150000020215      *
150100020215      * P.O.
150200020215     C                   move      b22ke2        $campo3
150300020215     C                   movel     $campo3       V2CPou
150400020211      *
150500020211      * Nuovo codice e decodifica
150600020211     C                   movel     TbeUni        $campo7           7
150700020215     C                   move      $Campo7       v2ccli
150800020215      *
150900020215     C                   movel     *blanks       v2dcli
151000030704     C**   tbeuni        ifne      *blanks
151100030704     c                   if        $CAMPO7 <> *blanks  and
151200030704     c                             $CAMPO7 <> *zeros
151300020211     C                   move      $Campo7       $Cod              7 0
151400020211     C                   EXSR      DeCod
151500020211     C                   if        o69err = *blanks and acoflg =' '
151600020215     C                   movel     acorag        v2dcli
151700020211     C                   endif
151800020215     C                   endif
151900020208      *
152000020208      * Imposto l'opzione
152100020215     C                   move      *blanks       v2COpz
152200020208      *
152300020208      *  Modifica
152400020208     C     B22Opz        ifeq      '2'
152500020215     C                   eval      v2cOpz = Opz(1)
152600020212     C                   setoff                                       6063
152700020208     C                   endif
152800020208      *  Visualizzazione
152900020208     C     B22Opz        ifeq      '5'
153000020212     C                   seton                                        6063
153100020215     C                   eval      v2cOpz = Opz(2)
153200020208     C                   endif
153300020208      *  Annullamento
153400020208     C     B22Opz        ifeq      '4'
153500020208     C                   seton                                        60
153600020212     C                   setoff                                       63
153700020215     C                   eval      v2cOpz = Opz(3)
153800020208     C                   endif
153900020208      *  Ripristina
154000020208     C     B22Opz        ifeq      '7'
154100020208     C                   seton                                        60
154200020212     C                   setoff                                       63
154300020215     C                   eval      v2cOpz = Opz(4)
154400020208     C                   endif
154500020208      *
154600020208     C                   ENDSR
154700020208      *--------------------------------------------------------------------------------------------*
154800020215      * Controllo seconda videata
154900020208      *--------------------------------------------------------------------------------------------*
155000020208     C     CtrV2         BEGSR
155100020214     C                   setoff                                       2862
155200020214      *
155300020214      * Nuovo codice cliente con cui confermare
155400020215 1a  C     v2dcli        ifne      *blanks
155500020215     C     v2ccli        andeq     *zeros
155600020214      *
155700020214      * Ricerca estesa
155800020214     C
155900020214     C                   movel     ragut         paxdut           30
156000020215     C                   movel(P)  v2dcli        paxdmt           48
156100020214      *
156200020214     C                   z-add     0             paxsta
156300020214     C                   move      $Kci          paxccc            4 0
156400020214     C                   clear                   paxdit
156500020214     C                   z-add     1             paxnum
156600020214      *
156700020214     C                   CALL      'XALFA3BR'
156800020214     C                   PARM                    PAXDUT
156900020214     C                   PARM                    CODUT
157000020214     C                   PARM                    PAXDMT
157100020214     C                   PARM                    PAXCCC
157200020214     C                   PARM                    PAXSTA            1 0
157300020214     C                   PARM                    PAXFLR           90
157400020214     C                   PARM                    PAXDIT            3
157500020214     C                   PARM                    PAXNUM            2 0
157600020214     C                   PARM                    PAXKCM           80
157700020214     C                   PARM                    PAXKSM          140
157800020214     C                   PARM                    PAXKDM           60
157900020214      *
158000020214 2a  C     PaxSta        ifgt      -1
158100020214     C                   movel     paxksm        $campo7
158200020215     C                   move      $campo7       v2ccli
158300020215     C                   movel     paxdmt        v2dcli
158400020214 2-  C                   endif
158500020214      *
158600020215     C                   goto      Finesr2
158700020214 1-  C                   endif
158800020214      *
158900020214      * Controllo validità codice
159000020215     C                   move      *blanks       v2dcli
159100020215 1a  C     v2ccli        ifne      *zeros
159200020214      *
159300020215     C                   move      v2ccli        $Cod              7 0
159400020214     C                   EXSR      DeCod
159500020214      *
159600020214 2a  C                   if        o69err = *blanks and acoflg =' '
159700020215     C                   movel     acorag        v2dcli
159800020214      *
159900020215      * Controllo che i due codici cliente siano diversi
160000020215 3a  C     v2ccli        ifeq      v2ccod
160100020214     C                   seton                                        2862
160200020215     C                   goto      Finesr2
160300020214 3-  C                   endif
160400020215      *
160500020215      * Controllo che i due codici cliente siano POSTE/non POSTE
160600020215     C     v2ccod        div       10000         $Pou1
160700020215     C     v2ccli        div       10000         $Pou2
160800020215     C                   exsr      CtrPos
160900020215 3a  C   24              do
161000020215     C                   seton                                        2861
161100020215     C                   goto      FineSr2
161200020215 3-  C                   enddo
161300020215      *
161400020214 2e  C                   else
161500020214     C                   seton                                        2862
161600020214 2-  C                   endif
161700020214 1-  C                   endif
161800020208      *
161900020215     C     Finesr2       ENDSR
162000020208      *--------------------------------------------------------------------------------------------*
162100020208      * Scrittura/ Aggiornamenti
162200020208      *--------------------------------------------------------------------------------------------*
162300020208     C     GesAgg        BEGSR
162400020208      *
162500020211 1a  C     TipVid        ifeq      '1'
162600020211      *
162700020211      * Gestisco la scrittura per ogni punto operativo
162800020211      * Carico in schiera i P.O.
162900020211     C                   EXSR      CarPou
163000020211      *
163100020212     C                   z-add     1             X                 2 0
163200020211 2a  C     $Po999        ifeq      *off
163300020211 3a  C     Pou(X)        downe     *zeros
163400020211      * scrittura
163500020211     C                   EXSR      WriTbe
163600020211     C                   add       1             X
163700020211 3-  C                   enddo
163800020211      *
163900020211 2e  C                   else
164000020211     C                   move      '999'         Pou(X)
164100020211     C                   EXSR      WriTbe
164200020211 2-  C                   endif
164300020211 1-  C                   endif
164400020208      *
164500020211 1a  C     TipVid        ifeq      '2'
164600020208      * Gestisco l'aggiornamento
164700020208      *
164800020211 2a  C                   Select
164900020208      *
165000020208     C     b22opz        wheneq    '2'
165100020215     C                   move      v2ccli        $cli              7
165200020208     C                   movel     $cli          TbeUni
165300020208     C                   move      *blanks       TbeFtr
165400020208     C                   update    tntbe000
165500020208      *
165600020208     C     b22opz        wheneq    '4'
165700020208     C                   move      'A'           TbeAtb
165800020208     C                   move      *blanks       TbeFtr
165900020208     C                   update    tntbe000
166000020208      *
166100020208     C     b22opz        wheneq    '7'
166200020208     C                   move      ' '           TbeAtb
166300020208     C                   move      *blanks       TbeFtr
166400020208     C                   update    tntbe000
166500020211 2-  C                   endsl
166600020211 1-  C                   endif
166700020212     C                   movel     'S'           $Fine
166800020212      *
166900020208     C                   ENDSR
167000020211      *--------------------------------------------------------------------------------------------*
167100020211      * CarPou - Caricamento in schiera dei punti operativi
167200020211      *--------------------------------------------------------------------------------------------*
167300020211     C     CarPou        BEGSR
167400020211     C                   move      *off          $Po999            1
167500020211     C                   z-add     1             X
167600020211     C                   movea     *zeros        Pou
167700020211      *
167800020211     C     V1CP01        ifne      *zeros
167900020211     C     V1CP01        ifeq      999
168000020211     C                   move      *on           $Po999
168100020211     C                   z-add     1             X
168200020211     C                   else
168300020211     C                   move      V1CP01        Pou(X)
168400020211     C                   add       1             X
168500020211     C                   endif
168600020211     C                   endif
168700020211      *
168800020211     C     V1CP02        ifne      *zeros
168900020212     C     $Po999        andne     *on
169000020211     C     V1CP02        ifeq      999
169100020211     C                   move      *on           $Po999
169200020211     C                   z-add     1             X
169300020211     C                   else
169400020215     C                   z-add     1             Y                 2 0
169500020215     C                   move      v1cp02        $campo3
169600020215     C     $campo3       lookup    Pou(y)                                 21
169700020215     C     *in21         ifeq      *off
169800020212     C                   move      V1CP02        Pou(X)
169900020211     C                   add       1             X
170000020211     C                   endif
170100020215     C                   endif
170200020211     C                   endif
170300020211      *
170400020211     C     V1CP03        ifne      *zeros
170500020212     C     $Po999        andne     *on
170600020211     C     V1CP03        ifeq      999
170700020211     C                   move      *on           $Po999
170800020211     C                   z-add     1             X
170900020211     C                   else
171000020215     C                   z-add     1             Y                 2 0
171100020215     C                   move      v1cp03        $campo3
171200020215     C     $campo3       lookup    Pou(y)                                 21
171300020215     C     *in21         ifeq      *off
171400020212     C                   move      V1CP03        Pou(X)
171500020211     C                   add       1             X
171600020211     C                   endif
171700020215     C                   endif
171800020211     C                   endif
171900020211      *
172000020211     C     V1CP04        ifne      *zeros
172100020212     C     $Po999        andne     *on
172200020211     C     V1CP04        ifeq      999
172300020211     C                   move      *on           $Po999
172400020211     C                   z-add     1             X
172500020211     C                   else
172600020215     C                   z-add     1             Y                 2 0
172700020215     C                   move      v1cp04        $campo3
172800020215     C     $campo3       lookup    Pou(y)                                 21
172900020215     C     *in21         ifeq      *off
173000020212     C                   move      V1CP04        Pou(X)
173100020211     C                   add       1             X
173200020211     C                   endif
173300020215     C                   endif
173400020211     C                   endif
173500020211      *
173600020211     C     V1CP05        ifne      *zeros
173700020212     C     $Po999        andne     *on
173800020211     C     V1CP05        ifeq      999
173900020211     C                   move      *on           $Po999
174000020211     C                   z-add     1             X
174100020211     C                   else
174200020215     C                   z-add     1             Y                 2 0
174300020215     C                   move      v1cp05        $campo3
174400020215     C     $campo3       lookup    Pou(y)                                 21
174500020215     C     *in21         ifeq      *off
174600020212     C                   move      V1CP05        Pou(X)
174700020211     C                   add       1             X
174800020211     C                   endif
174900020215     C                   endif
175000020211     C                   endif
175100020211      *
175200020211     C     V1CP06        ifne      *zeros
175300020212     C     $Po999        andne     *on
175400020211     C     V1CP06        ifeq      999
175500020211     C                   move      *on           $Po999
175600020211     C                   z-add     1             X
175700020211     C                   else
175800020215     C                   z-add     1             Y                 2 0
175900020215     C                   move      v1cp06        $campo3
176000020215     C     $campo3       lookup    Pou(y)                                 21
176100020215     C     *in21         ifeq      *off
176200020212     C                   move      V1CP06        Pou(X)
176300020211     C                   add       1             X
176400020211     C                   endif
176500020215     C                   endif
176600020211     C                   endif
176700020211      *
176800020211     C     V1CP07        ifne      *zeros
176900020212     C     $Po999        andne     *on
177000020211     C     V1CP07        ifeq      999
177100020211     C                   move      *on           $Po999
177200020211     C                   z-add     1             X
177300020211     C                   else
177400020215     C                   z-add     1             Y                 2 0
177500020215     C                   move      v1cp07        $campo3
177600020215     C     $campo3       lookup    Pou(y)                                 21
177700020215     C     *in21         ifeq      *off
177800020212     C                   move      V1CP07        Pou(X)
177900020211     C                   add       1             X
178000020211     C                   endif
178100020215     C                   endif
178200020211     C                   endif
178300020211      *
178400020211     C     V1CP08        ifne      *zeros
178500020212     C     $Po999        andne     *on
178600020211     C     V1CP08        ifeq      999
178700020211     C                   move      *on           $Po999
178800020211     C                   z-add     1             X
178900020211     C                   else
179000020215     C                   z-add     1             Y                 2 0
179100020215     C                   move      v1cp08        $campo3
179200020215     C     $campo3       lookup    Pou(y)                                 21
179300020215     C     *in21         ifeq      *off
179400020212     C                   move      V1CP08        Pou(X)
179500020211     C                   add       1             X
179600020211     C                   endif
179700020215     C                   endif
179800020211     C                   endif
179900020211      *
180000020211     C     V1CP09        ifne      *zeros
180100020212     C     $Po999        andne     *on
180200020211     C     V1CP09        ifeq      999
180300020211     C                   move      *on           $Po999
180400020211     C                   z-add     1             X
180500020211     C                   else
180600020215     C                   z-add     1             Y                 2 0
180700020215     C                   move      v1cp09        $campo3
180800020215     C     $campo3       lookup    Pou(y)                                 21
180900020215     C     *in21         ifeq      *off
181000020212     C                   move      V1CP09        Pou(X)
181100020211     C                   add       1             X
181200020211     C                   endif
181300020215     C                   endif
181400020211     C                   endif
181500020211      *
181600020211     C     V1CP10        ifne      *zeros
181700020212     C     $Po999        andne     *on
181800020211     C     V1CP10        ifeq      999
181900020211     C                   move      *on           $Po999
182000020211     C                   z-add     1             X
182100020211     C                   else
182200020215     C                   z-add     1             Y                 2 0
182300020215     C                   move      v1cp10        $campo3
182400020215     C     $campo3       lookup    Pou(y)                                 21
182500020215     C     *in21         ifeq      *off
182600020212     C                   move      V1CP10        Pou(X)
182700020211     C                   add       1             X
182800020211     C                   endif
182900020215     C                   endif
183000020211     C                   endif
183100020211      *
183200020211     C     V1CP11        ifne      *zeros
183300020212     C     $Po999        andne     *on
183400020211     C     V1CP11        ifeq      999
183500020211     C                   move      *on           $Po999
183600020211     C                   z-add     1             X
183700020211     C                   else
183800020215     C                   z-add     1             Y                 2 0
183900020215     C                   move      v1cp11        $campo3
184000020215     C     $campo3       lookup    Pou(y)                                 21
184100020215     C     *in21         ifeq      *off
184200020212     C                   move      V1CP11        Pou(X)
184300020211     C                   add       1             X
184400020211     C                   endif
184500020215     C                   endif
184600020211     C                   endif
184700020211      *
184800020211     C     V1CP12        ifne      *zeros
184900020212     C     $Po999        andne     *on
185000020211     C     V1CP12        ifeq      999
185100020211     C                   move      *on           $Po999
185200020211     C                   z-add     1             X
185300020211     C                   else
185400020215     C                   z-add     1             Y                 2 0
185500020215     C                   move      v1cp12        $campo3
185600020215     C     $campo3       lookup    Pou(y)                                 21
185700020215     C     *in21         ifeq      *off
185800020212     C                   move      V1CP12        Pou(X)
185900020211     C                   add       1             X
186000020211     C                   endif
186100020215     C                   endif
186200020211     C                   endif
186300020211      *
186400020211     C     V1CP13        ifne      *zeros
186500020212     C     $Po999        andne     *on
186600020211     C     V1CP13        ifeq      999
186700020211     C                   move      *on           $Po999
186800020211     C                   z-add     1             X
186900020211     C                   else
187000020215     C                   z-add     1             Y                 2 0
187100020215     C                   move      v1cp13        $campo3
187200020215     C     $campo3       lookup    Pou(y)                                 21
187300020215     C     *in21         ifeq      *off
187400020212     C                   move      V1CP13        Pou(X)
187500020211     C                   add       1             X
187600020211     C                   endif
187700020215     C                   endif
187800020211     C                   endif
187900020211      *
188000020211     C     V1CP14        ifne      *zeros
188100020212     C     $Po999        andne     *on
188200020211     C     V1CP14        ifeq      999
188300020211     C                   move      *on           $Po999
188400020211     C                   z-add     1             X
188500020211     C                   else
188600020215     C                   z-add     1             Y                 2 0
188700020215     C                   move      v1cp14        $campo3
188800020215     C     $campo3       lookup    Pou(y)                                 21
188900020215     C     *in21         ifeq      *off
189000020212     C                   move      V1CP14        Pou(X)
189100020211     C                   add       1             X
189200020211     C                   endif
189300020215     C                   endif
189400020211     C                   endif
189500020211      *
189600020211     C     V1CP15        ifne      *zeros
189700020212     C     $Po999        andne     *on
189800020211     C     V1CP15        ifeq      999
189900020211     C                   move      *on           $Po999
190000020211     C                   z-add     1             X
190100020211     C                   else
190200020215     C                   z-add     1             Y                 2 0
190300020215     C                   move      v1cp15        $campo3
190400020215     C     $campo3       lookup    Pou(y)                                 21
190500020215     C     *in21         ifeq      *off
190600020212     C                   move      V1CP15        Pou(X)
190700020211     C                   add       1             X
190800020211     C                   endif
190900020215     C                   endif
191000020211     C                   endif
191100020211      *
191200020211     C     V1CP16        ifne      *zeros
191300020212     C     $Po999        andne     *on
191400020211     C     V1CP16        ifeq      999
191500020211     C                   move      *on           $Po999
191600020211     C                   z-add     1             X
191700020211     C                   else
191800020215     C                   z-add     1             Y                 2 0
191900020215     C                   move      v1cp16        $campo3
192000020215     C     $campo3       lookup    Pou(y)                                 21
192100020215     C     *in21         ifeq      *off
192200020212     C                   move      V1CP16        Pou(X)
192300020211     C                   add       1             X
192400020211     C                   endif
192500020215     C                   endif
192600020211     C                   endif
192700020211      *
192800020211     C     V1CP17        ifne      *zeros
192900020212     C     $Po999        andne     *on
193000020211     C     V1CP17        ifeq      999
193100020211     C                   move      *on           $Po999
193200020211     C                   z-add     1             X
193300020211     C                   else
193400020215     C                   z-add     1             Y                 2 0
193500020215     C                   move      v1cp17        $campo3
193600020215     C     $campo3       lookup    Pou(y)                                 21
193700020215     C     *in21         ifeq      *off
193800020212     C                   move      V1CP17        Pou(X)
193900020211     C                   add       1             X
194000020211     C                   endif
194100020215     C                   endif
194200020211     C                   endif
194300020211      *
194400020211     C     V1CP18        ifne      *zeros
194500020212     C     $Po999        andne     *on
194600020211     C     V1CP18        ifeq      999
194700020211     C                   move      *on           $Po999
194800020211     C                   z-add     1             X
194900020211     C                   else
195000020215     C                   z-add     1             Y                 2 0
195100020215     C                   move      v1cp18        $campo3
195200020215     C     $campo3       lookup    Pou(y)                                 21
195300020215     C     *in21         ifeq      *off
195400020212     C                   move      V1CP18        Pou(X)
195500020211     C                   add       1             X
195600020211     C                   endif
195700020215     C                   endif
195800020211     C                   endif
195900020211      *
196000020211     C     V1CP19        ifne      *zeros
196100020212     C     $Po999        andne     *on
196200020211     C     V1CP19        ifeq      999
196300020211     C                   move      *on           $Po999
196400020211     C                   z-add     1             X
196500020211     C                   else
196600020215     C                   z-add     1             Y                 2 0
196700020215     C                   move      v1cp19        $campo3
196800020215     C     $campo3       lookup    Pou(y)                                 21
196900020215     C     *in21         ifeq      *off
197000020212     C                   move      V1CP19        Pou(X)
197100020211     C                   add       1             X
197200020211     C                   endif
197300020211     C                   endif
197400020215     C                   endif
197500020211      *
197600020211     C                   ENDSR
197700020211      *--------------------------------------------------------------------------------------------*
197800020211      * WriTbe - Scrittura tabella
197900020211      *--------------------------------------------------------------------------------------------*
198000020211     C     WriTbe        BEGSR
198100020211      *
198200020211     C                   clear                   Tntbe01
198300020211     C                   move      ' '           TbeAtb
198400020211     C                   movel     $Sif          TbeSif
198500020211     C                   movel     $Lin          TbeLin
198600020211     C                   movel     $Apl          TbeApl
198700020211     C                   move      '3NN'         TbeCod
198800020211     C                   move      *blanks       TbeKe1
198900020211     C                   move      *blanks       TbeKe2
199000020211      *
199100020211     C                   move      V1ccod        $Campo7           7
199200020211     C                   movel     $Campo7       TbeKe1
199300020211      *
199400020215     C     v1ccli        ifne      *zeros
199500020211     C                   move      V1ccli        $Campo7           7
199600020211     C                   movel     $Campo7       TbeUni
199700020215     C                   else
199800020215     C                   movel     *blanks       TbeUni
199900020215     C                   endif
200000020215      *
200100020211     C                   movel     POU(X)        TbeKe2
200200020211      *
200300020211     C                   move      'S'           TbeFtt
200400020211     C                   z-add     *zeros        TbeFlt
200500020211     C                   move      ' '           TbeFtr
200600020211     C                   move      *zeros        TbeDtr
200700020211      *
200800020211     C                   write     Tntbe01
200900020211      *
201000020211     C                   ENDSR
201100020208      *--------------------------------------------------------------------------------------------*
201200020208      * F03ges - Tasto funzionale f03 -> Fine programma
201300020208      *--------------------------------------------------------------------------------------------*
201400020208     C     F03ges        BEGSR
201500020208      *
201600020208     C                   movel     'S'           $Fine                          *fine pgm
201700020208     C                   clear                   tntb22ds
201800020208     C                   move      'F03'         b22Fun
201900020208      *
202000020208     C                   ENDSR
202100020208      *--------------------------------------------------------------------------------------------*
202200020208      * F12ges - Tasto funzionale f12 -> Ritorno
202300020208      *--------------------------------------------------------------------------------------------*
202400020208     C     F12ges        BEGSR
202500020208      *
202600020208     C                   movel     'S'           $Fine                          *fine pgm
202700020208      *
202800020208     C                   ENDSR
202900020208      *--------------------------------------------------------------------------------------------*
203000020208      * DeCod  - Decodifica codice cliente
203100020208      *--------------------------------------------------------------------------------------------*
203200020208     C     DeCod         BEGSR
203300020208      *
203400020208     C                   clear                   tibs69ds
203500020208     C                   movel     $Kci          I69Kcc
203600020208     C                   movel     $Cod          I69Kac
203700020208     C                   movel     KnSif         I69Sif
203800020208     C                   call      'TIBS69R'
203900020208     C                   parm                    Tibs69Ds
204000020208     C                   parm                    DsAco
204100020208     C                   parm                    DsInd
204200020208     C                   parm                    DsClp
204300020208     C                   parm                    DsCls
204400020208      *
204500020208     C                   ENDSR
204600020212      *--------------------------------------------------------------------------------------------*
204700020212      * ChnTbe - Aggancio tabella per controllo esistenza record
204800020212      *--------------------------------------------------------------------------------------------*
204900020212     C     ChnTbe        BEGSR
205000020212     C                   setoff                                       23
205100020212     C     K01Tbe5       chain     Tntbe01l                           23
205200020212      *
205300020212      * provo anche con codice 999
205400020220     C** 23              movel     '999'         TbeKe2
205500020220     C** 23K01Tbe5       chain     Tntbe01l                           23
205600020212      *
205700020212     C                   ENDSR
205800020212      *--------------------------------------------------------------------------------------------*
205900020212      * CtrPos - Controllo clienti o poste o non  poste
206000020212      *--------------------------------------------------------------------------------------------*
206100020212     C     CtrPos        BEGSR
206200020212     C                   setoff                                       24
206300020212      * Controllo se poste o no il codice P.O
206400020212     C                   move      'N'           wcli1             1
206500020212     C     $Pou1         chain     azorg01l                           23
206600020212     C                   movel     Orgde3        og143
206700020215     C     §Ogntw        ifeq      'PPT'
206800020212     C                   move      'S'           Wcli1
206900020212     C                   else
207000020212     C                   move      'N'           Wcli1
207100020212     C                   endif
207200020212      *
207300020212      * Controllo se poste o no il cliente
207400020212     C                   move      'N'           wcli2             1
207500020214     C     $Pou2         chain     azorg01l                           23
207600020212     C                   movel     Orgde3        og143
207700020215     C     §Ogntw        ifeq      'PPT'
207800020212     C                   move      'S'           Wcli2
207900020212     C                   else
208000020212     C                   move      'N'           Wcli2
208100020212     C                   endif
208200020212      *
208300020212     C     Wcli1         ifne      Wcli2
208400020215     C     $pou2         andne     999
208500020212     C                   seton                                        24
208600020212     C                   endif
208700020212      *
208800020212     C                   ENDSR
208900020212      *--------------------------------------------------------------------------------------------*
209000020320      * CtrSpe-Controllo che il cliente non mantenga il num. spedizione
209100020212      *--------------------------------------------------------------------------------------------*
209200020212     C     CtrSpe        BEGSR
209300020212     C                   setoff                                       25
209400020320     C                   clear                   Ds3C
209500020212     C                   move      1             $Kut              1 0
209600020212     C                   move      '3C'          $Cod1             2
209700020212     C                   movel     *blanks       $Key              8
209800020212     C                   movel     $Campo7       $Key
209900020212     C     Ktabel        chain     Tabel00f                           21
210000020212     C                   movel     TblUni        Ds3C
210100020212     C     *in21         ifeq      *off
210200020212     C     §3CFsp        andeq     'S'
210300020212     C                   seton                                        25
210400020212     C                   endif
210500020212      *
210600020212     C                   ENDSR
210700020320      *--------------------------------------------------------------------------------------------*
210800020320      * CtrCdT-Controllo su cliente se il codice trattamento è tipo Disk 'B'
210900020320      *--------------------------------------------------------------------------------------------*
211000020320     C     CtrCdT        BEGSR
211100020320     C                   setoff                                       25
211200020320     C                   clear                   Ds1B
211300020320     C                   move      1             $Kut              1 0
211400020320     C                   move      '1B'          $Cod1             2
211500020320     C                   movel     *blanks       $Key              8
211600020320     C                   movel     §3CCtm        $Key
211700020320     C     Ktabel        chain     Tabel00f                           21
211800020320     C                   movel     TblUni        Ds1B
211900020320     C     *in21         ifeq      *off
212000020320     C     §1Bfg1        andeq     'S'
212100020320     C                   seton                                        25
212200020320     C                   endif
212300020320      *
212400020320     C                   ENDSR
212500020208      *--------------------------------------------------------------------------------------------*
212600020208      * RutInz - operazioni iniziali
212700020208      *--------------------------------------------------------------------------------------------*
212800020208     C     rutinz        BEGSR
212900020208      *
213000020208      * Ricevimento parametri
213100020208     C     *ENTRY        PLIST
213200020208     C                   PARM                    kpjba
213300020208     C                   PARM                    Tntb22Ds
213400020208      *
213500020208     C                   z-add     1             CodUt
213600020208     C                   CALL      'X§PARUT'
213700020208     C                   PARM                    UT§DSE
213800020208      *
213900020208      * Recupero capoconto
214000020208      *
214100020208     C                   Do        50            X                 2 0
214200020208     C                   move      TCU(X)        TCUDS
214300020208     C     F56           ifeq      'CG'
214400020208     C     F4            andeq     '1'
214500020208     C     F3            andeq     '0'
214600020208     C                   z-add     KCU(X)        $Kci              4 0
214700020208     C                   leave
214800020208     C                   endif
214900020208     C                   Enddo
215000020208      *
215100020208      * A seconda dell'opzione imposto la videata da gestire
215200020208     C     B22Fun        ifeq      'F10'
215300020208     C                   move      '1'           TipVid
215400020211     C                   move      '1'           $CarVd1
215500020208     C                   else
215600020208     C                   move      '2'           TipVid
215700020211     C                   move      '1'           $CarVd2
215800020208     C                   endif
215900020211      *
216000020211      * Reperisco testata tabella
216100020211     C                   movel     *blanks       Tbecod
216200020211     C                   move      *zeros        TbeKe1
216300020211     C                   move      '3NN'         TbeKe1
216400020211     C     K01Tbe2       chain     Tntbe01l                           21
216500020211      *
216600020211      * Se non esiste l atestata tabella non faccio nulla
216700020211     C   21              move      '1'           $Fine
216800020211      *
216900020211     C  n21              do
217000020211     C                   movel     TbeSif        $Sif             10
217100020211     C                   movel     TbeApl        $Apl              2
217200020211     C                   movel     TbeLin        $Lin              1
217300020215     C                   move      '3NN'         TbeCod
217400020211     C                   enddo
217500020211      *
217600020211     C                   EndSr
217700020211      *--------------------------------------------------------------------------------------------*
217800020208      *
217900020208      * chiavi di lettura
218000020208      *
218100020208     C     k02tbe4       KLIST                                                  *tntbe01l
218200020208     C                   KFLD                    tbecod                          -tabella
218300020208     C                   KFLD                    tbelin                          -chiave uno
218400020208     C                   KFLD                    tbesif                          -chiave due
218500020208     C                   KFLD                    tbeke1                          -lingua
218600020208      *
218700020208     C     k02tbe5       KLIST                                                  *tntbe01l
218800020208     C                   KFLD                    tbecod                          -tabella
218900020208     C                   KFLD                    tbelin                          -chiave uno
219000020208     C                   KFLD                    tbesif                          -chiave due
219100020208     C                   KFLD                    tbeke1                          -lingua
219200020208     C                   KFLD                    tbeke2                          -lingua
219300020208      *
219400020211     C     k01tbe2       KLIST                                                  *tntbe01l
219500020211     C                   KFLD                    tbecod                          -tabella
219600020211     C                   KFLD                    tbeke1                          -chiave uno
219700020212      *
219800020212     C     k01tbe5       KLIST                                                  *tntbe01l
219900020212     C                   KFLD                    TbeCod                          -tabella
220000020212     C                   KFLD                    tbeke1                          -chiave uno
220100020212     C                   KFLD                    tbeKe2                          -tabella
220200020212     C                   KFLD                    $lin                            -chiave uno
220300020212     C                   KFLD                    $sif                            -tabella
220400020212      *
220500020212     C     ktabel        KLIST                                                  *tntbe01l
220600020212     C                   KFLD                    $Kut                            -tabella
220700020212     C                   KFLD                    $Cod1                           -chiave uno
220800020212     C                   KFLD                    $key                            -tabella
220900020211      *
221000020208** Opz  Lungh. 15                                                            *
221100020208  Modifica
221200020208Visualizzazione
221300020208 Annullamneto
221400020208  Ripristino
221500020212** Msg
221600020212Immettere almeno un punto operativo che puo' confermare le bolle del cliente
221700020212Punto operativo inesistente o errato
221800020212P.O. già abilitato per cliente
221900020212P.O. 999 non valido se cliente mantiene Num. spedizione
222000020215Il P.O. del cliente e i P.O scelti devono essere POSTE o NON POSTE
222100020320P.O. 999 non valido se cliente ha codice trattamento tipo Disk 'B'
