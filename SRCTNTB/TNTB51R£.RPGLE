000100020208     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200020208      *--------------------------------------------------------------------------------------------*
000300051202      *     MANUTENZIONE TABELLA 3TR PER abilitazione trasferimento dati                           *
000400020208      *--------------------------------------------------------------------------------------------*
000500020208      * Data base                                                                                  *
000600020208      *--------------------------------------------------------------------------------------------*
000700051202     Ftntbe02l  UF A E           K DISK
000800051206     Ftntbe01l  IF   E           K DISK    rename(tntbe000:tntbe001)
000900020208     FAzOrg01l  IF   E           K DISK
001000051202     FTnTb51d   CF   E             WORKSTN
001100020208      *--------------------------------------------------------------------------------------------*
001200051202     D Opz             S             15    DIM(5) CTDATA PERRCD(1)              *Messaggi video
001300051214     D msg             S             78    DIM(14) CTDATA PERRCD(1)             *Messaggi video
001400051202     D skipo           S              3    DIM(10)
001500020208      *---
001600020208     D $Fine           S              1    INZ('N')                             *fine programma
001700020208     D TipVid          S              1    INZ('0')                             *video in gestione
001800020208     D $CarVd1         S              1    INZ('0')                             *video in gestione
001900020208     D $CarVd2         S              1    INZ('0')                             *video in gestione
002000020208      *---
002100020208      * Ds
002200020208      *---
002300051202     D Tntb50Ds      E DS                                                       *ds di procedura
002400051206     D D3TR          E DS                                                       *ds di procedura
002500051206     d
002600020208     D Tibs69Ds      E DS                                                       *ds di procedura
002700020208     D DsAco         E DS                  ExtName(CnAco00f)                    *ds di procedura
002800020208     D DsInd         E DS                  ExtName(CnInd00f)                    *ds di procedura
002900020208     D DsClp         E DS                  ExtName(CnClp00f)                    *ds di procedura
003000020208     D DsCls         E DS                  ExtName(FnCls00f)                    *ds di procedura
003100020208      *
003200020208     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003300020208     D  TCU                  398    697
003400020208     D                                     DIM(50)
003500020208     D  KCU                  698    847P 0
003600020208     D                                     DIM(50)
003700020208      *
003800020208     D TCUDS           DS
003900020208     D  F1                     1      1
004000020208     D  F3                     3      3
004100020208     D  F2                     2      2
004200020208     D  F4                     4      4
004300020208     D  F56                    5      6
004400020208      *
004500020208      *---
004600020208     D kpjba         E DS                                                       *architettura
004700020208      *--------------------------------------------------------------------------------------------*
004800020208      * Main lines
004900020208      *--------------------------------------------------------------------------------------------*
005000020208      *
005100020208      * Operazioni iniziali
005200020208      *
005300020208     C                   EXSR      RutInz
005400020208      *
005500020208      * Gestione video
005600020208      *
005700020208     C     $Fine         DOUEQ     'S'
005800020208     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile
005900020208     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile
006000051206     C     TipVid        CASEQ     'C'           GesCTR                          *Sfile
006100020208     C                   ENDCS
006200020208     C                   ENDDO
006300020208      *
006400020208     C                   SETON                                        LR
006500020208      *--------------------------------------------------------------------------------------------*
006600020208      * GesVd1 - Gestione videata immissione nuova tabella -
006700020208      *--------------------------------------------------------------------------------------------*
006800020208     C     GesVd1        BEGSR
006900020208      *
007000020208      * Inizializzazione videata
007100020208     C     $CarVd1       ifeq      *on
007200020208     C                   EXSR      CarVd1
007300020208     C                   move      *off          $CarVd1
007400020208     C                   endif
007500020208      *
007600051205     C                   EXFMT     Tb51v1
007700020208      *
007800020208      *
007900020208     C                   select
008000020208      * F3=Fine
008100020208     C     *inKc         wheneq    *on
008200020208     C                   EXSR      F03Ges
008300020208      *
008400020208     C     *inKl         wheneq    *on
008500051202     C                   movel     'S'           $Fine                          *fine pgm
008600020208     C                   other
008700020208      *
008800020208      * Controlli e decodifiche
008900020208     C                   EXSR      CtrV1
009000020208      *
009100020208     C     *IN28         ifeq      *off
009200020208     C     *inKf         andeq     *on
009300020208     C                   EXSR      GesAgg
009400020208     C                   endif
009500020208     C                   endsl
009600020208      *
009700020208     C                   endsr
009800020208      *--------------------------------------------------------------------------------------------*
009900020208      * CarVd1 - Carica primo video
010000020208      *--------------------------------------------------------------------------------------------*
010100020208     C     CarVd1        BEGSR
010200020208      *
010300051205     C                   clear                   Tb51v1
010400020208     C                   movel     Ragut         V1CRag           20
010500020208     C                   movel     Knmus         V1CUte           10
010600020208     C                   movel     KnSif         V1CSif           10
010700051202     C
010800051202     C                   IF        B50KE1<>*BLANKS OR B50KE2<>*BLANKS
010900051202     c                   clear                   tbeke1
011000051202     c                   clear                   tbeke2
011100051202     c                   movel     b50ke2        tbeke2
011200051202     c                   movel     b50ke1        tbeke1
011300051202     C
011400051202     C                   move      '3TR'         TbeCod
011500051202     C                   movel     b50lin        TbeLin
011600051202     C                   movel     b50sif        TbeSif
011700051202     C* ALLOCO RECORD
011800051202     c     ktbe5         chain     tntbe02l
011900051202     C                   ENDIF
012000051202     C
012100020214      * imposto il campo solo se valorizzato e in questo caso
012200020214      * proteggo il campo
012300051202     C     b50ke2        ifne      *blanks
012400020214     C                   seton                                        38
012500051202     C                   movel     b50ke2        $Cod
012600020214     C                   move      $Cod          V1CCod
012700020212      * decodifica
012800020212     C                   move      V1CCod        $Cod              7 0
012900020212     C                   EXSR      DeCod
013000020212      *
013100020212 2a  C                   if        o69err = *blanks and acoflg =' '
013200020212     C                   movel     acorag        V1DCod
013300020212 2e  C                   endif
013400051202 2e  C                   else
013500051202     c* P.o.
013600051205     c                   if        b50ke1>*zeros
013700051202     C                   movel     b50ke1        v1cpou
013800051202     C     v1cpou        chain     Azorg01l                           21
013900051202     c
014000051202     c  n21              movel     orgdes        v1dpou
014100051202     c                   seton                                        38
014200051202     c                   endif
014300051202     c                   endif
014400051206     c**
014500051206     c* P.o. di reso
014600051207     c                   if        b50ormreso>*zeros
014700051207     C                   movel     b50ormreso    v1cres
014800051206     C     v1cres        chain     Azorg01l                           21
014900051206     c
015000051206     c  n21              movel     orgdes        v1dres
015100051206     c                   endif
015200051202     c
015300051202     c* Carico i p.o.
015400051202     c                   movea     b50poa        skipo
015500051202     c                   z-add     1             xx                3 0
015600051202     c                   dow       skipo(xx)>*zeros
015700051202     c                   if        xx=1
015800051202     c                   movel     skipo(1)      v1cp01
015900051202     c                   endif
016000051202     c                   if        xx=2
016100051202     c                   movel     skipo(2)      v1cp02
016200051202     c                   endif
016300051202     c                   if        xx=3
016400051202     c                   movel     skipo(3)      v1cp03
016500051202     c                   endif
016600051202     c                   if        xx=4
016700051202     c                   movel     skipo(4)      v1cp04
016800051202     c                   endif
016900051202     c                   if        xx=5
017000051202     c                   movel     skipo(5)      v1cp05
017100051202     c                   endif
017200051202     c                   if        xx=6
017300051202     c                   movel     skipo(6)      v1cp06
017400051202     c                   endif
017500051202     c                   if        xx=7
017600051202     c                   movel     skipo(7)      v1cp07
017700051202     c                   endif
017800051202     c                   if        xx=8
017900051202     c                   movel     skipo(8)      v1cp08
018000051202     c                   endif
018100051202     c                   if        xx=9
018200051202     c                   movel     skipo(9)      v1cp09
018300051202     c                   endif
018400051202     c                   if        xx=10
018500051202     c                   movel     skipo(10)     v1cp10
018600051202     c                   endif
018700051202     c
018800051202     c                   add       1             xx
018900051202     c                   enddo
019000051202     c
019100051202      *
019200051202      * Imposto l'opzione
019300051202     C                   EVAL      V1COPZ=OPZ(5)
019400051202      *
019500051202      *  Modifica
019600051202     C     b50Opz        ifeq      '2'
019700051202     C                   eval      v1cOpz = Opz(1)
019800051202     C                   setoff                                       6063
019900051202     C                   endif
020000051202      *  Visualizzazione
020100051202     C     b50Opz        ifeq      '5'
020200051202     C                   seton                                        6063
020300051202     C                   eval      v1cOpz = Opz(2)
020400051202     C                   endif
020500051202      *  Annullamento
020600051202     C     b50Opz        ifeq      '4'
020700051202     C                   seton                                        61
020800051202     C                   setoff                                       63
020900051202     C                   eval      v1cOpz = Opz(3)
021000051202     C                   endif
021100051202      *  Ripristina
021200051202     C     b50Opz        ifeq      '7'
021300051202     C                   seton                                        61
021400051202     C                   setoff                                       63
021500051202     C                   eval      v1cOpz = Opz(4)
021600051202     C                   endif
021700020208      *
021800020208     C                   ENDSR
021900020208      *--------------------------------------------------------------------------------------------*
022000020208      * Controllo e decodifica prima videata
022100020208      *--------------------------------------------------------------------------------------------*
022200020208     C     CtrV1         BEGSR
022300020208      *
022400051202     C                   setoff                                       283739
022500051202     C                   setoff                                       445253
022600020208     C                   setoff                                       545556
022700020208     C                   setoff                                       575859
022800051202     C                   setoff                                       60
022900020208     C                   clear                   V1CMsg
023000020208      *
023100051205    0C     *in38         ifeq      *off
023200051205     c
023300020214      * Ricerca estesa cu codice cliente se non protetto
023400051205 1a 1C     v1dcod        ifne      *blanks
023500020214 1a  C     v1ccod        andeq     *zeros
023600020208      *
023700020208      * Ricerca estesa
023800051205    2c                   if        v1dcod<>*blanks
023900020208     C                   movel     ragut         paxdut           30
024000020214     C                   move      v1ccod        $campo7           7
024100020214     C                   movel(P)  $campo7       paxdmt           48
024200020208      *
024300020208     C                   z-add     0             paxsta
024400051202     C                   move      b50kci        paxccc            4 0
024500020208     C                   clear                   paxdit
024600020208     C                   z-add     1             paxnum
024700020208      *
024800020208     C                   CALL      'XALFA3BR'
024900020208     C                   PARM                    PAXDUT
025000020208     C                   PARM                    CODUT
025100020208     C                   PARM                    PAXDMT
025200020208     C                   PARM                    PAXCCC
025300020208     C                   PARM                    PAXSTA            1 0
025400020208     C                   PARM                    PAXFLR           90
025500020208     C                   PARM                    PAXDIT            3
025600020208     C                   PARM                    PAXNUM            2 0
025700020208     C                   PARM                    PAXKCM           80
025800020208     C                   PARM                    PAXKSM          140
025900020208     C                   PARM                    PAXKDM           60
026000020208      *
026100051205 2a 3C     PaxSta        ifgt      -1
026200020214     C                   movel     paxksm        $campo7
026300020214     C                   move      $campo7       v1ccod
026400020214     C                   movel     paxdmt        v1dcod
026500051205 2- 3C                   endif
026600020208      *
026700020208     C                   goto      Finesr
026800051205 1- 2C                   endif
026900051205 1- 1C                   endif
027000051202     c*
027100051202     c* Immettere il codice o il p.o.
027200051205    1c                   if        v1ccod<>0       and v1cpou<>0
027300051202     C                   seton                                        2839
027400051202     c                   movel     msg(8)        v1cmsg
027500051202     C                   goto      Finesr
027600051205    1c                   endif
027700051205     c* Immettere uno dei due
027800051205    1c                   if        v1ccod=0  and v1cpou=0
027900051205     C                   seton                                        2839
028000051205     c                   movel     msg(8)        v1cmsg
028100051205     C                   goto      Finesr
028200051205    1c                   endif
028300020208      *
028400020212      * Controllo validità codice
028500020214     C                   move      *blanks       v1dcod
028600051205 1a 1C     v1ccod        ifne      *zeros
028700020208      *
028800020214     C                   move      v1ccod        $Cod              7 0
028900020208     C                   EXSR      DeCod
029000020208      *
029100051205 2a 2C                   if        o69err = *blanks and acoflg =' '
029200020214     C                   movel     acorag        v1dcod
029300020212      *
029400051205 2ex2c                   else
029500020214     C                   seton                                        2839
029600051202     c                   movel     msg(7)        v1cmsg
029700020212     C                   goto      Finesr
029800051205 2- 2C                   endif
029900020208      *
030000051205 1- 1C                   endif
030100051202     c
030200051202     c* Controllo p.o.
030300051205    1c                   if        v1cpou<>0
030400051202     C     v1cpou        chain     Azorg01l                           21
030500051206    2C                   if        *in21  or orgfva<>' '   or
030600051206     c                             (orgfag<>'A' and orgfag<>'F')
030700051202     C                   seton                                        2837
030800051202     C                   movel     msg(2)        V1cmsg
030900051202     C                   goto      FineSr
031000051206    2c                   endif
031100051205    1c                   endif
031200051202     c
031300051202     c* Controllo record doppio
031400051202     c
031500051205    1c                   if        v1ccod<>0
031600051202     c                   clear                   tbeke1
031700051202     c                   clear                   tbeke2
031800051202     c                   movel     v1ccod        tbeke2
031900051202     c                   else
032000051202     c                   movel     v1cpou        tbeke1
032100051205    1c                   endif
032200051202     c
032300051202     C                   move      '3TR'         TbeCod
032400051202     C                   movel     b50lin        TbeLin
032500051202     C                   movel     b50sif        TbeSif
032600051202     c     ktbe5         chain     tntbe02l
032700051205    1c                   if        %found(tntbe02l)
032800051202     C                   seton                                        2839
032900051202     C                   movel     msg(3)        V1cmsg
033000051202     C                   goto      FineSr
033100051205    1c                   endif
033200051205    0c                   endif
033300051202     c
033400051206     c
033500051206     c* Controllo p.o. di reso se immesso
033600051207     c                   clear                   v1dres
033700051206    1c                   if        v1cres<>0
033800051206     C     v1cres        chain     Azorg01l                           21
033900051206    2C                   if        *in21  or orgfva<>' '   or
034000051206     c                             (orgfag<>'A' and orgfag<>'F')
034100051206     C                   seton                                        2836
034200051206     C                   movel     msg(12)       V1cmsg
034300051206     C                   goto      FineSr
034400051207     c                   else
034500051207     c                   movel     orgdes        v1dres
034600051206    2c                   endif
034700051206    1c                   endif
034800020211      *
034900020211      * Ci deve essere almeno un P.O diverso da zero
035000051205 1a 1C     v1cp01        ifeq      *zeros
035100020212     C     v1cp02        andeq     *zeros
035200020212     C     v1cp03        andeq     *zeros
035300020212     C     v1cp04        andeq     *zeros
035400020212     C     v1cp05        andeq     *zeros
035500020212     C     v1cp06        andeq     *zeros
035600020212     C     v1cp07        andeq     *zeros
035700020212     C     v1cp08        andeq     *zeros
035800020212     C     v1cp09        andeq     *zeros
035900020212     C     v1cp10        andeq     *zeros
036000020212      *
036100020215     C                   seton                                        2844
036200020212     C                   movel     msg(1)        V1cmsg
036300020212     C                   goto      FineSr
036400051205    1C                   endif
036500051205      *
036600020208      * Controllo P.O
036700051202     c                   clear                   es99              1
036800051202     c                   clear                   no99              1
036900051202     c
037000051205     c                   movel     v1cp01        wpo               3 0
037100051202     c                   exsr      CTRPO
037200051205    1c                   if        *in28
037300051202     c                   seton                                        44
037400051202     C                   goto      FineSr
037500051205    1c                   endif
037600051202     c
037700051202     c                   movel     v1cp02        wpo
037800051202     c                   exsr      CTRPO
037900051205    1c                   if        *in28
038000051202     c                   seton                                        52
038100051202     C                   goto      FineSr
038200051205    1c                   endif
038300020215      *
038400051202     c                   movel     v1cp03        wpo
038500051202     c                   exsr      CTRPO
038600051202     c                   if        *in28
038700051202     c                   seton                                        53
038800051202     C                   goto      FineSr
038900051202     c                   endif
039000051202      *
039100051202     c                   movel     v1cp04        wpo
039200051202     c                   exsr      CTRPO
039300051202     c                   if        *in28
039400051202     c                   seton                                        54
039500051202     C                   goto      FineSr
039600051202     c                   endif
039700051202      *
039800051202     c                   movel     v1cp05        wpo
039900051202     c                   exsr      CTRPO
040000051202     c                   if        *in28
040100051202     c                   seton                                        55
040200051202     C                   goto      FineSr
040300051202     c                   endif
040400051202      *
040500051202     c                   movel     v1cp06        wpo
040600051202     c                   exsr      CTRPO
040700051202     c                   if        *in28
040800051202     c                   seton                                        56
040900051202     C                   goto      FineSr
041000051202     c                   endif
041100051202      *
041200051202     c                   movel     v1cp07        wpo
041300051202     c                   exsr      CTRPO
041400051202     c                   if        *in28
041500051202     c                   seton                                        57
041600051202     C                   goto      FineSr
041700051202     c                   endif
041800051202      *
041900051202     c                   movel     v1cp08        wpo
042000051202     c                   exsr      CTRPO
042100051202     c                   if        *in28
042200051202     c                   seton                                        58
042300051202     C                   goto      FineSr
042400051202     c                   endif
042500051202      *
042600051202     c                   movel     v1cp09        wpo
042700051202     c                   exsr      CTRPO
042800051202     c                   if        *in28
042900051202     c                   seton                                        59
043000051202     C                   goto      FineSr
043100051202     c                   endif
043200051202      *
043300051202     c                   movel     v1cp10        wpo
043400051202     c                   exsr      CTRPO
043500051202     c                   if        *in28
043600051202     c                   seton                                        60
043700051202     C                   goto      FineSr
043800051202     c                   endif
043900051205     c
044000051205     c* Sed immesso 999 non ci deve essere un p.o. valido e viceversa
044100051205     c                   if        es99='S' and  no99='S'
044200051205     C                   seton                                        2844
044300051205     C                   movel     msg(5)        V1cmsg
044400051205     C                   goto      FineSr
044500051205    1C                   endif
044600051202      *
044700020212      *
044800020212      *
044900020208     C     FineSr        ENDSR
045000051202     c*
045100051202      *--------------------------------------------------------------------------------------------*
045200051202      *   CTRPO
045300051202      *--------------------------------------------------------------------------------------------*
045400051202     c     CTRPO         BEGSR
045500051205 1a 1C     wpo           ifne      *zeros
045600051205    2C     wpo           ifne      999
045700051202     C     wpo           chain     Azorg01l                           21
045800051206    2C                   if        *in21  or orgfva<>' '   or
045900051206     c                             (orgfag<>'A' and orgfag<>'F')
046000051202     C                   seton                                        28
046100051202     C                   movel     msg(2)        V1cmsg
046200051202     C                   goto      endctrpo
046300051206 2- 3C                   endif
046400051202     c                   eval      no99='S'
046500051205   x2c                   else
046600051202     c                   eval      es99='S'
046700051205    2C                   endif
046800051205    1c                   endif
046900051202     c     endctrpo      ENDSR
047000020208      *--------------------------------------------------------------------------------------------*
047100020215      * GesVd2 - Gestione seconda videata con opzioni: '2', '5', '4', '7'
047200020208      *--------------------------------------------------------------------------------------------*
047300020208     C     GesVd2        BEGSR
047400020208      *
047500020208      * Inizializzazione videata
047600020208     C     $CarVd2       ifeq      *on
047700051202     C                   EXSR      CarVd1
047800020208     C                   move      *off          $CarVd2
047900020208     C                   endif
048000051206     c
048100020208      *
048200051205     C                   EXFMT     Tb51v1
048300020208      *
048400020212      * Tasti funzione se non si è in visualizza
048500051202     C     b50Opz        ifne      '5'
048600020212      *
048700020212     C                   select
048800020212     C     *inKl         wheneq    *on
048900051202     C                   movel     'S'           $Fine                          *fine pgm
049000020212     C                   other
049100020212      *
049200020212      * Controlli solo se si è in modifica
049300051202     C     b50Opz        ifeq      '2'
049400051202     C                   EXSR      CtrV1
049500020212     C                   endif
049600020212     C                   endsl
049700020212     C                   endif
049800020208      *
049900051202     C     b50Opz        ifne      '5'
050000020208     C     *IN28         ifeq      *off
050100020212     C     *inKf         andeq     *on
050200020208     C                   EXSR      GesAgg
050300020212     C                   endif
050400020212     C                   else
050500020212     C                   move      'S'           $Fine
050600020212     C                   endif
050700020208      *
050800020208     C                   ENDSR
050900020208      *--------------------------------------------------------------------------------------------*
051000020208      * Scrittura/ Aggiornamenti
051100020208      *--------------------------------------------------------------------------------------------*
051200020208     C     GesAgg        BEGSR
051300020208      *
051400020211      *
051500020211      * Gestisco la scrittura per ogni punto operativo
051600020211      * Carico in schiera i P.O.
051700020211     C                   EXSR      CarPou
051800051202     C
051900020211      *
052000051202     C                   move      '3TR'         TbeCod
052100051202     C                   movel     b50lin        TbeLin
052200051202     C                   movel     b50sif        TbeSif
052300051202     c
052400051202     c                   if        v1ccod>0
052500051202     c                   movel(p)  v1ccod        tbeke2
052600051206     c                   movel     v1ccod        pocli             3 0
052700051202     c                   movel(p)  pocli         tbeke1
052800051202     c                   else
052900051202     c                   movel(p)  v1cpou        tbeke1
053000051202     c                   clear                   tbeke2
053100051202     c                   endif
053200051202     c
053300051202     c
053400051206     c                   movea     skipo         §3trpoabi
053500051206     c                   movel     v1cres        §3trporeso
053600051206     c                   movel(P)  d3tr          tbeuni
053700051202     c
053800051202     C                   move      'S'           TbeFtt
053900051202     C                   z-add     *zeros        TbeFlt
054000051202     C                   move      ' '           TbeFtr
054100051202     C  n38              move      *zeros        TbeDtr
054200051202      *
054300051206     C  n38              write     Tntbe000                             99
054400051206     c
054500051206     c                   if        *in99
054600051206     c* Significa che esiste annullato: lo chaino
054700051206     c     ktbe5         chain     tntbe02l
054800051206     c                   if        %found(tntbe02l)
054900051206     c                   movel(P)  d3tr          tbeuni
055000051206     c
055100051206     C                   move      'S'           TbeFtt
055200051206     C                   z-add     *zeros        TbeFlt
055300051206     C                   move      ' '           TbeFtr
055400051206     C                   move      ' '           Tbeatb
055500051206     c                   update    tntbe000
055600051206     c                   endif
055700051206     c                   endif
055800051202     c
055900020208      *
056000051202     c                   if        *in38
056100020208      *
056200020211 2a  C                   Select
056300020208      *
056400051202     C     b50opz        wheneq    '2'
056500020208     C                   update    tntbe000
056600020208      *
056700051202     C     b50opz        wheneq    '4'
056800020208     C                   move      'A'           TbeAtb
056900020208     C                   move      *blanks       TbeFtr
057000020208     C                   update    tntbe000
057100020208      *
057200051202     C     b50opz        wheneq    '7'
057300020208     C                   move      ' '           TbeAtb
057400020208     C                   move      *blanks       TbeFtr
057500020208     C                   update    tntbe000
057600020211 2-  C                   endsl
057700020211 1-  C                   endif
057800051202 c
057900020212     C                   movel     'S'           $Fine
058000020212      *
058100020208     C                   ENDSR
058200020211      *--------------------------------------------------------------------------------------------*
058300020211      * CarPou - Caricamento in schiera dei punti operativi
058400020211      *--------------------------------------------------------------------------------------------*
058500020211     C     CarPou        BEGSR
058600051202     C                   MOVEA     *ZEROS        SKIPO
058700051202     C                   Z-ADD     1             XX
058800051202     C
058900051202     C                   IF        V1CP01>0
059000051202     C                   MOVEL     V1CP01        SKIPO(XX)
059100051202     C                   ADD       1             XX
059200051202     C                   ENDIF
059300051202     C                   IF        V1CP02>0
059400051202     C                   MOVEL     V1CP02        SKIPO(XX)
059500051202     C                   ADD       1             XX
059600051202     C                   ENDIF
059700051202     C                   IF        V1CP03>0
059800051202     C                   MOVEL     V1CP03        SKIPO(XX)
059900051202     C                   ADD       1             XX
060000051202     C                   ENDIF
060100051202     C                   IF        V1CP04>0
060200051202     C                   MOVEL     V1CP04        SKIPO(XX)
060300051202     C                   ADD       1             XX
060400051202     C                   ENDIF
060500051202     C                   IF        V1CP05>0
060600051202     C                   MOVEL     V1CP05        SKIPO(XX)
060700051202     C                   ADD       1             XX
060800051202     C                   ENDIF
060900051202     C                   IF        V1CP06>0
061000051202     C                   MOVEL     V1CP06        SKIPO(XX)
061100051202     C                   ADD       1             XX
061200051202     C                   ENDIF
061300051202     C                   IF        V1CP07>0
061400051202     C                   MOVEL     V1CP07        SKIPO(XX)
061500051202     C                   ADD       1             XX
061600051202     C                   ENDIF
061700051202     C                   IF        V1CP08>0
061800051202     C                   MOVEL     V1CP08        SKIPO(XX)
061900051202     C                   ADD       1             XX
062000051202     C                   ENDIF
062100051202     C                   IF        V1CP09>0
062200051202     C                   MOVEL     V1CP09        SKIPO(XX)
062300051202     C                   ADD       1             XX
062400051202     C                   ENDIF
062500051202     C                   IF        V1CP10>0
062600051202     C                   MOVEL     V1CP10        SKIPO(XX)
062700051202     C                   ADD       1             XX
062800051202     C                   ENDIF
062900020211      *
063000020211     C                   ENDSR
063100020208      *--------------------------------------------------------------------------------------------*
063200020208      * F03ges - Tasto funzionale f03 -> Fine programma
063300020208      *--------------------------------------------------------------------------------------------*
063400020208     C     F03ges        BEGSR
063500020208      *
063600020208     C                   movel     'S'           $Fine                          *fine pgm
063700051202     C                   clear                   tntb50ds
063800051202     C                   move      'F03'         b50Fun
063900020208      *
064000020208     C                   ENDSR
064100020208      *--------------------------------------------------------------------------------------------*
064200020208      * DeCod  - Decodifica codice cliente
064300020208      *--------------------------------------------------------------------------------------------*
064400020208     C     DeCod         BEGSR
064500020208      *
064600020208     C                   clear                   tibs69ds
064700051202     C                   movel     b50kci        I69Kcc
064800020208     C                   movel     $Cod          I69Kac
064900020208     C                   movel     KnSif         I69Sif
065000020208     C                   call      'TIBS69R'
065100020208     C                   parm                    Tibs69Ds
065200020208     C                   parm                    DsAco
065300020208     C                   parm                    DsInd
065400020208     C                   parm                    DsClp
065500020208     C                   parm                    DsCls
065600020208      *
065700020208     C                   ENDSR
065800020208      *--------------------------------------------------------------------------------------------*
065900020208      * RutInz - operazioni iniziali
066000020208      *--------------------------------------------------------------------------------------------*
066100020208     C     rutinz        BEGSR
066200020208      *
066300020208      * Ricevimento parametri
066400020208     C     *ENTRY        PLIST
066500020208     C                   PARM                    kpjba
066600051202     C                   PARM                    Tntb50Ds
066700020208      *
066800020208     C                   z-add     1             CodUt
066900020208     C                   CALL      'X§PARUT'
067000020208     C                   PARM                    UT§DSE
067100051206      *
067200051206      * chiavi di lettura
067300051206      *
067400051206      *
067500051206     C     ktbe5         KLIST                                                  *tntbe01l
067600051206     C                   KFLD                    tbecod                          -tabella
067700051206     C                   KFLD                    tbelin                          -chiave uno
067800051206     C                   KFLD                    tbesif                          -chiave due
067900051206     C                   KFLD                    tbeke1                          -lingua
068000051206     C                   KFLD                    tbeke2                          -lingua
068100051206      *
068200051206     C     K01tbe2       KLIST                                                  *tntbe01l
068300051206     C                   KFLD                    tbecod                          -tabella
068400051206     C                   KFLD                    tbeke1                          -chiave uno
068500051206      *
068600020208      *
068700020208      * A seconda dell'opzione imposto la videata da gestire
068800051202     C     b50Fun        ifeq      'F10'
068900020208     C                   move      '1'           TipVid
069000020211     C                   move      '1'           $CarVd1
069100020208     C                   else
069200051206     c
069300051207     c                   if        b50opz='C' or b50opz='O'
069400051206     C                   move      'C'           TipVid
069500051206      *
069600051206      * Recupero S.I.
069700051206     C                   MOVEL     *blanks       tbecod                         *tabella
069800051206     C                   MOVEL     *all'0'       tbeke1                         *chiave uno
069900051206     C                   MOVE      '3TR'         tbeke1
070000051206     C     k01tbe2       chain     tntbe01l                           97
070100051206     C  N97              movel     tbesif        b50sif
070200051206     c                   else
070300051206     C                   move      '2'           TipVid
070400020211     C                   move      '1'           $CarVd2
070500020208     C                   endif
070600051206     C                   endif
070700020211      *
070800020211     C                   EndSr
070900051206      *--------------------------------------------------------------------------------------------*
071000051206      * GESCTR - Gestisto il controllo di un p.o. se abilitato
071100051206      *--------------------------------------------------------------------------------------------*
071200051206     C     GesCtr        BEGSR
071300051206     C                   movel     'S'           $Fine                          *fine pgm
071400051207     c
071500051207     c                   if        b50opz='C'
071600051207     c                   clear                   b50ormreso
071700051207     c                   endif
071800051206      *
071900051206     c                   clear                   tbeke1
072000051206     c                   clear                   tbeke2
072100051206     c                   movel     b50ke2        tbeke2
072200051206     c                   movel     b50ke1        tbeke1
072300051206     C
072400051206     C                   move      '3TR'         TbeCod
072500051206     C                   movel     b50lin        TbeLin
072600051206     C                   movel     b50sif        TbeSif
072700051206     C* ALLOCO RECORD
072800051206     c                   clear                   wesrecpo          1
072900051206     c     ktbe5         chain     tntbe02l
073000051206     c                   if        not %found(tntbe02l)  or
073100051206     c                             %found(tntbe02l) and tbeatb<>' '
073200051206     c                   clear                   tbeke2
073300051206     c                   eval      wesrecpo='S'
073400051206     c     ktbe5         chain     tntbe02l
073500051206     c                   endif
073600051206     c
073700051207    1c                   if        %found(tntbe02l) and tbeatb=' '
073800051206     c
073900051206     c* Se non trovato sono abilitati tutti per cui ok
074000051207     c                   movel     tbeuni        d3TR
074100051207     c                   movea     §3trpoabi     skipo
074200051206     c     '999'         lookup    skipo                                  30
074300051206    2c                   if        *in30
074400051206    3c                   if        wesrecpo='S'
074500051206     c                   eval      b50msg=msg(9)
074600051206     c                   else
074700051206     c                   eval      b50msg=msg(6)
074800051206    3c                   endif
074900051206     c
075000051206     c                   eval      b50err='1'
075100051206     c                   goto      endctr
075200051206   x2c                   else
075300051206     c* Vedo se presente il p.o.
075400051206     c                   movel     b50poa        w003a             3
075500051206     c     w003a         lookup    skipo                                  30
075600051206    3c                   if        not *in30
075700051206    4c                   if        wesrecpo='S'
075800051206     c                   eval      b50msg=msg(10)
075900051206     c                   else
076000051206     c                   eval      b50msg=msg(11)
076100051206    4c                   endif
076200051206     c
076300051206     c                   eval      b50err='1'
076400051206     c                   goto      endctr
076500051207     c
076600051207   x3c                   else
076700051207     c* Passo in output il p.o. orm/reso
076800051207    4c                   if        b50opz='C'
076900051207     c                   movel     §3trporeso    b50ormreso
077000051207    4c                   endif
077100051207     c
077200051207     c* Se opz='O' devo controllare il p.o. per orm/reso
077300051207    4c                   if        b50opz='O'
077400051207     c                   movel     §3trporeso    w003a             3
077500051207    5c                   if        b50ormreso<>w003a
077600051207     c                   eval      b50msg=msg(13)
077700051207     c                   eval      b50err='1'
077800051207     c                   goto      endctr
077900051207    5C                   endif
078000051214     c
078100051214    5c                   if        b50ormreso<=*zeros
078200051214     c                   eval      b50msg=msg(14)
078300051214     c                   eval      b50err='1'
078400051214     c                   goto      endctr
078500051214    5C                   endif
078600051214     c
078700051207    4C                   endif
078800051207    3C                   endif
078900051206    2C                   endif
079000051206     c
079100051207   x1c                   else
079200051207     c* Se devo anche controllare il p.o. per orm/reso sempre errore
079300051207     c*  se manca la tabella
079400051207    2c                   if        b50opz='O'
079500051207     c                   eval      b50msg=msg(13)
079600051207     c                   eval      b50err='1'
079700051207     c                   goto      endctr
079800051207    2c                   endif
079900051207     c
080000051206    1C                   ENDIF
080100051206     c
080200051206     c     endctr        endsr
080300020211      *--------------------------------------------------------------------------------------------*
080400020211      *
080500020208** Opz  Lungh. 15                                                            *
080600020208  Modifica
080700020208Visualizzazione
080800020208 Annullamneto
080900020208  Ripristino
081000051202  IMMISSIONE
081100020212** Msg
081200051215Immettere almeno un punto operativo abilitato alla ricezione dati             01
081300051206Punto Operativo inesistente                                                   02
081400051206Codice cliente o punto operativo già presente in tabella                      03
081500051206P.o. di ricezione dati deve essere diverso dal p.o. cliente                   04
081600051206Se immesso un P.O. abilitato, impossibile specificare anche 999-nessun P.O.   05
081700051215Nessun P.O. abilitato a ricevere i dati  di questo cliente                    05
081800051206Codice cliente inesistente                                                    07
081900051206Immettere il codice cliente o il punto operativo                              08
082000051215Nessun P.O. abilitato a ricevere i dati di questo P.Operativo                 09
082100051215Il P.O non è abilitato a ricevere i dati di questo P.Operativo                10
082200051215Il P.O non è abilitato a ricevere i dati di questo cliente                    11
082300051206Punto operativo per reso inesistente                                          12
082400051207Il p.o. ritiro è diverso dal p.o. previsto per il codice cliente              13
082500051214P.O. di ritiro non controllabile: uguale = 0                                  14
