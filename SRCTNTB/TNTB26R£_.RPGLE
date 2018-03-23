000100891106     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891106     F*--------------------------------------------------------------*
000300891106     F* GESTIONE TABELLA LIBRI IVA                                   *
000400891106     F*----------------------* USO DEGLI INDICATORI *----------------*
000500891106     F*  01        INSERIMENTO                                       *
000600891106     F*  02        VARIAZIONE                                        *
000700900604     F*  03        ANNULLATO
000800891106     F*  30        LIBRI IVA ERRATO                                  *
000900891106     F*  31        LIBRO VENDITE OBBLIGATORIO                        *
001000891106     F*  32        LIBRO ACQUISTI OBBLIGATORIO                       *
001100891106     F*  33        LIBRO VENDITE RETTIFICHE OBBLIGATORIO             *
001200891106     F*  34        LIBRO ACQUISTI RETTIFICHE OBBLIGATORIO            *
001300900420     F*  35        SE IL FLAG = 2 IL CODICE FILIALE è OBBLIGATORIO   *
001400900420     F*  36        CODICE ERRATO
001500891106     F*--------------------------------------------------------------*
001600020723     FTNTB26D   CF   E             WORKSTN
001700020723     FTNTBE01L  UF A E           K DISK
001800020723     Fanuni01l  IF   E           K DISK
001900020723     Fansif01l  IF   E           K DISK
002000891106     D*--------------------------------------------------------------*
002100891106     D*                        I N P U T                             *
002200891106     D*--------------------------------------------------------------*
002300020724     D DyIV          E DS
002400020723     D TIBS02DS      E DS
002500891106     D KPJBA         E DS
002600020723     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002700020723     D cncr80        E DS
002800020723     D §cod            S                   like(tbecod) inz('YIV')
002900020723     D §ke1            S                   like(tbeke1)
003000020723     D §ke2            S                   like(tbeke2)
003100020723     D LIBIV           S                   like(VLIV)
003200020723     D SOC             S                   like(VSOC)
003300020723     D                SDS
003400020723     D  VTCPGM                 1     10
003500971210     C******************************************************
003600020723     C                   do        *hival
003700891106     C                   SETOFF                                       79
003800891106     C                   MOVEL     KNMUS         PROF              3
003900891106     C     PROF          IFNE      'EDP'
004000891106     C                   SETON                                        79
004100891106     C                   END
004200020723     C                   MOVEL(P)  '?'           VLIV
004300020723     C                   MOVEL(P)  '?'           VSOC
004400891107     C     LIBIV         IFNE      *BLANKS
004500020723     C                   MOVEL     SOC           VSOC
004600020723     C                   MOVEL     LIBIV         VLIV
004700891107     C                   END
004800891106     C     INIZIO        TAG
004900891106     C                   EXFMT     TB04D1
005000020723     c* fine
005100020723     C                   if        *inkc
005200020723     c                   leave
005300020723     c                   end
005400020723     C* CONTROLLI 1° VIDEO
005500020723     C                   EXSR      CTR01
005600020723     C   99              GOTO      INIZIO
005700891106     C*
005800900604     C                   SETOFF                                       010203
005900891107     C                   EXSR      PULVID
006000020723     C*
006100020723     C     KTAB          CHAIN     Tntbe01L                           80
006200020723     C* INSERIMENTO
006300891106     C     *IN80         IFEQ      '1'
006400891106     C                   SETON                                        01        *IMMISSIONE
006500891106     C                   ELSE
006600020723     C*
006700900420     C                   EXSR      AGGVID
006800020723     C* RECORD ANNULLATO
006900020723     C     TBEATB        IFNE      ' '
007000900604     C                   SETON                                        03
007100900604     C                   ELSE
007200020723     C* MANUTENZIONE
007300891106     C                   SETON                                        02        *VARIAZIONE
007400891106     C                   END
007500900604     C                   END
007600020723     C* 2° VIDEO
007700891106     C     SU            TAG
007800891106     C                   EXFMT     TB04D2
007900020723     C   Kc              leave
008000020723     C   Kl              GOTO      INIZIO
008100891106     C**--- ANNULLAMENTO
008200900604     C   KQ              DO
008300020723     C                   MOVEL     'A'           TBEATB
008400020723     C                   UPDATE    TNTBE000
008500891106     C                   GOTO      INIZIO
008600891106     C                   END
008700900604     C** RIPRISTINO
008800900604     C   KE              DO
008900020723     C                   MOVEL     *BLANKS       TBEATB
009000020723     C                   UPDATE    Tntbe000
009100900604     C                   GOTO      INIZIO
009200900604     C                   END
009300020723     C* CONTROLLA 2° VIDEO
009400891106     C                   EXSR      CTRFMT
009500020723     C                   IF        *IN99 OR NOT *INKF
009600020723     C                   GOTO      SU
009700020723     C                   END
009800020723     C* SCRIVE/AGGIORNA RECORD
009900020723     C                   IF        *INKF
010000891106     C                   EXSR      SCRIVI
010100020723     C                   IF        LIBIV = ' '
010200020723     C                   GOTO      INIZIO
010300020723     C                   ELSE
010400020723     C                   LEAVE
010500020723     C                   END
010600020723     C                   END
010700020723     C                   enddo
010800891106     C                   SETON                                        LR
010900891106     C**------------------------
011000020723     C** CONTROLLO PRIMO VIDEO
011100891106     C**------------------------
011200020723     C     CTR01         BEGSR
011300020723     C                   SETOFF                                       99
011400020723     C*§  RICERCA società
011500020723     C     Vsoc          IFEQ      '?'
011600020723     c                   clear                   TIBS02DS
011700020723     c                   movel     'R'           t02mod
011800020723     c                   movel     knsif         t02sif
011900020723     C                   movel     §cod          t02cod
012000020723     C                   CALL      'TIBS02R'
012100020723     c                   parm                    KPJBA
012200020723     c                   parm                    TIBS02DS
012300020723      *
012400020723    2C                   IF        T02err = *blanks
012500020723     C                   movel     T02ke1        Vsoc
012600020723     C                   movel     T02ke2        Vliv
012700020723     C                   ENDIF
012800020723     C                   ENDIF
012900020723     C*§  RICERCA filiale
013000020723     C     VLIV          IFEQ      '?'
013100020723     C     Vsoc          andne     '?'
013200020723     c                   clear                   TIBS02DS
013300020723     c                   movel     'R'           t02mod
013400020723     c                   movel     knsif         t02sif
013500020723     c                   movel     vsoc          t02ke1
013600020723     C                   movel     §cod          t02cod
013700020723     C                   CALL      'TIBS02R'
013800020723     c                   parm                    KPJBA
013900020723     c                   parm                    TIBS02DS
014000020723      *
014100020723    2C                   IF        T02err = *blanks
014200020723     C                   movel     T02ke1        Vsoc
014300020723     C                   movel     T02ke2        Vliv
014400020723     C                   ENDIF
014500020723     C                   ENDIF
014600020723     C* CAMPI OBBLIGATORI
014700020723     C     VSOC          IFEQ      *BLANKS
014800020723     C     VSOC          OREQ      *ZEROS
014900020723     C                   SETON                                        2999
015000020723     C                   END
015100020723     C     VLIV          IFEQ      *BLANKS
015200020723     C     VLIV          OREQ      *ZEROS
015300020723     C                   SETON                                        3099
015400020723     C                   END
015500020723     C** IL CODICE società deve esistere
015600020723     C                   MOVEL     *BLANKS       DESsoc
015700020723     c     vsoc          chain     ansif01l                           29
015800020723     C                   IF        *IN29
015900020723     C                   SETON                                        99
016000020723     C                   ELSE
016100020723     c                   movel     sifdesbrev    dessoc
016200020723     C                   END
016300020723     C** IL CODICE società + filiale deve esistere
016400020723     C                   MOVEL(p)  VLIV          uniunita
016500020723     C                   MOVEL     *BLANKS       DESFIL
016600020723     c     kuni          chain     anuni01l                           30
016700020723     C  N30              MOVEL     UNIDESBREV    DESFIL
016800020723     C   30              SETON                                        99
016900020723     C*
017000020723     C                   ENDSR
017100020723     C**------------------------
017200020723     C** PULISCI CAMPI VIDEO
017300020723     C**------------------------
017400020723     C     PULVID        BEGSR
017500020724     c* proteggo i campi di trasmissione se la società non è 201
017600020724     c     vsoc          comp      '201'                              4545
017700020724     c*
017800891106     C                   Z-ADD     0             §IVLVF
017900891106     C                   Z-ADD     0             §IVLAQ
018000891106     C                   Z-ADD     0             §IVLCO
018100891106     C                   Z-ADD     0             §IVLVN
018200891106     C                   Z-ADD     0             §IVLAN
018300900420     C                   MOVEL     *BLANK        VFTT
018400900420     C                   Z-ADD     0             VFLT
018500020723     C                   MOVEL     *BLANK        TBEFTR
018600020723     C                   Z-ADD     0             TBEDTR
018700900420     C                   MOVEL     *BLANKS       DESFLT
018800020723     C                   MOVEL(p)  vsoc          §ke1
018900020723     C                   MOVEL(p)  vliv          §ke2
019000891106     C                   ENDSR
019100891106     C**------------------------
019200891106     C** CONTROLLI 2 VIDEATA
019300891106     C**------------------------
019400891106     C     CTRFMT        BEGSR
019500020723     C                   SETOFF                                       99
019600891106     C**--- LIBRO VENDITE
019700891106     C     §IVLVF        IFEQ      0
019800891106     C                   SETON                                        3199
019900891106     C                   GOTO      END1
020000891106     C                   END
020100891106     C**--- LIBRO ACQUISTI
020200891106     C     §IVLAQ        IFEQ      0
020300891106     C                   SETON                                        3299
020400891106     C                   GOTO      END1
020500891106     C                   END
020600891106     C**--- LIBRO VENDITE (RETTIFICHE)
020700891106     C     §IVLVN        IFEQ      0
020800891106     C                   SETON                                        3399
020900891106     C                   GOTO      END1
021000891127     C                   END
021100891106     C**--- LIBRO ACQUISTI (RETTIFICHE)
021200891106     C     §IVLAN        IFEQ      0
021300891106     C                   SETON                                        3499
021400891106     C                   GOTO      END1
021500891106     C                   END
021600900420     C**--- FLAG DI TRASMISSIONE
021700020723     C     VFTT          IFEQ      'S'
021800900420     C     VFLT          ANDEQ     0
021900900420     C                   SETON                                        3599
022000900420     C                   GOTO      END1
022100900420     C                   END
022200020723     C     VFTT          IFNE      'S'
022300900420     C     VFLT          ANDGT     0
022400900420     C                   SETON                                        3699
022500900420     C                   GOTO      END1
022600900420     C                   END
022700900420     C     VFLT          IFGT      0
022800020723     C                   MOVEL(p)  VFLT          uniunita
022900020723     C                   MOVEL     *BLANKS       DESFLT
023000020723     c     kuni          chain     anuni01l                           36
023100020723     C  N36              MOVEL     UNIDESBREV    DESFLT
023200020723     C   36              SETON                                        99
023300900420     C                   END
023400891106     C     END1          ENDSR
023500020723     C**----------------------------
023600020723     C** AGGIORNAMENTO / INSERIMENTO
023700020723     C**----------------------------
023800020723     C     SCRIVI        BEGSR
023900020723     C     KTAB          CHAIN     Tntbe01L                           80
024000020723     C                   MOVEL     §COD          TBEcod
024100020723     C                   MOVEL     *BLANKS       TBeke1
024200020723     C                   MOVEL     *BLANKS       TBeke2
024300020723     C                   MOVEL     *BLANKS       TBEUNI
024400020723     C                   MOVEL     §KE1          TBEKe1
024500020723     C                   MOVEL     §KE2          TBEKe2
024600020724     C                   MOVEL     DyIV          TBEUNI
024700900420     C*§
024800020723     C                   MOVE      VFTT          TBEFTT                          DA VIDEO
024900020723     C                   Z-ADD     VFLT          TBEFLT                          FILIALE
025000020723     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
025100020723     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
025200900420     C*§
025300020723     C   80              WRITE     TNTBE000                             98
025400020723     C  N80              UPDATE    TNTBE000                             98
025500891106     C                   ENDSR
025600900420     C**----------------------------
025700900420     C** AGGIORNO IL VIDEO CON I
025800900420     C** CAMPI DEL FILE
025900900420     C**----------------------------
026000900420     C     AGGVID        BEGSR
026100020724     C                   MOVEL     TBEUNI        DyIV
026200020723     C                   MOVEL     TBEFTT        VFTT
026300020723     C                   Z-ADD     TBEFLT        VFLT
026400900420     C     VFLT          IFGT      0
026500020723     C                   MOVEL(p)  VFLT          uniunita
026600020723     c     kuni          chain     anuni01l                           36
026700020723     c                   movel     unidesbrev    desfLT
026800900420     C                   END
026900900420     C                   ENDSR
027000020723     C******************************************************
027100020723     c     *inzsr        begsr
027200020723     C     *ENTRY        PLIST
027300020723     C                   PARM                    KPJBA
027400020723     C                   EVAL      SOC = %SUBST(KPJBU: 1: 3)
027500020723     C                   EVAL      LIBIV = %SUBST(KPJBU: 4: 3)
027600020723     C                   CALL      'X§PARUT'
027700020723     C                   PARM                    UT§DSE
027800020723     C                   MOVEL     RAGUT         RSUT
027900020723     C* POSIZIONAMENTO TABELLE                             *
028000020723     C     KTAB          KLIST                                                  *
028100020723     C                   KFLD                    §COD                           *
028200020723     C                   KFLD                    §KE1                           *
028300020723     c                   KFLD                    §KE2                           *
028400020723     C     Kuni          KLIST
028500020723     C                   KFLD                    vsoc
028600020723     C                   KFLD                    UNIUNITA
028700020723     c                   endsr
