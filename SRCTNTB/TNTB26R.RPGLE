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
008400020726     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
008500020726     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
008600020723     C                   UPDATE    TNTBE000
008700891106     C                   GOTO      INIZIO
008800891106     C                   END
008900900604     C** RIPRISTINO
009000900604     C   KE              DO
009100020723     C                   MOVEL     *BLANKS       TBEATB
009200020726     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
009300020726     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
009400020723     C                   UPDATE    Tntbe000
009500900604     C                   GOTO      INIZIO
009600900604     C                   END
009700020723     C* CONTROLLA 2° VIDEO
009800891106     C                   EXSR      CTRFMT
009900020723     C                   IF        *IN99 OR NOT *INKF
010000020723     C                   GOTO      SU
010100020723     C                   END
010200020723     C* SCRIVE/AGGIORNA RECORD
010300020723     C                   IF        *INKF
010400891106     C                   EXSR      SCRIVI
010500020723     C                   IF        LIBIV = ' '
010600020723     C                   GOTO      INIZIO
010700020723     C                   ELSE
010800020723     C                   LEAVE
010900020723     C                   END
011000020723     C                   END
011100020723     C                   enddo
011200891106     C                   SETON                                        LR
011300891106     C**------------------------
011400020723     C** CONTROLLO PRIMO VIDEO
011500891106     C**------------------------
011600020723     C     CTR01         BEGSR
011700020723     C                   SETOFF                                       99
011800020723     C*§  RICERCA società
011900020723     C     Vsoc          IFEQ      '?'
012000020723     c                   clear                   TIBS02DS
012100020723     c                   movel     'R'           t02mod
012200020723     c                   movel     knsif         t02sif
012300020723     C                   movel     §cod          t02cod
012400020723     C                   CALL      'TIBS02R'
012500020723     c                   parm                    KPJBA
012600020723     c                   parm                    TIBS02DS
012700020723      *
012800020723    2C                   IF        T02err = *blanks
012900020723     C                   movel     T02ke1        Vsoc
013000020723     C                   movel     T02ke2        Vliv
013100020723     C                   ENDIF
013200020723     C                   ENDIF
013300020723     C*§  RICERCA filiale
013400020723     C     VLIV          IFEQ      '?'
013500020723     C     Vsoc          andne     '?'
013600020723     c                   clear                   TIBS02DS
013700020723     c                   movel     'R'           t02mod
013800020723     c                   movel     knsif         t02sif
013900020723     c                   movel     vsoc          t02ke1
014000020723     C                   movel     §cod          t02cod
014100020723     C                   CALL      'TIBS02R'
014200020723     c                   parm                    KPJBA
014300020723     c                   parm                    TIBS02DS
014400020723      *
014500020723    2C                   IF        T02err = *blanks
014600020723     C                   movel     T02ke1        Vsoc
014700020723     C                   movel     T02ke2        Vliv
014800020723     C                   ENDIF
014900020723     C                   ENDIF
015000020723     C* CAMPI OBBLIGATORI
015100020723     C     VSOC          IFEQ      *BLANKS
015200020723     C     VSOC          OREQ      *ZEROS
015300020723     C                   SETON                                        2999
015400020723     C                   END
015500020723     C     VLIV          IFEQ      *BLANKS
015600020723     C     VLIV          OREQ      *ZEROS
015700020723     C                   SETON                                        3099
015800020723     C                   END
015900020723     C** IL CODICE società deve esistere
016000020723     C                   MOVEL     *BLANKS       DESsoc
016100020723     c     vsoc          chain     ansif01l                           29
016200020723     C                   IF        *IN29
016300020723     C                   SETON                                        99
016400020723     C                   ELSE
016500020723     c                   movel     sifdesbrev    dessoc
016600020723     C                   END
016700020723     C** IL CODICE società + filiale deve esistere
016800020723     C                   MOVEL(p)  VLIV          uniunita
016900020723     C                   MOVEL     *BLANKS       DESFIL
017000020723     c     kuni          chain     anuni01l                           30
017100020723     C  N30              MOVEL     UNIDESBREV    DESFIL
017200020723     C   30              SETON                                        99
017300020723     C*
017400020723     C                   ENDSR
017500020723     C**------------------------
017600020723     C** PULISCI CAMPI VIDEO
017700020723     C**------------------------
017800020723     C     PULVID        BEGSR
017900020724     c* proteggo i campi di trasmissione se la società non è 201
018000020724     c     vsoc          comp      '201'                              4545
018100020724     c*
018200891106     C                   Z-ADD     0             §IVLVF
018300891106     C                   Z-ADD     0             §IVLAQ
018400891106     C                   Z-ADD     0             §IVLCO
018500891106     C                   Z-ADD     0             §IVLVN
018600891106     C                   Z-ADD     0             §IVLAN
018700150204     C                   Z-ADD     0             §IVLAR
018800900420     C                   MOVEL     *BLANK        VFTT
018900900420     C                   Z-ADD     0             VFLT
019000020723     C                   MOVEL     *BLANK        TBEFTR
019100020723     C                   Z-ADD     0             TBEDTR
019200900420     C                   MOVEL     *BLANKS       DESFLT
019300020723     C                   MOVEL(p)  vsoc          §ke1
019400020723     C                   MOVEL(p)  vliv          §ke2
019500891106     C                   ENDSR
019600891106     C**------------------------
019700891106     C** CONTROLLI 2 VIDEATA
019800891106     C**------------------------
019900891106     C     CTRFMT        BEGSR
020000020723     C                   SETOFF                                       99
020100891106     C**--- LIBRO VENDITE
020200891106     C     §IVLVF        IFEQ      0
020300891106     C                   SETON                                        3199
020400891106     C                   GOTO      END1
020500891106     C                   END
020600891106     C**--- LIBRO ACQUISTI
020700891106     C     §IVLAQ        IFEQ      0
020800891106     C                   SETON                                        3299
020900891106     C                   GOTO      END1
021000891106     C                   END
021100891106     C**--- LIBRO VENDITE (RETTIFICHE)
021200891106     C     §IVLVN        IFEQ      0
021300891106     C                   SETON                                        3399
021400891106     C                   GOTO      END1
021500891127     C                   END
021600891106     C**--- LIBRO ACQUISTI (RETTIFICHE)
021700891106     C     §IVLAN        IFEQ      0
021800891106     C                   SETON                                        3499
021900891106     C                   GOTO      END1
022000891106     C                   END
022100150204     C**--- LIBRO ACQUISTI (REV.CHARGE)
022200150204     C     §IVLAR        IFEQ      0
022300150204     C                   SETON                                        3799
022400150204     C                   GOTO      END1
022500150204     C                   END
022600900420     C**--- FLAG DI TRASMISSIONE
022700020723     C     VFTT          IFEQ      'S'
022800900420     C     VFLT          ANDEQ     0
022900900420     C                   SETON                                        3599
023000900420     C                   GOTO      END1
023100900420     C                   END
023200020723     C     VFTT          IFNE      'S'
023300900420     C     VFLT          ANDGT     0
023400900420     C                   SETON                                        3699
023500900420     C                   GOTO      END1
023600900420     C                   END
023700900420     C     VFLT          IFGT      0
023800020723     C                   MOVEL(p)  VFLT          uniunita
023900020723     C                   MOVEL     *BLANKS       DESFLT
024000020723     c     kuni          chain     anuni01l                           36
024100020723     C  N36              MOVEL     UNIDESBREV    DESFLT
024200020723     C   36              SETON                                        99
024300900420     C                   END
024400891106     C     END1          ENDSR
024500020723     C**----------------------------
024600020723     C** AGGIORNAMENTO / INSERIMENTO
024700020723     C**----------------------------
024800020723     C     SCRIVI        BEGSR
024900020723     C     KTAB          CHAIN     Tntbe01L                           80
025000020723     C                   MOVEL     §COD          TBEcod
025100020723     C                   MOVEL     *BLANKS       TBeke1
025200020723     C                   MOVEL     *BLANKS       TBeke2
025300020723     C                   MOVEL     *BLANKS       TBEUNI
025400020723     C                   MOVEL     §KE1          TBEKe1
025500020723     C                   MOVEL     §KE2          TBEKe2
025600020724     C                   MOVEL     DyIV          TBEUNI
025700900420     C*§
025800020723     C                   MOVE      VFTT          TBEFTT                          DA VIDEO
025900020723     C                   Z-ADD     VFLT          TBEFLT                          FILIALE
026000020723     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
026100020723     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
026200900420     C*§
026300020723     C   80              WRITE     TNTBE000                             98
026400020723     C  N80              UPDATE    TNTBE000                             98
026500891106     C                   ENDSR
026600900420     C**----------------------------
026700900420     C** AGGIORNO IL VIDEO CON I
026800900420     C** CAMPI DEL FILE
026900900420     C**----------------------------
027000900420     C     AGGVID        BEGSR
027100020724     C                   MOVEL     TBEUNI        DyIV
027200020723     C                   MOVEL     TBEFTT        VFTT
027300020723     C                   Z-ADD     TBEFLT        VFLT
027400900420     C     VFLT          IFGT      0
027500020723     C                   MOVEL(p)  VFLT          uniunita
027600020723     c     kuni          chain     anuni01l                           36
027700020723     c                   movel     unidesbrev    desfLT
027800900420     C                   END
027900900420     C                   ENDSR
028000020723     C******************************************************
028100020723     c     *inzsr        begsr
028200020723     C     *ENTRY        PLIST
028300020723     C                   PARM                    KPJBA
028400020723     C                   EVAL      SOC = %SUBST(KPJBU: 1: 3)
028500020723     C                   EVAL      LIBIV = %SUBST(KPJBU: 4: 3)
028600020723     C                   CALL      'X§PARUT'
028700020723     C                   PARM                    UT§DSE
028800020723     C                   MOVEL     RAGUT         RSUT
028900020723     C* POSIZIONAMENTO TABELLE                             *
029000020723     C     KTAB          KLIST                                                  *
029100020723     C                   KFLD                    §COD                           *
029200020723     C                   KFLD                    §KE1                           *
029300020723     c                   KFLD                    §KE2                           *
029400020723     C     Kuni          KLIST
029500020723     C                   KFLD                    vsoc
029600020723     C                   KFLD                    UNIUNITA
029700020723     c                   endsr
