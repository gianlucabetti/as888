000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300000309     fFNDCT00F  IP   E           K DISK
000400000309     fFNDPT01L  IF   E           k DISK
000500000309     fFNLBL02L  IF   E           K DISK
000600990409     fFNARB01L  IF   E           K DISK
000700990409     fFNBLP01L  IF   E           K DISK
000800000309     fFIAR601L  IF   E           K DISK
000900990412     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
001000980611      * ---------------------------------------------------------
001100971016     D L1              S              3  0 DIM(30)                              P.O.GESTITI
001200980717
001300990407     D FIDN48DS      E DS
001400971030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500980727     d  tcu                  398    697    DIM(50)
001600980611     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
001700981020     D TRUL06DS      E DS
001800980611     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
001900981020     D CNCR80        E DS
002000981020     D KPJBA         E DS
002100981228     D DDCT01        E DS
002200981029
002300980610     d                 ds                  INZ
002400980727     d  DS_arbdat              1      8  0
002500980525     d  arbaas                 1      4  0
002600980525     d  arbmgs                 5      8  0
002700980615
002800980513     d TCUDS           DS
002900980513     d  f1                     1      1
003000980513     d  f3                     3      3
003100980513     d  f2                     2      2
003200980513     d  f4                     4      4
003300980513     d  f56                    5      6
003400981020
003500980615
003600981214     D wisksc          S                   LIKE(ARBksc)
003700981029     D wisctr          S                   LIKE(ARBctr)
003800981230     D W§dctport       S                   LIKE(§DCTport)
003900000309     d TRCAR6          s                   LIKE(AR6TRC)
004000990409     D wtpb            s              1
004100990210     D w0040           s              4  0
004200980708     D JJ              s              3  0
004300980625      * ---------------------------------------------------------
004400971016     IFNBLP000
004500971016     I              BLPAAS                      ARBAAS
004600971016     I              BLPLNP                      ARBLNP
004700971016     I              BLPNRS                      ARBNRS
004800971016     I              BLPNSP                      ARBNSP
004900971016     I              BLPMGS                      ARBMGS
005000990409     I              BLPTSP                      ARBTSP
005100971016     I              BLPKSC                      ARBKSC
005200990409     I              BLPCTR                      ARBCTR
005300971016     I              BLPCCA                      ARBCCA
005400980625      * ---------------------------------------------------------
005500990409     c                   clear                   wtpb
005600990409     c     kbol01        chain(N)  fnarb000                           30
005700990409     c                   IF        *IN30 = *OFF
005800990409     c                   eval      Wtpb = 'A'
005900990409     c                   ELSE
006000990429     c     kbol01        chain(N)  fnblp000                           30
006100990409     c  N30              eval      Wtpb = 'P'
006200990409     c                   ENDIF
006300990409      *
006400990409     c                   IF        Wtpb <> *blanks
006500990409      *
006600990409     c     DCTfil        lookup    L1                                     30
006700990409      * calcolo mandato solo per le C.A. aperte da me
006800990409     c                   IF        *IN30 = *ON
006900990409     c     kca           chain     fndpt000                           30
007000990412     c   30              clear                   fndpt000
007100990412     c   30              exsr      ctr_cliass
007200990409     c                   ENDIF
007300990409      *
007400000309     c                   IF        O48mand = 'S'
007500990412     c   OF              except    TESTA
007600990412     c                   eval      *inof = *off
007700990412     c                   except    DETTA
007800000309     c                   endif
007900990412      *
008000990409     c                   ENDIF
008100990409      **************************************************************************
008200990409      * VERIFICA ESISTENZA MANDATO ASSICURATIVO
008300990409      **************************************************************************
008400990409     C     CTR_CLIASS    BEGSR
008500000309      *
008600000309     C                   MOVEL     DCTFLO        DDCT01
008700000309     c                   clear                   FIDN48DS
008800990409
008900990409      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con quello C.A. tranne se la
009000990409      *  sped. è "Mamma di cambio porto", in questo caso inverto il porto C.A.
009100990409     c                   SELECT
009200990409     c                   WHEN      ARBcca <> '9'
009300990409     c                   movel     §DCTport      W§DCTport
009400990409     c                   WHEN      §DCTport = 'A'
009500990409     c                   movel     'F'           W§DCTport
009600990409     c                   WHEN      §DCTport = 'F'
009700990409     c                   movel     'A'           W§DCTport
009800990409     c                   ENDSL
009900990409      *
010000990409      * Imposto Codice Cliente Intestatario Spedizione (Franco=Mittente - Assegnato=Destinatario)
010100990409     c                   z-add     ARBksc        WISksc
010200990409     c                   z-add     ARBctr        WISctr
010300990409      *
010400990409      *   Se sono in Arrivo e la sped è Mamma di cambio porto prendo il codice dalla figlia
010500990409     c                   IF        Wtpb = 'A'  and  ARBcca = '9'
010600990409     c     Kbol01        chain(N)  fnlbl000                           30
010700990409     c                   If        *IN30 = *off
010800990409     c     Klbl_F        chain(N)  fnarb000                           30
010900990409     c                   If        *IN30 = *off
011000990409     c                   z-add     ARBksc        WISksc
011100990409     c                   z-add     ARBctr        WISctr
011200990409     c     kbol01        chain(N)  fnarb000                           30
011300990409     c                   Endif
011400990409     c                   Endif
011500990409     c                   ELSE
011600990409      *
011700990409      *   Se assegnato in partenza verifico il codice destinatario
011800990409     c                   IF        Wtpb = 'P'  and  w§DCTport = 'A'
011900000309     C                   EVAL      TRCAR6 = '1'
012000000309     c     Kar6          chain(N)  FIAR6000                           30
012100990409     c                   If        *in30 = *OFF
012200000309     c                   If        AR6ksc <> *ZEROS   AND  AR6ctr <> *zeros
012300000309     c                   movel     AR6ksc        WISksc
012400000309     c                   movel     AR6ctr        WISctr
012500990409     c                   Endif
012600990409     c                   Endif
012700990409     c                   ENDIF
012800990409      *
012900990409     c                   ENDIF
013000990409
013100990409     c                   clear                   FIDN48DS
013200990409
013300990409      *  Clienti 8888 e 9999 sono senza mandato ed elevazione
013400990409     c                   move      WISksc        w0040
013500990409     c                   IF          W0040 <> 9999  AND  W0040 <> 8888
013600990409      *
013700990409     c                   movel     'L'           I48tla
013800990409     c                   z-add     WISksc        I48ksc
013900990409     c                   z-add     WISctr        I48ctr
014000990409     c                   movel     ARBtsp        I48tsp
014100990409     c                   movel     DS_arbdat     I48dsp
014200990409     c                   IF        §DCTtisp = *blanks
014300990409     c                   movel     'I'           I48fie
014400990409     c                   ELSE
014500990409     c                   movel     'E'           I48fie
014600990409     c                   ENDIF
014700990409     c                   movel     §DCTport      I48por
014800990409     c                   movel     'S'           I48mns
014900990409
015000990409     c                   CALL      'FIDN48R'
015100990409     C                   PARM                    KPJBA
015200990409     C                   PARM                    FIDN48DS
015300990409
015400990409     c                   IF        O48mand = 'S'
015500990409
015600990409     c                   z-add     dctaac        dptaac
015700990409     c                   z-add     dctfil        dptfil
015800990409     c                   z-add     dctnca        dptnca
015900990409     c                   movel     KCI           dptkcc
016000990409     c                   z-add     I48ksc        dptksc
016100990409     c                   z-add     I48ctr        dptctr
016200990409     c                   z-add     O48prg        dptprg
016300990409     c                   movel     O48ftc        dptftc
016400990409     c                   movel     O48fvm        dptfvm
016500990409     c                   z-add     O48vlm        dptvlm
016600990409     c                   movel     O48vvm        dptvvm
016700990409     c                   z-add     O48vlm        dptvlm
016800000309     c*                  write     FNDPT000
016900990409     c                   ENDIF
017000990409     c                   ENDIF
017100990409      *
017200990409     C                   ENDSR
017300980722      *****************************************************************
017400980623      *  ROUTINE INIZIALE
017500980722      *****************************************************************
017600951009     C     *INZSR        BEGSR
017700980722
017800990412     c                   eval      *inof = *on
017900990412
018000981126      * Dati società
018100951009     C                   Z-ADD     1             CODUT
018200951009     C                   CALL      'X§PARUT'
018300951009     C                   PARM                    UT§DSE
018400980611      *
018500971016     C                   MOVEL     REC80         CNCR80
018600980611      *
018700980611      *--- RICERCA CAPOCONTI
018800990409     C                   DO        50            jj
018900990409     C                   MOVE      TCU(jj)       TCUDS
019000980513     C     f56           CABNE     'CG'          END1
019100980513     C     f4            COMP      '1'                                    21
019200980513     C     f4            COMP      '2'                                    22
019300980513     C     f4            COMP      '3'                                    23
019400980513     C     f4            COMP      '6'                                    27
019500980513     C** 1 CLIENTI   21
019600980513     C** 2 FORNITORI 22
019700980513     C** 3 AGENTI    23
019800980513     C     f3            comp      '0'                                242425
019900980513     C     f3            comp      'I'                                    26
020000980513     C** 0 ITALIA   25
020100980513     C** 1 ESTERO   24
020200980513     ** I CAPO CONTO IVA
020300980513     c                   if        *in21 = *on
020400990409     C   24              Z-ADD     KCU(jj)       KCE               4 0
020500990409     C   25              Z-ADD     KCU(jj)       KCI               4 0
020600980513     c                   end
020700980513     c                   if        *in22 = *on
020800990409     C   24              Z-ADD     KCU(jj)       KFE               4 0
020900990409     C   25              Z-ADD     KCU(jj)       KFI               4 0
021000980513     c                   end
021100980513     c                   if        *in23 = *on
021200990409     C   24              Z-ADD     KCU(jj)       KAE               4 0
021300990409     C   25              Z-ADD     KCU(jj)       KAI               4 0
021400980513     c                   end
021500990409     C   26              Z-ADD     KCU(jj)       KIVA              4 0
021600990409     C   27              Z-ADD     KCU(jj)       KBNA              4 0
021700980513     C     end1          tag
021800980513     C                   end
021900981229     C                   SETOFF                                       212223
022000981229     C                   SETOFF                                       242526
022100981229     C                   SETOFF                                       27
022200980619      ***
022300981005      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1  se non sono in sede
022400980619      ***
022500981005     C                   if        SIMFEL <> 0
022600981020     C                   CLEAR                   TRUL06DS
022700971016     C                   MOVE      '£1'          D06COD
022800971016     C                   MOVEL     SIMFEL        D06KEY
022900971016     C                   MOVEL     'L'           D06TLA
023000981020     C                   MOVEL     TRUL06DS      KPJBU
023100980619      *
023200971016     C                   CALL      'TRUL06R'
023300971016     C                   PARM                    KPJBA
023400981020     C                   MOVEL     KPJBU         TRUL06DS
023500971016     C                   MOVEA     LIN           L1
023600981005    2C                   endif
023700980625
023800980625      *
023900980508     c     kbol01        klist
024000990409     c                   kfld                    dctaas
024100990409     c                   kfld                    dctlnp
024200990409     c                   kfld                    dctnrs
024300990409     c                   kfld                    dctnsp
024400981023      *
024500000309     c     kar6          klist
024600990409     c                   kfld                    dctaas
024700990409     c                   kfld                    dctlnp
024800990409     c                   kfld                    dctnrs
024900990409     c                   kfld                    dctnsp
025000000309     c                   kfld                    trcar6
025100990409      *
025200990209     c     kca           klist
025300980518     c                   kfld                    dctaac
025400980518     c                   kfld                    dctfil
025500980518     c                   kfld                    dctnca
025600000309      *
025700000309     c     klbl_F        klist
025800000309     c                   kfld                    LBLaan
025900000309     c                   kfld                    LBLlpn
026000000309     c                   kfld                    LBLnrn
026100000309     c                   kfld                    LBLnsn
026200000309      *
026300951009     C                   ENDSR
026400980611      *------------------------------------------------------------
026500990412     OPRTF198   E            TESTA             2
026600990412     o                                           74 'Calcolo mandato assicurati-
026700990412     o                                              vo'
026800990412     o          E            DETTA       2
026900990412     O                       dctaac               4
027000990412     O                       dctfil           +   1
027100990412     O                       dctnca        z  +   1
027200990412     O                       §dctlega         +   2
027300990412     O                       WISKSC           +   2
027400990412     O                       WISCTR           +   1
027500990412     O                       O48FTC           +   2
027600990412     O                       O48fvm           +   2
027700990412     O                       O48vlm        2  +   2
