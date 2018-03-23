000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300990409     fFNDCT00F  UP A E           K DISK
000400990407     fFNDPT01L  UF A E           k DISK
000500990409     fFNLBL02L  IF   E           K DISK
000600990409     fFNARB01L  IF   E           K DISK
000700990409     fFNBLP01L  IF   E           K DISK
000800990409     fFNBL401L  IF   E           K DISK
000900990409     fTABEL00F  IF   E           K DISK
001000990412     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
001100980611      * ---------------------------------------------------------
001200971016     D L1              S              3  0 DIM(30)                              P.O.GESTITI
001300990217     D R7O             S              1    DIM(30)                              Con. Anom. Reso
001400980717
001500990407     D FIDN48DS      E DS
001600971030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001700980727     d  tcu                  398    697    DIM(50)
001800980611     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
001900981020     D TRUL06DS      E DS
002000980611     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
002100981020     D CNCR80        E DS
002200981020     D KPJBA         E DS
002300981020     D DS7O          E DS
002400981228     D DDCT01        E DS
002500981023     D DSBL4A        E DS
002600981029
002700980610     d                 ds                  INZ
002800980727     d  DS_arbdat              1      8  0
002900980525     d  arbaas                 1      4  0
003000980525     d  arbmgs                 5      8  0
003100980615
003200980513     d TCUDS           DS
003300980513     d  f1                     1      1
003400980513     d  f3                     3      3
003500980513     d  f2                     2      2
003600980513     d  f4                     4      4
003700980513     d  f56                    5      6
003800981020
003900980615
004000981214     D wisksc          S                   LIKE(ARBksc)
004100981029     D wisctr          S                   LIKE(ARBctr)
004200990409     D Wtrc            S                   LIKE(BL4trc)
004300980625     D Kcod            S                   LIKE(TBLcod)
004400981230     D W§dctport       S                   LIKE(§DCTport)
004500990409     D wtpb            s              1
004600990210     D w0040           s              4  0
004700980708     D JJ              s              3  0
004800980625      * ---------------------------------------------------------
004900971016     IFNBLP000
005000971016     I              BLPAAS                      ARBAAS
005100971016     I              BLPLNP                      ARBLNP
005200971016     I              BLPNRS                      ARBNRS
005300971016     I              BLPNSP                      ARBNSP
005400971016     I              BLPMGS                      ARBMGS
005500990409     I              BLPTSP                      ARBTSP
005600971016     I              BLPKSC                      ARBKSC
005700990409     I              BLPCTR                      ARBCTR
005800971016     I              BLPCCA                      ARBCCA
005900980625      * ---------------------------------------------------------
006000990409     c                   clear                   wtpb
006100990409     c     kbol01        chain(N)  fnarb000                           30
006200990409     c                   IF        *IN30 = *OFF
006300990409     c                   eval      Wtpb = 'A'
006400990409     c                   ELSE
006500990429     c     kbol01        chain(N)  fnblp000                           30
006600990409     c  N30              eval      Wtpb = 'P'
006700990409     c                   ENDIF
006800990409      *
006900990409     c                   IF        Wtpb <> *blanks
007000990409      *
007100990409     c     DCTfil        lookup    L1                                     30
007200990409      * calcolo mandato solo per le C.A. aperte da me
007300990409     c                   IF        *IN30 = *ON
007400990409     c     kca           chain     fndpt000                           30
007500990412     c   30              clear                   fndpt000
007600990412     c   30              exsr      ctr_cliass
007700990409     c                   ENDIF
007800990409      *
007900990409      * se flag legami non impostato e se spedizione con consegna anomala diversa da RESO
008000990409      * verifico se bolla legata
008100990409     c                   movel     DCTflo        DDCT01
008200990409     c                   IF        §dctlega = *blanks  and  ARBcca <> *blanks
008300990409     c     arbcca        lookup    R7O                                    30
008400990409     c                   IF        *IN30 = *off
008500990409     c     Kbol01        chain(N)  fnlbl000                           30
008600990409     c                   IF        *IN30 = *off
008700990409     c                   eval      §DCTlega = 'S'
008800990409     c                   movel     DDCT01        DCTflo
008900990409     c                   update    FNDCT000
009000990409     c                   ENDIF
009100990409     c                   ENDIF
009200990409     c                   ENDIF
009300990409      *
009400990412     c   OF              except    TESTA
009500990412     c                   eval      *inof = *off
009600990412     c                   except    DETTA
009700990412      *
009800990409     c                   ENDIF
009900990409      **************************************************************************
010000990409      * VERIFICA ESISTENZA MANDATO ASSICURATIVO
010100990409      **************************************************************************
010200990409     C     CTR_CLIASS    BEGSR
010300990409
010400990409      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con quello C.A. tranne se la
010500990409      *  sped. è "Mamma di cambio porto", in questo caso inverto il porto C.A.
010600990409     c                   SELECT
010700990409     c                   WHEN      ARBcca <> '9'
010800990409     c                   movel     §DCTport      W§DCTport
010900990409     c                   WHEN      §DCTport = 'A'
011000990409     c                   movel     'F'           W§DCTport
011100990409     c                   WHEN      §DCTport = 'F'
011200990409     c                   movel     'A'           W§DCTport
011300990409     c                   ENDSL
011400990409      *
011500990409      * Imposto Codice Cliente Intestatario Spedizione (Franco=Mittente - Assegnato=Destinatario)
011600990409     c                   clear                   DSBL4A
011700990409     c                   z-add     ARBksc        WISksc
011800990409     c                   z-add     ARBctr        WISctr
011900990409      *
012000990409      *   Se sono in Arrivo e la sped è Mamma di cambio porto prendo il codice dalla figlia
012100990409     c                   IF        Wtpb = 'A'  and  ARBcca = '9'
012200990409     c     Kbol01        chain(N)  fnlbl000                           30
012300990409     c                   If        *IN30 = *off
012400990409     c     Klbl_F        chain(N)  fnarb000                           30
012500990409     c                   If        *IN30 = *off
012600990409     c                   z-add     ARBksc        WISksc
012700990409     c                   z-add     ARBctr        WISctr
012800990409     c     kbol01        chain(N)  fnarb000                           30
012900990409     c                   Endif
013000990409     c                   Endif
013100990409     c                   ELSE
013200990409      *
013300990409      *   Se assegnato in partenza verifico il codice destinatario
013400990409     c                   IF        Wtpb = 'P'  and  w§DCTport = 'A'
013500990409     c                   movel     'A'           WTRC
013600990409     c     KAR4          chain(N)  FNBL4000                           30
013700990409     c                   If        *in30 = *OFF
013800990409     c                   movel     BL4NOT        DSBL4A
013900990409     c                   If        §B4ksf <> *blanks  AND  §B4ctf <> *blanks
014000990409     c                   movel     §B4ksf        WISksc
014100990409     c                   movel     §B4ctf        WISctr
014200990409     c                   Endif
014300990409     c                   Endif
014400990409     c                   ENDIF
014500990409      *
014600990409     c                   ENDIF
014700990409
014800990409     c                   clear                   FIDN48DS
014900990409
015000990409      *  Clienti 8888 e 9999 sono senza mandato ed elevazione
015100990409     c                   move      WISksc        w0040
015200990409     c                   IF          W0040 <> 9999  AND  W0040 <> 8888
015300990409      *
015400990409     c                   movel     'L'           I48tla
015500990409     c                   z-add     WISksc        I48ksc
015600990409     c                   z-add     WISctr        I48ctr
015700990409     c                   movel     ARBtsp        I48tsp
015800990409     c                   movel     DS_arbdat     I48dsp
015900990409     c                   IF        §DCTtisp = *blanks
016000990409     c                   movel     'I'           I48fie
016100990409     c                   ELSE
016200990409     c                   movel     'E'           I48fie
016300990409     c                   ENDIF
016400990409     c                   movel     §DCTport      I48por
016500990409     c                   movel     'S'           I48mns
016600990409
016700990409     c                   CALL      'FIDN48R'
016800990409     C                   PARM                    KPJBA
016900990409     C                   PARM                    FIDN48DS
017000990409
017100990409     c                   IF        O48mand = 'S'
017200990409
017300990409     c                   z-add     dctaac        dptaac
017400990409     c                   z-add     dctfil        dptfil
017500990409     c                   z-add     dctnca        dptnca
017600990409     c                   movel     KCI           dptkcc
017700990409     c                   z-add     I48ksc        dptksc
017800990409     c                   z-add     I48ctr        dptctr
017900990409     c                   z-add     O48prg        dptprg
018000990409     c                   movel     O48ftc        dptftc
018100990409     c                   movel     O48fvm        dptfvm
018200990409     c                   z-add     O48vlm        dptvlm
018300990409     c                   movel     O48vvm        dptvvm
018400990409     c                   z-add     O48vlm        dptvlm
018500990409     c                   write     FNDPT000
018600990409     c                   ENDIF
018700990409     c                   ENDIF
018800990409      *
018900990409     C                   ENDSR
019000980722      *****************************************************************
019100980623      *  ROUTINE INIZIALE
019200980722      *****************************************************************
019300951009     C     *INZSR        BEGSR
019400980722
019500990412     c                   eval      *inof = *on
019600990412
019700981126      * Dati società
019800951009     C                   Z-ADD     1             CODUT
019900951009     C                   CALL      'X§PARUT'
020000951009     C                   PARM                    UT§DSE
020100980611      *
020200971016     C                   MOVEL     REC80         CNCR80
020300980611      *
020400980611      *--- RICERCA CAPOCONTI
020500990409     C                   DO        50            jj
020600990409     C                   MOVE      TCU(jj)       TCUDS
020700980513     C     f56           CABNE     'CG'          END1
020800980513     C     f4            COMP      '1'                                    21
020900980513     C     f4            COMP      '2'                                    22
021000980513     C     f4            COMP      '3'                                    23
021100980513     C     f4            COMP      '6'                                    27
021200980513     C** 1 CLIENTI   21
021300980513     C** 2 FORNITORI 22
021400980513     C** 3 AGENTI    23
021500980513     C     f3            comp      '0'                                242425
021600980513     C     f3            comp      'I'                                    26
021700980513     C** 0 ITALIA   25
021800980513     C** 1 ESTERO   24
021900980513     ** I CAPO CONTO IVA
022000980513     c                   if        *in21 = *on
022100990409     C   24              Z-ADD     KCU(jj)       KCE               4 0
022200990409     C   25              Z-ADD     KCU(jj)       KCI               4 0
022300980513     c                   end
022400980513     c                   if        *in22 = *on
022500990409     C   24              Z-ADD     KCU(jj)       KFE               4 0
022600990409     C   25              Z-ADD     KCU(jj)       KFI               4 0
022700980513     c                   end
022800980513     c                   if        *in23 = *on
022900990409     C   24              Z-ADD     KCU(jj)       KAE               4 0
023000990409     C   25              Z-ADD     KCU(jj)       KAI               4 0
023100980513     c                   end
023200990409     C   26              Z-ADD     KCU(jj)       KIVA              4 0
023300990409     C   27              Z-ADD     KCU(jj)       KBNA              4 0
023400980513     C     end1          tag
023500980513     C                   end
023600981229     C                   SETOFF                                       212223
023700981229     C                   SETOFF                                       242526
023800981229     C                   SETOFF                                       27
023900980619      ***
024000981005      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1  se non sono in sede
024100980619      ***
024200981005     C                   if        SIMFEL <> 0
024300981020     C                   CLEAR                   TRUL06DS
024400971016     C                   MOVE      '£1'          D06COD
024500971016     C                   MOVEL     SIMFEL        D06KEY
024600971016     C                   MOVEL     'L'           D06TLA
024700981020     C                   MOVEL     TRUL06DS      KPJBU
024800980619      *
024900971016     C                   CALL      'TRUL06R'
025000971016     C                   PARM                    KPJBA
025100981020     C                   MOVEL     KPJBU         TRUL06DS
025200971016     C                   MOVEA     LIN           L1
025300981005    2C                   endif
025400990409
025500990409      *     Causali consegna anomala Reso
025600990217     C                   clear                   JJ
025700981009     C                   MOVE      '7O'          KCOD
025800990312     C     ktabe         SETLL     TABEL00F
025900981009     C                   DO        *HIVAL
026000990312     C     ktabe         READE     TABEL00F                               31
026100981029     C   31              leave
026200981009     c                   movel     tbluni        DS7O
026300990217     c                   IF        §7Ores =  'S'
026400990217     C                   ADD       1             jj
026500990217     C                   MOVEL     TBLKEY        R7O(jj)
026600990217     c                   ENDIF
026700990409      *
026800981009     C                   ENDDO
026900980625
027000990312     C     KTABE         KLIST
027100990312     C                   KFLD                    CODUT
027200990312     C                   KFLD                    KCOD
027300980625      *
027400980508     c     kbol01        klist
027500990409     c                   kfld                    dctaas
027600990409     c                   kfld                    dctlnp
027700990409     c                   kfld                    dctnrs
027800990409     c                   kfld                    dctnsp
027900981023      *
028000981104     c     kar4          klist
028100990409     c                   kfld                    dctaas
028200990409     c                   kfld                    dctlnp
028300990409     c                   kfld                    dctnrs
028400990409     c                   kfld                    dctnsp
028500981023     c                   kfld                    WTRC
028600990409      *
028700990209     c     kca           klist
028800980518     c                   kfld                    dctaac
028900980518     c                   kfld                    dctfil
029000980518     c                   kfld                    dctnca
029100980721      *
029200981228     c     klbl_F        klist
029300981228     c                   kfld                    LBLaan
029400981228     c                   kfld                    LBLlpn
029500981228     c                   kfld                    LBLnrn
029600981228     c                   kfld                    LBLnsn
029700981224      *
029800951009     C                   ENDSR
029900980611      *------------------------------------------------------------
030000990412     OPRTF198   E            TESTA             2
030100990412     o                                           74 'Calcolo mandato assicurati-
030200990412     o                                              vo'
030300990412     o          E            DETTA       2
030400990412     O                       dctaac               4
030500990412     O                       dctfil           +   1
030600990412     O                       dctnca        z  +   1
030700990412     O                       §dctlega         +   2
030800990412     O                       WISKSC           +   2
030900990412     O                       WISCTR           +   1
031000990412     O                       O48FTC           +   2
031100990412     O                       O48fvm           +   2
031200990412     O                       O48vlm        2  +   2
