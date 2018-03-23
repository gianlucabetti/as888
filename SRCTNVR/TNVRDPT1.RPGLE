000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300990409     fFNDCT00F  UP A E           K DISK
000400990407     fFNDPT01L  UF A E           k DISK
000500990409     fFNLBL02L  IF   E           K DISK
000600990409     fFNBLP01L  IF   E           K DISK
000700990409     fFNBL401L  IF   E           K DISK
000800990409     fTABEL00F  IF   E           K DISK
000900990412     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
001000980611      * ---------------------------------------------------------
001100971016     D L1              S              3  0 DIM(30)                              P.O.GESTITI
001200990217     D R7O             S              1    DIM(30)                              Con. Anom. Reso
001300980717
001400990407     D FIDN48DS      E DS
001500971030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001600980727     d  tcu                  398    697    DIM(50)
001700980611     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
001800981020     D TRUL06DS      E DS
001900980611     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
002000981020     D CNCR80        E DS
002100981020     D KPJBA         E DS
002200981020     D DS7O          E DS
002300981228     D DDCT01        E DS
002400981023     D DSBL4A        E DS
002500981029
002600980610     d                 ds                  INZ
002700990429     d  DS_BLPdat              1      8  0
002800990429     d  BLPaas                 1      4  0
002900990429     d  BLPmgs                 5      8  0
003000980615
003100980513     d TCUDS           DS
003200980513     d  f1                     1      1
003300980513     d  f3                     3      3
003400980513     d  f2                     2      2
003500980513     d  f4                     4      4
003600980513     d  f56                    5      6
003700981020
003800980615
003900990429     D wisksc          S                   LIKE(BLPksc)
004000990429     D wisctr          S                   LIKE(BLPctr)
004100990409     D Wtrc            S                   LIKE(BL4trc)
004200980625     D Kcod            S                   LIKE(TBLcod)
004300981230     D W§dctport       S                   LIKE(§DCTport)
004400990409     D wtpb            s              1
004500990210     D w0040           s              4  0
004600980708     D JJ              s              3  0
004700980625      * ---------------------------------------------------------
004800990409     c                   clear                   wtpb
004900990429     c     kbol01        chain(N)  fnblp000                           30
005000990409     c  N30              eval      Wtpb = 'P'
005100990409      *
005200990409     c                   IF        Wtpb <> *blanks
005300990429     c                   clear                   FIDN48DS
005400990409      *
005500990409     c     DCTfil        lookup    L1                                     30
005600990409      * calcolo mandato solo per le C.A. aperte da me
005700990409     c                   IF        *IN30 = *ON
005800990409     c     kca           chain     fndpt000                           30
005900990412     c   30              clear                   fndpt000
006000990412     c   30              exsr      ctr_cliass
006100990409     c                   ENDIF
006200990409      *
006300990409      * se flag legami non impostato e se spedizione con consegna anomala diversa da RESO
006400990409      * verifico se bolla legata
006500990409     c                   movel     DCTflo        DDCT01
006600990429     c                   IF        §dctlega = *blanks  and  BLPcca <> *blanks
006700990429     c     BLPcca        lookup    R7O                                    30
006800990409     c                   IF        *IN30 = *off
006900990409     c     Kbol01        chain(N)  fnlbl000                           30
007000990409     c                   IF        *IN30 = *off
007100990409     c                   eval      §DCTlega = 'S'
007200990409     c                   movel     DDCT01        DCTflo
007300990409     c                   update    FNDCT000
007400990429      *
007500990429     c   OF              except    TESTA
007600990429     c                   eval      *inof = *off
007700990429     c                   except    DETTA
007800990409     c                   ENDIF
007900990409     c                   ENDIF
008000990409     c                   ENDIF
008100990412      *
008200990409     c                   ENDIF
008300990409      **************************************************************************
008400990409      * VERIFICA ESISTENZA MANDATO ASSICURATIVO
008500990409      **************************************************************************
008600990409     C     CTR_CLIASS    BEGSR
008700990409
008800990409      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con quello C.A. tranne se la
008900990409      *  sped. è "Mamma di cambio porto", in questo caso inverto il porto C.A.
009000990409     c                   SELECT
009100990429     c                   WHEN      BLPcca <> '9'
009200990409     c                   movel     §DCTport      W§DCTport
009300990409     c                   WHEN      §DCTport = 'A'
009400990409     c                   movel     'F'           W§DCTport
009500990409     c                   WHEN      §DCTport = 'F'
009600990409     c                   movel     'A'           W§DCTport
009700990409     c                   ENDSL
009800990409      *
009900990409      * Imposto Codice Cliente Intestatario Spedizione (Franco=Mittente - Assegnato=Destinatario)
010000990409     c                   clear                   DSBL4A
010100990429     c                   z-add     BLPksc        WISksc
010200990429     c                   z-add     BLPctr        WISctr
010300990409      *
010400990409      *   Se assegnato in partenza verifico il codice destinatario
010500990409     c                   IF        Wtpb = 'P'  and  w§DCTport = 'A'
010600990409     c                   movel     'A'           WTRC
010700990409     c     KAR4          chain(N)  FNBL4000                           30
010800990409     c                   If        *in30 = *OFF
010900990409     c                   movel     BL4NOT        DSBL4A
011000990409     c                   If        §B4ksf <> *blanks  AND  §B4ctf <> *blanks
011100990409     c                   movel     §B4ksf        WISksc
011200990409     c                   movel     §B4ctf        WISctr
011300990409     c                   Endif
011400990409     c                   Endif
011500990409     c                   ENDIF
011600990409
011700990409      *  Clienti 8888 e 9999 sono senza mandato ed elevazione
011800990409     c                   move      WISksc        w0040
011900990409     c                   IF          W0040 <> 9999  AND  W0040 <> 8888
012000990409      *
012100990409     c                   movel     'L'           I48tla
012200990409     c                   z-add     WISksc        I48ksc
012300990409     c                   z-add     WISctr        I48ctr
012400990429     c                   movel     BLPtsp        I48tsp
012500990429     c                   movel     DS_BLPdat     I48dsp
012600990409     c                   IF        §DCTtisp = *blanks
012700990409     c                   movel     'I'           I48fie
012800990409     c                   ELSE
012900990409     c                   movel     'E'           I48fie
013000990409     c                   ENDIF
013100990409     c                   movel     §DCTport      I48por
013200990409     c                   movel     'S'           I48mns
013300990409
013400990409     c                   CALL      'FIDN48R'
013500990409     C                   PARM                    KPJBA
013600990409     C                   PARM                    FIDN48DS
013700990409
013800990409     c                   IF        O48mand = 'S'
013900990409
014000990409     c                   z-add     dctaac        dptaac
014100990409     c                   z-add     dctfil        dptfil
014200990409     c                   z-add     dctnca        dptnca
014300990409     c                   movel     KCI           dptkcc
014400990409     c                   z-add     I48ksc        dptksc
014500990409     c                   z-add     I48ctr        dptctr
014600990409     c                   z-add     O48prg        dptprg
014700990409     c                   movel     O48ftc        dptftc
014800990409     c                   movel     O48fvm        dptfvm
014900990409     c                   z-add     O48vlm        dptvlm
015000990409     c                   movel     O48vvm        dptvvm
015100990409     c                   z-add     O48vlm        dptvlm
015200990409     c                   write     FNDPT000
015300990429
015400990429     c   OF              except    TESTA
015500990429     c                   eval      *inof = *off
015600990429     c                   except    DETTA
015700990409     c                   ENDIF
015800990409     c                   ENDIF
015900990409      *
016000990409     C                   ENDSR
016100980722      *****************************************************************
016200980623      *  ROUTINE INIZIALE
016300980722      *****************************************************************
016400951009     C     *INZSR        BEGSR
016500980722
016600990412     c                   eval      *inof = *on
016700990412
016800981126      * Dati società
016900951009     C                   Z-ADD     1             CODUT
017000951009     C                   CALL      'X§PARUT'
017100951009     C                   PARM                    UT§DSE
017200980611      *
017300971016     C                   MOVEL     REC80         CNCR80
017400980611      *
017500980611      *--- RICERCA CAPOCONTI
017600990409     C                   DO        50            jj
017700990409     C                   MOVE      TCU(jj)       TCUDS
017800980513     C     f56           CABNE     'CG'          END1
017900980513     C     f4            COMP      '1'                                    21
018000980513     C     f4            COMP      '2'                                    22
018100980513     C     f4            COMP      '3'                                    23
018200980513     C     f4            COMP      '6'                                    27
018300980513     C** 1 CLIENTI   21
018400980513     C** 2 FORNITORI 22
018500980513     C** 3 AGENTI    23
018600980513     C     f3            comp      '0'                                242425
018700980513     C     f3            comp      'I'                                    26
018800980513     C** 0 ITALIA   25
018900980513     C** 1 ESTERO   24
019000980513     ** I CAPO CONTO IVA
019100980513     c                   if        *in21 = *on
019200990409     C   24              Z-ADD     KCU(jj)       KCE               4 0
019300990409     C   25              Z-ADD     KCU(jj)       KCI               4 0
019400980513     c                   end
019500980513     c                   if        *in22 = *on
019600990409     C   24              Z-ADD     KCU(jj)       KFE               4 0
019700990409     C   25              Z-ADD     KCU(jj)       KFI               4 0
019800980513     c                   end
019900980513     c                   if        *in23 = *on
020000990409     C   24              Z-ADD     KCU(jj)       KAE               4 0
020100990409     C   25              Z-ADD     KCU(jj)       KAI               4 0
020200980513     c                   end
020300990409     C   26              Z-ADD     KCU(jj)       KIVA              4 0
020400990409     C   27              Z-ADD     KCU(jj)       KBNA              4 0
020500980513     C     end1          tag
020600980513     C                   end
020700981229     C                   SETOFF                                       212223
020800981229     C                   SETOFF                                       242526
020900981229     C                   SETOFF                                       27
021000980619      ***
021100981005      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1  se non sono in sede
021200980619      ***
021300981005     C                   if        SIMFEL <> 0
021400981020     C                   CLEAR                   TRUL06DS
021500971016     C                   MOVE      '£1'          D06COD
021600971016     C                   MOVEL     SIMFEL        D06KEY
021700971016     C                   MOVEL     'L'           D06TLA
021800981020     C                   MOVEL     TRUL06DS      KPJBU
021900980619      *
022000971016     C                   CALL      'TRUL06R'
022100971016     C                   PARM                    KPJBA
022200981020     C                   MOVEL     KPJBU         TRUL06DS
022300971016     C                   MOVEA     LIN           L1
022400981005    2C                   endif
022500990409
022600990409      *     Causali consegna anomala Reso
022700990217     C                   clear                   JJ
022800981009     C                   MOVE      '7O'          KCOD
022900990312     C     ktabe         SETLL     TABEL00F
023000981009     C                   DO        *HIVAL
023100990312     C     ktabe         READE     TABEL00F                               31
023200981029     C   31              leave
023300981009     c                   movel     tbluni        DS7O
023400990217     c                   IF        §7Ores =  'S'
023500990217     C                   ADD       1             jj
023600990217     C                   MOVEL     TBLKEY        R7O(jj)
023700990217     c                   ENDIF
023800990409      *
023900981009     C                   ENDDO
024000980625
024100990312     C     KTABE         KLIST
024200990312     C                   KFLD                    CODUT
024300990312     C                   KFLD                    KCOD
024400980625      *
024500980508     c     kbol01        klist
024600990409     c                   kfld                    dctaas
024700990409     c                   kfld                    dctlnp
024800990409     c                   kfld                    dctnrs
024900990409     c                   kfld                    dctnsp
025000981023      *
025100981104     c     kar4          klist
025200990409     c                   kfld                    dctaas
025300990409     c                   kfld                    dctlnp
025400990409     c                   kfld                    dctnrs
025500990409     c                   kfld                    dctnsp
025600981023     c                   kfld                    WTRC
025700990409      *
025800990209     c     kca           klist
025900980518     c                   kfld                    dctaac
026000980518     c                   kfld                    dctfil
026100980518     c                   kfld                    dctnca
026200980721      *
026300981228     c     klbl_F        klist
026400981228     c                   kfld                    LBLaan
026500981228     c                   kfld                    LBLlpn
026600981228     c                   kfld                    LBLnrn
026700981228     c                   kfld                    LBLnsn
026800981224      *
026900951009     C                   ENDSR
027000980611      *------------------------------------------------------------
027100990412     OPRTF198   E            TESTA             2
027200990412     o                                           74 'Calcolo mandato assicurati-
027300990412     o                                              vo'
027400990412     o          E            DETTA       2
027500990412     O                       dctaac               4
027600990412     O                       dctfil           +   1
027700990412     O                       dctnca        z  +   1
027800990412     O                       §dctlega         +   2
027900990412     O                       WISKSC           +   2
028000990412     O                       WISCTR           +   1
028100990412     O                       O48FTC           +   2
028200990412     O                       O48fvm           +   2
028300990412     O                       O48vlm        2  +   2
