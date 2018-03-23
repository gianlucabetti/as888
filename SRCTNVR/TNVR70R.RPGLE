000100990928      *  testata
000200990928     fFNDCT01L  UF A E           k DISK
000300990928     fFNDCT00F  O  A E             DISK    rename(fndct000:fndctnew)
000400990928      *  documenti da richiedere
000500990928     fFNDCC01L  UF A E           k DISK
000600990928     fFNDCC00F  O  A E             DISK    rename(fndcc000:fndccnew)
000700990928      *  dettaglio
000800990928     fFNDCD01L  UF A E           k DISK
000900990928     fFNDCD00F  O  A E             DISK    rename(fndcd000:fndcdnew)
001000990928      *  fasi
001100990928     fFNDCF01L  UF A E           k DISK
001200990928     fFNDCF00F  O  A E             DISK    rename(fndcf000:fndcfnew)
001300990928      *  liquidazione
001400990928     fFNDCL01L  UF A E           k DISK
001500990928     fFNDCL00F  O  A E             DISK    rename(fndcl000:fndclnew)
001600990928      *  responsabilità
001700990928     fFNDCR01L  UF A E           k DISK
001800990928     fFNDCR00F  O  A E             DISK    rename(fndcr000:fndcrnew)
001900990928      *  descrizioni
002000990928     fFNDCS01L  UF A E           k DISK
002100990928     fFNDCS00F  O  A E             DISK    rename(fndcs000:fndcsnew)
002200990928      *  mandato
002300990928     fFNDPT01L  UF A E           k DISK
002400990928     fFNDPT00F  O  A E             DISK    rename(fndpt000:fndptnew)
002500990928      *  anagrafico
002600990928     fFNDKA01L  UF A E           k DISK
002700990928     fFNDKA00F  O  A E             DISK    rename(fndka000:fndkanew)
002800990928      *
002900990928     fTABEL00F  IF   E           K DISK
003000990910
003100990928     d wlnp            s                   like(dctlnp)
003200990928     D ktpd            s                   LIKE(DCStpd)  INZ('C')
003300990928
003400990928     D knkt            DS
003500990928     D  DCTAAS
003600990928     D  DCTFIL
003700990928     D  DCTNCA
003800990928
003900990928     D KPJBA         E DS
004000990928     D DDCT01        E DS
004100990928     D UT§DSE0F      E DS
004200990928     D CNCR80        E DS
004300990928     D TRUL06DS      E DS
004400990928     D  LIN                    1     90  0    DIM(30)
004500990928
004600990928      *-----------------------------------------------------------
004700990928
004800990928     c     *loval        setll     fndct000
004900990929     c                   read      fndct000                               lr
005000990929     c                   DOW       *inlr = *off
005100990928
005200990928     c                   movel     dctflo        ddct01
005300990928     c                   movel     §dctlnpc      wlnp
005400990928
005500990928      * determino se spedizione da elaborare
005600990928     c                   IF        Wlnp = 420  or  dctlna = 420  or
005700990928     c                             Wlnp = 430  or  dctlna = 430  or
005800990928     c                             Wlnp = 455  or  dctlna = 455  or
005900990928     c                             Wlnp = 475  or  dctlna = 475
006000990928
006100990928      * determino se spedizione locale
006200990928     c     wlnp          lookup    lin                                    01
006300990928     c   01dctlna        lookup    lin                                    01
006400990928      *
006500990928     c                   IF        *in01 = *on
006600990928     c                   exsr      locali
006700990928     c                   ELSE
006800990928     c                   exsr      non_locali
006900990928     c                   ENDIF
007000990928      *
007100990928     c                   ENDIF
007200990928      *
007300990929     c                   read      fndct000                               lr
007400990928     c                   ENDDO
007500990928      *-------------------------------------------------------------------------
007600990928      *  GESTIONE SPEDIZIONI LOCALI
007700990928      *-------------------------------------------------------------------------
007800990928     c     LOCALI        BEGSR
007900990928
008000990929      * FNDCT
008100990928     c                   clear                   dctft1
008200990928     c                   clear                   dctft2
008300990928      *
008400990928     c                   select
008500990928     c                   when      Wlnp = 420  or  dctlna = 420
008600990929     c                   eval       dctfl1 = 420
008700990928     c                   when      Wlnp = 430  or  dctlna = 430
008800990929     c                   eval       dctfl1 = 430
008900990928     c                   when      Wlnp = 455  or  dctlna = 455
009000990929     c                   eval       dctfl1 = 455
009100990928     c                   when      Wlnp = 475  or  dctlna = 475
009200990929     c                   eval       dctfl1 = 475
009300990928     c                   endsl
009400990928      *
009500990928     c                   if        dctgfc = 405
009600990929     c                   eval        dctgfc = 440
009700990928     c                   endif
009800990928      *
009900990928     c                   update    fndct000
010000990928
010100990928      * FNDCF
010200990928     c     knca          setll     fndcf000
010300990929     c     knca          reade     fndcf000
010400990928     c                   DOW       %EOF = *OFF
010500990928     c                   clear                   dcfftr
010600990928     c                   update    fndcf000
010700990928     c     knca          reade     fndcf000
010800990928     c                   ENDDO
010900990928
011000990928      * FNDCD
011100990928     c     knca          setll     fndcd000
011200990928     c     knca          reade     fndcd000
011300990928     c                   DOW       %EOF = *OFF
011400990928     c                   clear                   DCDftr
011500990928     c                   update    fndcd000
011600990928     c     knca          reade     fndcd000
011700990928     c                   ENDDO
011800990928
011900990928      * FNDCR
012000990928     c     knca          setll     fndcr000
012100990928     c     knca          reade     fndcr000
012200990928     c                   DOW       %EOF = *OFF
012300990928     c                   clear                   DCRftr
012400990928     c                   update    fndcr000
012500990928     c     knca          reade     fndcr000
012600990928     c                   ENDDO
012700990928
012800990928      * FNDCC
012900990928     c     knca          setll     fndcc000
013000990928     c     knca          reade     fndcc000
013100990928     c                   DOW       %EOF = *OFF
013200990928     c                   clear                   DCCftr
013300990928     c                   update    fndcc000
013400990928     c     knca          reade     fndcc000
013500990928     c                   ENDDO
013600990928
013700990928      * FNDCS
013800990928     c     kdcs          setll     fndcs000
013900990928     c     kdcs          reade     fndcs000
014000990928     c                   DOW       %EOF = *OFF
014100990928     c                   clear                   DCSft1
014200990928     c                   clear                   DCSdt1
014300990928     c                   update    fndcs000
014400990928     c     kdcs          reade     fndcs000
014500990928     c                   ENDDO
014600990928
014700990928      * FNDKA
014800990928     c     knca          setll     fndka000
014900990928     c     knca          reade     fndka000
015000990928     c                   DOW       %EOF = *OFF
015100990928     c                   clear                   Dkaftr
015200990928     c                   update    fndka000
015300990928     c     knca          reade     fndka000
015400990928     c                   ENDDO
015500990928
015600990928      * FNDCL
015700990928     c     knca          chain     fndcl000
015800990928     c                   IF        %FOUND
015900990928     c                   clear                   DCLftr
016000990928     c                   update    fndcl000
016100990928     c                   ENDIF
016200990928
016300990928      * FNDPT
016400990928     c     knca          chain     fndpt000
016500990928     c                   IF        %FOUND
016600990928     c                   clear                   DPTftr
016700990928     c                   update    fndpt000
016800990928     c                   ENDIF
016900990928
017000990928     c                   ENDSR
017100990928      *-------------------------------------------------------------------------
017200990928      *  GESTIONE SPEDIZIONI NON LOCALI
017300990928      *-------------------------------------------------------------------------
017400990928     c     NON_LOCALI    BEGSR
017500990929
017600990929      * FNDCT
017700990929     c                   if        dctgfc = 405
017800990929     c                   eval        dctgfc = 440
017900990929     c                   endif
018000990929     c                   write     fndctnew
018100991001     c                   delete    fndct000
018200990929
018300990929      * FNDCF
018400990929     c     knca          setll     fndcf000
018500990929     c     knca          reade     fndcf000
018600990929     c                   DOW       %EOF = *OFF
018700990929     c                   write     fndcfnew
018800991001     c                   delete    fndcf000
018900990929     c     knca          reade     fndcf000
019000990929     c                   ENDDO
019100990929
019200990929      * FNDCD
019300990929     c     knca          setll     fndcd000
019400990929     c     knca          reade     fndcd000
019500990929     c                   DOW       %EOF = *OFF
019600990929     c                   write     fndcdnew
019700991001     c                   delete    fndcd000
019800990929     c     knca          reade     fndcd000
019900990929     c                   ENDDO
020000990929
020100990929      * FNDCR
020200990929     c     knca          setll     fndcr000
020300990929     c     knca          reade     fndcr000
020400990929     c                   DOW       %EOF = *OFF
020500990929     c                   write     fndcrnew
020600991001     c                   delete    fndcr000
020700990929     c     knca          reade     fndcr000
020800990929     c                   ENDDO
020900990929
021000990929      * FNDCC
021100990929     c     knca          setll     fndcc000
021200990929     c     knca          reade     fndcc000
021300990929     c                   DOW       %EOF = *OFF
021400990929     c                   write     fndccnew
021500991001     c                   delete    fndcc000
021600990929     c     knca          reade     fndcc000
021700990929     c                   ENDDO
021800990929
021900990929      * FNDCS
022000990929     c     kdcs          setll     fndcs000
022100990929     c     kdcs          reade     fndcs000
022200990929     c                   DOW       %EOF = *OFF
022300990929     c                   write     fndcsnew
022400991001     c                   delete    fndcs000
022500990929     c     kdcs          reade     fndcs000
022600990929     c                   ENDDO
022700990929
022800990929      * FNDKA
022900990929     c     knca          setll     fndka000
023000990929     c     knca          reade     fndka000
023100990929     c                   DOW       %EOF = *OFF
023200990929     c                   write     fndkanew
023300991001     c                   delete    fndka000
023400990929     c     knca          reade     fndka000
023500990929     c                   ENDDO
023600990929
023700990929      * FNDCL
023800990929     c     knca          chain     fndcl000
023900990929     c                   IF        %FOUND
024000990929     c                   write     fndclnew
024100991001     c                   delete    fndcl000
024200990929     c                   ENDIF
024300990929
024400990929      * FNDPT
024500990929     c     knca          chain     fndpt000
024600990929     c                   IF        %FOUND
024700990929     c                   write     fndptnew
024800991001     c                   delete    fndpt000
024900990929     c                   ENDIF
025000990929
025100990928     c                   ENDSR
025200990910      *-------------------------------------------------------------------------
025300990928      *  SUBROUTINE INIZIALE
025400990928      *-------------------------------------------------------------------------
025500990910     c     *INZSR        BEGSR
025600990910
025700990928     c     Knca          KLIST
025800990928     c                   KFLD                    DCTAAC
025900990928     c                   KFLD                    DCTFIL
026000990928     c                   KFLD                    DCTNCA
026100990928
026200990928     C     KDCS          KLIST
026300990928     C                   KFLD                    KTPD
026400990928     C                   KFLD                    KNKT
026500990928
026600990928     c     KTABE         KLIST
026700990928     c                   KFLD                    CODUT
026800990928     c                   KFLD                    TBLcod
026900990910
027000990928      * CARICO TABELLA £1
027100990928     C                   eval      codut = 1
027200990928     C                   CALL      'X§PARUT'
027300990928     C                   PARM                    UT§DSE0F
027400990928     C                   MOVEL     REC80         CNCR80
027500990928      *
027600990928     C                   CLEAR                   TRUL06DS
027700990928     C                   MOVE      '£1'          D06COD
027800990928     C                   MOVEL     SIMFEL        D06KEY
027900990928     C                   MOVEL(P)  TRUL06DS      KPJBU
028000990928     C                   CALL      'TRUL06R'
028100990928     C                   PARM                    KPJBA
028200990928     C                   MOVEL     KPJBU         TRUL06DS
028300990928
028400990407     c                   ENDSR
