000100990928      *  testata
000200990929     fFNDCT00F  IP   E             DISK    rename(fndct000:fndctnew)
000300990928     fFNDCT01L  UF A E           k DISK
000400990928      *  documenti da richiedere
000500990928     fFNDCC01L  UF A E           k DISK
000600990929     fFNDCC043  IF   E           k DISK    rename(fndcc000:fndccnew)
000700990928      *  dettaglio
000800990928     fFNDCD01L  UF A E           k DISK
000900990929     fFNDCD043  IF   E           k DISK    rename(fndcd000:fndcdnew)
001000990928      *  fasi
001100990928     fFNDCF01L  UF A E           k DISK
001200990929     fFNDCF043  IF   E           k DISK    rename(fndcf000:fndcfnew)
001300990928      *  liquidazione
001400990928     fFNDCL01L  UF A E           k DISK
001500990929     fFNDCL043  IF   E           k DISK    rename(fndcl000:fndclnew)
001600990928      *  responsabilità
001700990928     fFNDCR01L  UF A E           k DISK
001800990929     fFNDCR043  IF   E           k DISK    rename(fndcr000:fndcrnew)
001900990928      *  descrizioni
002000990928     fFNDCS01L  UF A E           k DISK
002100990929     fFNDCS043  IF   E           k DISK    rename(fndcs000:fndcsnew)
002200990928      *  mandato
002300990928     fFNDPT01L  UF A E           k DISK
002400990929     fFNDPT043  IF   E           k DISK    rename(fndpt000:fndptnew)
002500990928      *  anagrafico
002600990928     fFNDKA01L  UF A E           k DISK
002700990929     fFNDKA043  IF   E           k DISK    rename(fndka000:fndkanew)
002800990929      *
002900990929     fprtf198   O    F  198        PRINTER  oflind(*INOF)
003000990910
003100990929     d wdctfca         s                   like(dctfca)
003200990928     D ktpd            s                   LIKE(DCStpd)  INZ('C')
003300990928
003400990928     D knkt            DS
003500990928     D  DCTAAS
003600990928     D  DCTFIL
003700990928     D  DCTNCA
003800990928
003900990928     D DDCT01        E DS
004000990928
004100990928      *-----------------------------------------------------------
004200990928
004300990929     c                   eval      wdctfca = dctfca
004400990929     c     knca          CHAIN     fndct000
004500990928
004600990929     c                   IF        %found
004700990928     c                   exsr      locali
004800990928     c                   ELSE
004900990928     c                   exsr      non_locali
005000990928     c                   ENDIF
005100990929
005200990928      *-------------------------------------------------------------------------
005300990928      *  GESTIONE SPEDIZIONI LOCALI
005400990928      *-------------------------------------------------------------------------
005500990928     c     LOCALI        BEGSR
005600990928
005700990929      * FNDCT
005800990928     c                   if        dctgfc = 405
005900990929     c                   eval        dctgfc = 440
006000990928     c                   endif
006100990928      *
006200990929     c                   IF        Wdctfca <> dctfca
006300990929     c                   except    ANOMALIE
006400990929     c                   ENDIF
006500990929      *
006600990928     c                   update    fndct000
006700990928
006800990928     c                   ENDSR
006900990928      *-------------------------------------------------------------------------
007000990928      *  GESTIONE SPEDIZIONI NON LOCALI
007100990928      *-------------------------------------------------------------------------
007200990928     c     NON_LOCALI    BEGSR
007300990929
007400990929      * FNDCT
007500990929     c                   if        dctgfc = 405
007600990929     c                   eval        dctgfc = 440
007700990929     c                   endif
007800990929     c                   write     fndct000
007900990929
008000990929      * FNDCF
008100990929     c     knca          setll     fndcfnew
008200990929     c     knca          reade     fndcfnew
008300990929     c                   DOW       %EOF = *OFF
008400990929     c                   write     fndcf000
008500990929     c     knca          reade     fndcfnew
008600990929     c                   ENDDO
008700990929
008800990929      * FNDCD
008900990929     c     knca          setll     fndcdnew
009000990929     c     knca          reade     fndcdnew
009100990929     c                   DOW       %EOF = *OFF
009200990929     c                   write     fndcd000
009300990929     c     knca          reade     fndcdnew
009400990929     c                   ENDDO
009500990929
009600990929      * FNDCR
009700990929     c     knca          setll     fndcrnew
009800990929     c     knca          reade     fndcrnew
009900990929     c                   DOW       %EOF = *OFF
010000990929     c                   write     fndcr000
010100990929     c     knca          reade     fndcrnew
010200990929     c                   ENDDO
010300990929
010400990929      * FNDCC
010500990929     c     knca          setll     fndccnew
010600990929     c     knca          reade     fndccnew
010700990929     c                   DOW       %EOF = *OFF
010800990929     c                   write     fndcc000
010900990929     c     knca          reade     fndccnew
011000990929     c                   ENDDO
011100990929
011200990929      * FNDCS
011300990929     c     kdcs          setll     fndcsnew
011400990929     c     kdcs          reade     fndcsnew
011500990929     c                   DOW       %EOF = *OFF
011600991002     c     kdcs          chain     fndcs000
011700991002     c                   if        not %FOUND
011800990929     c                   write     fndcs000
011900991002     c                   endif
012000990929     c     kdcs          reade     fndcsnew
012100990929     c                   ENDDO
012200990929
012300990929      * FNDKA
012400990929     c     knca          setll     fndkanew
012500990929     c     knca          reade     fndkanew
012600990929     c                   DOW       %EOF = *OFF
012700990929     c                   write     fndka000
012800990929     c     knca          reade     fndkanew
012900990929     c                   ENDDO
013000990929
013100990929      * FNDCL
013200990929     c     knca          chain     fndclnew
013300990929     c                   IF        %FOUND
013400990929     c                   write     fndcl000
013500990929     c                   ENDIF
013600990929
013700990929      * FNDPT
013800990929     c     knca          chain     fndptnew
013900990929     c                   IF        %FOUND
014000990929     c                   write     fndpt000
014100990929     c                   ENDIF
014200990929
014300990928     c                   ENDSR
014400990910      *-------------------------------------------------------------------------
014500990928      *  SUBROUTINE INIZIALE
014600990928      *-------------------------------------------------------------------------
014700990910     c     *INZSR        BEGSR
014800990910
014900990928     c     Knca          KLIST
015000990928     c                   KFLD                    DCTAAC
015100990928     c                   KFLD                    DCTFIL
015200990928     c                   KFLD                    DCTNCA
015300990928
015400990928     C     KDCS          KLIST
015500990928     C                   KFLD                    KTPD
015600990928     C                   KFLD                    KNKT
015700990928
015800990407     c                   ENDSR
015900990929      *--------------------------------------------------------------------------------------------*
016000990929     OPRTF198   E            ANOMALIE    1
016100990929     o                                        +   2 'C.A.'
016200990929     o                       DCTAAC           +   3
016300990929     o                       DCTfil        Z  +   1
016400990929     o                       DCTnca        Z  +   1
016500990929     o                                        +   2 'Fase CA presente'
016600990929     o                       DCTfca        Z  +   1
016700990929     o                                        +   2 'Fase CA ricevuta'
016800990929     o                       WDCTfca       Z  +   1
