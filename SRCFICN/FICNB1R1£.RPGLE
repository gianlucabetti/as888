000100950127     H DECEDIT('0,') DATEDIT(*DMY.)
000200030821     H* FICNB1R1 *----------------------------------------------------*
000300030821     H*         - conta le Bolle appartnenenti a stesso STOP
000400950127     H*--------------------------------------------------------------*
000500011113     FFIFTD01L  IF   E           K DISK
000600030821     Ffnarb01l  IF   E           K DISK
000700030821     Ffnblp01l  IF   E           K DISK
000800020319     F*--------------------------------------------------------------*
000900030821     D FICNB1        E DS                  EXTNAME(FICNB1DS)
001000950127     C*****************************************************************
001100020319     C     *ENTRY        PLIST
001200030821     C                   PARM                    ficnb1
001300020319      *
001400020319     C     KFTD          KLIST
001500030821     C                   KFLD                    ftdPDR
001600030821     C                   KFLD                    ftdTSR
001700030821     C                   KFLD                    ftdNDC
001800030821     C                   KFLD                    ftdDDC
001900030821     C                   KFLD                    ftdSTP
002000020319     C*
002100030821     C     KBolla        KLIST
002200030821     C                   kfld                    ftdAAS
002300030821     C                   kfld                    ftdLNP
002400030821     C                   kfld                    ftdNRS
002500030821     C                   kfld                    ftdNSP
002600030821      *
002700020319      *-----------------------------------------------------------------
002800030821     C                   z-add     0             conta_Sped        5 0
002900030821     C                   z-add     0             conta_FW          5 0
003000030821     C                   z-add     0             conta_RCC         5 0
003100020319     C*
003200030821     C                   z-add     §t1PDR        ftdPDR
003300030821     C                   move      §t1TSR        ftdTSR
003400030821     C                   z-add     §t1NDC        ftdNDC
003500030821     C                   z-add     §t1DDC        ftdDDC
003600030821     C                   z-add     §t1STP        ftdSTP
003700030821     C*
003800030821     C     KFTD          chain     fiftd01l                           21
003900030821     C     *in21         DOweq     *Off
004000030821     c                   add       1             conta_Sped
004100030821     C*
004200030821     C* Se è un Ritiro vado su Bolle in Partenza
004300030821     c                   if        ftdTSR = 'R'
004400030821     C     KBolla        chain     fnblp01l
004500030821     c                   if        %Found(fnblp01l) and
004600030821     c                             blpCBO = '2R'
004700030821     c                   add       1             conta_RCC
004800030821     c                   end
004900030821     c                   end
005000030821     C*
005100030821     C* Se è una Consegna vado su Bolle in Arrivo
005200030821     c                   if        ftdTSR = 'C'
005300030821     C     KBolla        chain     fnarb01l
005400030821     c                   if        %Found(fnarb01l) and
005500030822     c                             arbCBO = 'FW'
005600030821     c                   add       1             conta_FW
005700030821     c                   end
005800030821     c                   end
005900030821     C*
006000030821     C     KFTD          reade     fiftd01l                               21
006100030821     C                   ENDDO
006200030821     C*
006300030821     c                   eval      §t1NSS  =  conta_Sped
006400030821     c                   eval      §t1RCC  =  conta_RCC
006500030821     c                   eval      §t1FW   =  conta_FW
006600030821     C*
006700020320     c                   RETURN
006800020319     C*---------------------------------------------------------------*
