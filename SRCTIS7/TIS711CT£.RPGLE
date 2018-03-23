000100990920      /TITLE Upload via Internet: invio a PO.
000200990908     H dftactgrp(*yes)
000300170523
000400170523     Ftitas30c  IF   E           K DISK
000500170524     Ffivabwwr  UF A E             DISK
000600990920
000700990920     D dscmz         e ds
000800990920     D esito           s              1
000900170523
001000170523     D IDsped          ds                  qualified
001100170523     D   aas                          4s 0
001200170523     D   lnp                          3s 0
001300170523     D   nrs                          2s 0
001400170523     D   nsp                          7s 0
001500170523
001600991228
001700990916
001800991228     C*
001900170523     C                   eval      IDsped = pInSped
002000170523     C*
002100170523     C* Aggancio la spedizione in sede
002200170523     C     KEYtas30_P    chain     titas30c
002300170523     C                   if        %found(titas30c)
002400170524     C     *start        setll     fivabwwr
002500170524     C                   read      fivabwwr
002600170524     C                   delete    fivabwwr
002700170523     C                   clear                   fivab000
002800170523     C                   eval      vabAAS = tasAAS
002900170609     C                   eval      vabLNP = tasLNP
003000170609     C***                eval      vabNRS = tasNRS
003100170609     C                   eval      vabNRS = 15
003200170523     C                   eval      vabNSP = tasNSP
003300170609     C***                eval      vabCCM = tasCCM
003400170609     C                   eval      vabCCM = 3250044
003500170523     C                   eval      vabMGS = tasMGS
003600170523     C                   eval      vabLNA = tasLNA
003700170523     C                   eval      vabZNC = tasZNC
003800170523     C                   eval      vabRSD = tasRSD
003900170523     C                   eval      vabIND = tasIND
004000170523     C                   eval      vabCAD = tasCAD
004100170523     C                   eval      vabLOD = tasLOD
004200170523     C                   eval      vabPRD = tasPRD
004300170523     C                   eval      vabNZD = tasNZD
004400170524     C***                eval      vabTSP = tasTSP
004500170801     C                   eval      vabTSP = 'H'
004600170801     C***                eval      vabTSP = 'E'
004700170609     C***                eval      vabTSP = 'C'
004800170523     C                   eval      vabNCL = tasNCL
004900170524     C***                eval      vabPKB = tasPKB
005000170524     C                   eval      vabPKB = 100
005100170523     C                   eval      vabRMN = tasRMN
005200170609     C***                if        tasNCD > *zeros
005300170609     C***                eval      vabNCD = tasNCD
005400170609     C***                else
005500170609     C***                eval      vabNCD = 50001
005600170609     C***                endif
005700170609     C***                if        tasNCA > *zeros
005800170609     C***                eval      vabNCA = tasNCA
005900170609     C***                endif
006000170609     C                   eval      VABNCD = *zeros
006100170609     C                   eval      VABNCA = *zeros
006200170523     C                   write     fivab000
006300170524     C***                feod      fivabwwr
006400170523     C                   reset                   dscmz
006500170523     C                   move      789           cmzdst
006600170801     C***                eval      cmzsyf = '3250044'
006700170523     C                   eval      cmzfld = 'FIVABWWR'
006800170523     C                   eval      cmzmbd = 'VPC0000001'
006900170523     C                   eval      cmzfla = 'FIVAB00F'
007000170523     C                   eval      cmzmba = 'FIVAB00F'
007100170523     C                   eval      cmznrr = *zeros
007200170523     C                   eval      cmzlba = 'G'
007300170523     C                   call(e)   'TIS711C'
007400170523     C                   parm                    dscmz
007500170523     C                   parm      *blanks       esito
007600170523     C                   endif
007700990916     C*
007800170524     C                   seton                                        lr
007900170523
008000170523
008100990916
008200990916      /TITLE Operazioni iniziali.
008300990916     C     *inzsr        begsr
008400990916     C*
008500990916     C     *entry        plist
008600170523     C                   parm                    pInSped          16
008700170523     C*
008800170523     C* - TITAS30C - Parziale
008900170523     C     KEYtas30_P    KLIST
009000170523     C                   KFLD                    IDsped.aas
009100170523     C                   KFLD                    IDsped.lnp
009200170523     C                   KFLD                    IDsped.nrs
009300170523     C                   KFLD                    IDsped.nsp
009400990916     C*
009500990916     C                   endsr
