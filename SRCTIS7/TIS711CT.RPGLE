000100990920      /TITLE Upload via Internet: invio a PO.
000200990908     H dftactgrp(*yes)
000300170523
000400170523     Ftitas30c  IF   E           K DISK
000500170822     F**fivabwwr  UF A E             DISK
000600170822     Fedivabwr  UF A E             DISK
000700990920
000800990920     D dscmz         e ds
000900990920     D esito           s              1
001000170523
001100170523     D IDsped          ds                  qualified
001200170523     D   aas                          4s 0
001300170523     D   lnp                          3s 0
001400170523     D   nrs                          2s 0
001500170523     D   nsp                          7s 0
001600170523
001700991228
001800990916
001900991228     C*
002000170825     C* Sped x test:     2017020130309828
002100170825     C*
002200170523     C                   eval      IDsped = pInSped
002300170523     C*
002400170523     C* Aggancio la spedizione in sede
002500170523     C     KEYtas30_P    chain     titas30c
002600170523     C                   if        %found(titas30c)
002700170822     C***  *start        setll     fivabwwr
002800170822     C***                read      fivabwwr
002900170822     C***                delete(e) fivabwwr
003000170822     C***                clear                   fivab000
003100170822     C     *start        setll     edivabwr
003200170822     C                   read      edivabwr
003300170822     C                   delete(e) edivabwr
003400170822     C                   clear                   edivab00
003500170523     C                   eval      vabAAS = tasAAS
003600170609     C                   eval      vabLNP = tasLNP
003700170609     C***                eval      vabNRS = tasNRS
003800170609     C                   eval      vabNRS = 15
003900170523     C                   eval      vabNSP = tasNSP
004000170609     C***                eval      vabCCM = tasCCM
004100170609     C                   eval      vabCCM = 3250044
004200170523     C                   eval      vabMGS = tasMGS
004300170523     C                   eval      vabLNA = tasLNA
004400170523     C                   eval      vabZNC = tasZNC
004500170523     C                   eval      vabRSD = tasRSD
004600170523     C                   eval      vabIND = tasIND
004700170523     C                   eval      vabCAD = tasCAD
004800170523     C                   eval      vabLOD = tasLOD
004900170523     C                   eval      vabPRD = tasPRD
005000170523     C                   eval      vabNZD = tasNZD
005100170524     C***                eval      vabTSP = tasTSP
005200170801     C                   eval      vabTSP = 'H'
005300170801     C***                eval      vabTSP = 'E'
005400170609     C***                eval      vabTSP = 'C'
005500170523     C                   eval      vabNCL = tasNCL
005600170524     C***                eval      vabPKB = tasPKB
005700170524     C                   eval      vabPKB = 100
005800170523     C                   eval      vabRMN = tasRMN
005900170609     C***                if        tasNCD > *zeros
006000170609     C***                eval      vabNCD = tasNCD
006100170609     C***                else
006200170609     C***                eval      vabNCD = 50001
006300170609     C***                endif
006400170609     C***                if        tasNCA > *zeros
006500170609     C***                eval      vabNCA = tasNCA
006600170609     C***                endif
006700170609     C                   eval      VABNCD = *zeros
006800170609     C                   eval      VABNCA = *zeros
006900170822     C***                write     fivab000
007000170822     C                   write     edivab00
007100170524     C***                feod      fivabwwr
007200170822     C***                feod      edivabwr
007300170523     C                   reset                   dscmz
007400170523     C                   move      789           cmzdst
007500170801     C***                eval      cmzsyf = '3250044'
007600170822     C***                eval      cmzfld = 'FIVABWWR'
007700170822     C                   eval      cmzfld = 'EDIVABWR'
007800170523     C                   eval      cmzmbd = 'VPC0000001'
007900170822     C***                eval      cmzfla = 'FIVAB00F'
008000170822     C                   eval      cmzfla = 'EDIVAB0F'
008100170822     C***                eval      cmzmba = 'FIVAB00F'
008200170822     C                   eval      cmzmba = 'EDIVAB0F'
008300170523     C                   eval      cmznrr = *zeros
008400170523     C                   eval      cmzlba = 'G'
008500170523     C                   call(e)   'TIS711C'
008600170523     C                   parm                    dscmz
008700170523     C                   parm      *blanks       esito
008800170523     C                   endif
008900990916     C*
009000170524     C                   seton                                        lr
009100170523
009200170523
009300990916
009400990916      /TITLE Operazioni iniziali.
009500990916     C     *inzsr        begsr
009600990916     C*
009700990916     C     *entry        plist
009800170523     C                   parm                    pInSped          16
009900170523     C*
010000170523     C* - TITAS30C - Parziale
010100170523     C     KEYtas30_P    KLIST
010200170523     C                   KFLD                    IDsped.aas
010300170523     C                   KFLD                    IDsped.lnp
010400170523     C                   KFLD                    IDsped.nrs
010500170523     C                   KFLD                    IDsped.nsp
010600990916     C*
010700990916     C                   endsr
