000100990407     h DECEDIT('0,') DATEDIT(*DMY.)
000200990910
000300991027     fFNAR2T0E  IP   E             DISK
000400991027     fFIARBT0E  O  A E             DISK
000500990910
000600990913     d Ktrc            S                   LIKE(ARBtrc)  INZ('2')
000700990910
000800990913     c                   clear                   fiarbt00
000900990910      *
001000990913     c                   eval      ARBatb = AR2atb
001100990913     c                   eval      ARBaas = AR2aas
001200990913     c                   eval      ARBlnp = AR2lnp
001300990913     c                   eval      ARBnrs = AR2nrs
001400990913     c                   eval      ARBnsp = AR2nsp
001500990913     c                   eval      ARBcbo = AR2cbo
001600990913     c                   eval      ARBdtv = AR2dtv
001700990913     c                   eval      ARBorv = AR2orv
001800990916     c                   eval      ARBcvb = AR2cvb
001900990913     c                   eval      ARBpru = AR2pru
002000990913     c                   eval      ARBtrc = KTRC
002100990913     c                   eval      ARBksc = AR2ksc
002200990913     c                   eval      ARBctr = AR2ctr
002300990913     c                   eval      ARBdiv = 'ITL'
002400990913     c                   eval      ARBsv1 = AR2sv1
002500990913     c                   eval      ARBva1 = AR2va1
002600990913     c                   If        ARBva1 > *zeros
002700990913     c                   eval      ARBimv = AR2va1
002800990913     c                   Endif
002900991014     c                   eval      ARBift = AR2ift
003000991014     c                   eval      ARBali = AR2ali
003100990913     c                   eval      ARBcei = AR2cei
003200990913     c                   eval      ARBfiv = AR2fiv
003300990913     c                   eval      ARBnft = AR2nft
003400990913     c                   eval      ARBdft = AR2dft
003500991027     c                   eval      ARBlna = AR2lna
003600991027     c                   eval      ARBfev = AR2fev
003700991027     c                   eval      ARBdat = AR2dat
003800991027     c                   eval      ARBder = AR2der
003900990910      *  IVA
004000990913     c                   If        AR2iva > *zeros
004100990913     c                   Select
004200990914     c                   When        ARBsv1 = *blanks
004300990914     c                   eval      ARBsv1 = '8'
004400990913     c                   eval      ARBva1 = AR2iva
004500990914     c                   When        ARBsv2 = *blanks
004600990914     c                   eval      ARBsv2 = '8'
004700990913     c                   eval      ARBva2 = AR2iva
004800990914     c                   When        ARBsv3 = *blanks
004900990914     c                   eval      ARBsv3 = '8'
005000990913     c                   eval      ARBva3 = AR2iva
005100990913     c                   Endsl
005200990913     c                   Endif
005300990910      *  BOLLO
005400990913     c                   If        AR2bol > *zeros
005500990913     c                   Select
005600990914     c                   When        ARBsv1 = *blanks
005700990914     c                   eval      ARBsv1 = '6'
005800990913     c                   eval      ARBva1 = AR2bol
005900990914     c                   When        ARBsv2 = *blanks
006000990914     c                   eval      ARBsv2 = '6'
006100990913     c                   eval      ARBva2 = AR2bol
006200990914     c                   When        ARBsv3 = *blanks
006300990914     c                   eval      ARBsv3 = '6'
006400990913     c                   eval      ARBva3 = AR2bol
006500990913     c                   Endsl
006600990913     c                   Endif
006700990910      *
006800990913     c                   WRITE     FIARBT00
