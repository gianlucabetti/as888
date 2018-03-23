000100170906     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200170418
000300170418
000400170418      //********************************************************************************************
000500170418      //
000600170418      // Definizione file
000700170418      //
000800170418      //********************************************************************************************
000900170412
001000170412
001100170412      //********************************************************************************************
001200170412      //
001300170418      // Definizione interfacce/prototipi procedure.
001400170412      //
001500170412      //********************************************************************************************
001600170906     D/COPY GAITRASRC/SRCPROTOPI,UBS718R
001700170906     D/COPY GAITRASRC/SRCPROTOPI,UBS718R1
001800170413
001900170804     D DVPC          e ds                  inz
002000170418
002100010525     C*-----------------------------------------------------------------------------
002200170804     C                   clear                   DVPC
002300170804     C*
002400170906     C                   eval      §VPCCIS = 'N'
002500170906     C                   eval      §VPCCFD = 'N'
002600170906     C                   eval      §VPCTLL = 'N'
002700170906     C                   eval      §VPCIED = 'N'
002800170906     C                   eval      §VPCOFX = *zeros
002900170906     C                   eval      §VPCOFY = *zeros
003000170906     C                   eval      §VPCBRD = 'N'
003100170906     C                   eval      §VPCPDL = 'S'
003200170906     C                   eval      §VPCROW = 'S'
003300170906     C                   eval      §VPCSGF = 'Z'
003400170804     C*
003500170906     C                   eval      iUBS718LANG   = 'EN'
003600170928     C                   eval      iUBS718FGS    = 049
003700170928     C                   eval      iUBS718CCM    = 0495029
003800170928     C                   eval      iUBS718REF    = '99758772'
003900170928     C                   eval      iUBS718REFTYP = 'A'
004000170906     C                   eval      iUBS718LBLTYP = 'Z'
004100170906     C                   eval      iUBS718VPC    = DVPC
004200170804     C*
004300170804     C* Chiamata al driver di esecuzione operazioni
004400170906     C                   call      'UBS718R1'
004500170906     C                   parm                    iUBS718LANG
004600170908     C                   parm                    iUBS718FGS
004700170906     C                   parm                    iUBS718CCM
004800170906     C                   parm                    iUBS718REF
004900170906     C                   parm                    iUBS718REFTYP
005000170906     C                   parm                    iUBS718LBLTYP
005100170906     C                   parm                    iUBS718VPC
005200170906     C                   parm                    oUBS718Esito
005300170906     C                   parm                    oUBS718MSG
005400170925     C                   parm                    oUBS718NCL
005500170925     C                   parm                    oUBS718LNP
005600170925     C                   parm                    oUBS718LNA
005700170925     C                   parm                    oUBS718NRS
005800170925     C                   parm                    oUBS718ZNC
005900170925     C                   parm                    oUBS718NCD
006000170925     C                   parm                    oUBS718NCA
006100170906     C                   parm                    oUBS718DLEN
006200170906     C                   parm                    oUBS718DAT01
006300170906     C                   parm                    oUBS718DAT02
006400170906     C                   parm                    oUBS718DAT03
006500170906     C                   parm                    oUBS718DAT04
006600170906     C                   parm                    oUBS718DAT05
006700170906     C                   parm                    oUBS718DAT06
006800170906     C                   parm                    oUBS718DAT07
006900170906     C                   parm                    oUBS718DAT08
007000170906     C                   parm                    oUBS718DAT09
007100170906     C                   parm                    oUBS718DAT10
007200170804     C*
007300170804     C* Se tutto OK => effettuo "wrapper" dati
007400170906     C                   if        oUBS718Esito = 0
007500170906     C     oUBS718DLEN   dsply
007600170804     C                   endif
007700100624     C*
007800170418     C                   seton                                        lr
007900010525     C*
008000010525     C*-----------------------------------------------------------------------------
008100050920
008200050920
008300000714      /TITLE Routine di *init PGM
008400000714     C*
008500000714     C     *inzsr        begsr
008600000714     C*
008700000714     C                   endsr
008800000714
