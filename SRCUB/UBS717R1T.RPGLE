000100170412     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
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
001600170804     D/COPY GAITRASRC/SRCPROTOPI,UBS717R
001700170804     D/COPY GAITRASRC/SRCPROTOPI,UBS717R1
001800170413
001900170804     D EDIVABDS      e ds                  extname(edivab0f) inz
002000170804     D UBSI95DS      e ds                  inz
002100170804     D DVPC          e ds                  inz
002200170418
002300010525     C*-----------------------------------------------------------------------------
002400170804     C                   clear                   EDIVABDS
002500170804     C                   clear                   UBSI95DS
002600170804     C                   clear                   DVPC
002700170804     C*
002800170804     C                   eval      §VPCCIS = 'S'
002900170804     C                   eval      §VPCIED = 'S'
003000170804     C                   eval      §VPCSGF = 'Z'
003100170804     C                   eval      §VPCOFX = '050'
003200170804     C                   eval      §VPCOFY = '050'
003300170804     C                   eval      §VPCBRD = 'N'
003400170804     C                   eval      §VPCPDL = 'N'
003500170804     C                   eval      §VPCROW = 'S'
003600170804     C*
003700170804     C                   eval      VABFGS = 789
003800170804     C                   eval      VABCCM = 1028888
003900170804     C                   eval      VABAAS = 2017
004000170804     C                   eval      VABLNP = 102
004100170804     C                   eval      VABNRS = 34
004200170804     C                   eval      VABNSP = 1234567
004300170804     C                   eval      VABRSD = 'TEST EDPFG'
004400170804     C                   eval      VABLOD = 'BOLOGNA'
004500170804     C                   eval      VABCAD = '40138'
004600170804     C                   eval      VABIND = 'VAI ENRICO MATTEI, 42'
004700170804     C                   eval      VABPRD = 'BO'
004800170804     C                   eval      VABNCL = 12
004900170804     C                   eval      VABPKB = 31,5
005000170804     C                   eval      VABNCD = 00240
005100170804     C                   eval      VABNCA = 00251
005200170804     C*
005300170804     C                   eval      iUBS717VAB = EDIVABDS
005400170804     C                   eval      iUBS717ENV = 'VWS'
005500170804     C                   eval      iUBS717OPZ = *blanks
005600170804     C                   eval      iUBS717KSU = '1020001'
005700170804     C                   eval      iUBS717VPC = DVPC
005800170804     C*
005900170804     C* Chiamata al driver di esecuzione operazioni
006000170804     C                   call      'UBS717R1'
006100170804     C                   parm                    iUBS717VAB
006200170804     C                   parm                    iUBS717ENV
006300170804     C                   parm                    iUBS717OPZ
006400170804     C                   parm                    iUBS717KSU
006500170804     C                   parm                    iUBS717VPC
006600170804     C                   parm                    oUBS717Esito
006700170825     C                   parm                    oUBS717MSG
006800170804     C                   parm                    oUBS717SI95
006900170804     C                   parm                    oUBS717DLEN
007000170804     C                   parm                    oUBS717DAT01
007100170804     C                   parm                    oUBS717DAT02
007200170804     C                   parm                    oUBS717DAT03
007300170804     C                   parm                    oUBS717DAT04
007400170804     C                   parm                    oUBS717DAT05
007500170804     C                   parm                    oUBS717DAT06
007600170804     C                   parm                    oUBS717DAT07
007700170804     C                   parm                    oUBS717DAT08
007800170804     C                   parm                    oUBS717DAT09
007900170804     C                   parm                    oUBS717DAT10
008000170804     C                   parm                    oUBS717DAT11
008100170804     C                   parm                    oUBS717DAT12
008200170804     C                   parm                    oUBS717DAT13
008300170804     C                   parm                    oUBS717DAT14
008400170804     C                   parm                    oUBS717DAT15
008500170804     C                   parm                    oUBS717DAT16
008600171003     C                   parm                    oUBS717DAT17
008700171003     C                   parm                    oUBS717DAT18
008800171003     C                   parm                    oUBS717DAT19
008900171003     C                   parm                    oUBS717DAT20
009000171003     C                   parm                    oUBS717DAT21
009100171003     C                   parm                    oUBS717DAT22
009200171003     C                   parm                    oUBS717DAT23
009300171003     C                   parm                    oUBS717DAT24
009400171003     C                   parm                    oUBS717DAT25
009500170818     C***                parm                    oUBS717DAT26
009600170818     C***                parm                    oUBS717DAT27
009700170818     C***                parm                    oUBS717DAT28
009800170818     C***                parm                    oUBS717DAT29
009900170818     C***                parm                    oUBS717DAT30
010000170818     C***                parm                    oUBS717DAT31
010100170818     C***                parm                    oUBS717DAT32
010200170804     C*
010300170804     C* Se tutto OK => effettuo "wrapper" dati
010400170804     C                   if        oUBS717Esito = 0
010500170804     C     oUBS717DLEN   dsply
010600170804     C                   endif
010700100624     C*
010800170418     C                   seton                                        lr
010900010525     C*
011000010525     C*-----------------------------------------------------------------------------
011100050920
011200050920
011300000714      /TITLE Routine di *init PGM
011400000714     C*
011500000714     C     *inzsr        begsr
011600000714     C*
011700000714     C                   endsr
011800000714
