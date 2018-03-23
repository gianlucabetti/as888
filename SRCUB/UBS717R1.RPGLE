000100170412     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200990907
000300170412
000400170412      //********************************************************************************************
000500170412      //
000600170412      // Definizione prototipi procedure.
000700170412      //
000800170412      //********************************************************************************************
000900170412
001000170412
001100170412      //********************************************************************************************
001200170412      //
001300170412      // Definizione interfacce procedure.
001400170412      //
001500170412      //********************************************************************************************
001600170803     D/COPY GAITRASRC/SRCPROTOPI,UBS717R
001700170803     D/COPY GAITRASRC/SRCPROTOPI,UBS717R1
001800170516
001900170804
002000170804     D UBSI95DS      e ds
002100170804     D EDIVABDS      e ds                  extname(edivab0f)
002200170804
002300010525
002400010525     C*-----------------------------------------------------------------------------
002500170412     C*
002600170412     C* Avvio il monitoring del intero flusso
002700170412     C                   monitor
002800170803     C*
002900170804     C* Effettuo il "wrap"
003000170804     C                   exsr      exeWrap
003100170412     C*
003200170412     C* Gestisco eventuale errore
003300170412     C                   on-error
003400170518     C
003500170803     C                   eval      oUBS717Esito = -1
003600170412     C                   dump(A)
003700170420     C                   seton                                        lr
003800170412     C*
003900170412     C* Arresto il monitoring
004000170412     C                   endmon
004100100624     C*
004200170420     C                   return
004300010525     C*
004400010525     C*-----------------------------------------------------------------------------
004500170803
004600170803
004700170803     C*
004800170804     C     exeWrap       begsr
004900170522     C*
005000170804     C* Chiamata al driver di esecuzione operazioni
005100170804     C                   call      'UBS717R'
005200170804     C                   parm                    iUBS717VAB
005300170804     C                   parm                    iUBS717ENV
005400170804     C                   parm                    iUBS717OPZ
005500170804     C                   parm                    iUBS717KSU
005600170804     C                   parm                    iUBS717VPC
005700170804     C                   parm                    oUBS717Esito
005800170825     C                   parm                    oUBS717MSG
005900170804     C                   parm                    oUBS717SI95
006000170804     C                   parm                    oUBS717DLEN
006100170804     C                   parm                    oUBS717DATI
006200170522     C*
006300170804     C* Se tutto OK => effettuo "wrapper" dati
006400170804     C                   if        oUBS717Esito = 0
006500170803     C                   eval      oUBS717DAT01=%subst(oUBS717DATI:000001:32000)
006600170803     C                   eval      oUBS717DAT02=%subst(oUBS717DATI:032001:32000)
006700170803     C                   eval      oUBS717DAT03=%subst(oUBS717DATI:064001:32000)
006800170803     C                   eval      oUBS717DAT04=%subst(oUBS717DATI:096001:32000)
006900170803     C                   eval      oUBS717DAT05=%subst(oUBS717DATI:128001:32000)
007000170803     C                   eval      oUBS717DAT06=%subst(oUBS717DATI:160001:32000)
007100170803     C                   eval      oUBS717DAT07=%subst(oUBS717DATI:192001:32000)
007200170803     C                   eval      oUBS717DAT08=%subst(oUBS717DATI:224001:32000)
007300170803     C                   eval      oUBS717DAT09=%subst(oUBS717DATI:256001:32000)
007400170803     C                   eval      oUBS717DAT10=%subst(oUBS717DATI:288001:32000)
007500170803     C                   eval      oUBS717DAT11=%subst(oUBS717DATI:320001:32000)
007600170803     C                   eval      oUBS717DAT12=%subst(oUBS717DATI:352001:32000)
007700170803     C                   eval      oUBS717DAT13=%subst(oUBS717DATI:384001:32000)
007800170803     C                   eval      oUBS717DAT14=%subst(oUBS717DATI:416001:32000)
007900170803     C                   eval      oUBS717DAT15=%subst(oUBS717DATI:448001:32000)
008000170818     C                   eval      oUBS717DAT16=%subst(oUBS717DATI:480001:32000)
008100171003     C                   eval      oUBS717DAT17=%subst(oUBS717DATI:512001:32000)
008200171003     C                   eval      oUBS717DAT18=%subst(oUBS717DATI:544001:32000)
008300171003     C                   eval      oUBS717DAT19=%subst(oUBS717DATI:576001:32000)
008400171003     C                   eval      oUBS717DAT20=%subst(oUBS717DATI:608001:32000)
008500171003     C                   eval      oUBS717DAT21=%subst(oUBS717DATI:640001:32000)
008600171003     C                   eval      oUBS717DAT22=%subst(oUBS717DATI:672001:32000)
008700171003     C                   eval      oUBS717DAT23=%subst(oUBS717DATI:704001:32000)
008800171003     C                   eval      oUBS717DAT24=%subst(oUBS717DATI:736001:32000)
008900171003     C                   eval      oUBS717DAT25=%subst(oUBS717DATI:768001:32000)
009000170818     C***                eval      oUBS717DAT26=%subst(oUBS717DATI:800001:32000)
009100170818     C***                eval      oUBS717DAT27=%subst(oUBS717DATI:832001:32000)
009200170818     C***                eval      oUBS717DAT28=%subst(oUBS717DATI:864001:32000)
009300170818     C***                eval      oUBS717DAT29=%subst(oUBS717DATI:896001:32000)
009400170818     C***                eval      oUBS717DAT30=%subst(oUBS717DATI:928001:32000)
009500170818     C***                eval      oUBS717DAT31=%subst(oUBS717DATI:960001:32000)
009600170818     C***                eval      oUBS717DAT32=%subst(oUBS717DATI:992001)
009700170522     C                   endif
009800170419     C*
009900170419     C                   endsr
010000000714
010100050920
010200050920
010300000714      /TITLE Routine di *init PGM
010400000714     C*
010500000714     C     *inzsr        begsr
010600060103     C*
010700170803     C     *entry        plist
010800170803     C                   parm                    iUBS717VAB
010900170803     C                   parm                    iUBS717ENV
011000170803     C                   parm                    iUBS717OPZ
011100170803     C                   parm                    iUBS717KSU
011200170804     C                   parm                    iUBS717VPC
011300170803     C                   parm                    oUBS717Esito
011400170825     C                   parm                    oUBS717MSG
011500170803     C                   parm                    oUBS717SI95
011600170804     C                   parm                    oUBS717DLEN
011700170803     C                   parm                    oUBS717DAT01
011800170803     C                   parm                    oUBS717DAT02
011900170803     C                   parm                    oUBS717DAT03
012000170803     C                   parm                    oUBS717DAT04
012100170803     C                   parm                    oUBS717DAT05
012200170803     C                   parm                    oUBS717DAT06
012300170803     C                   parm                    oUBS717DAT07
012400170803     C                   parm                    oUBS717DAT08
012500170803     C                   parm                    oUBS717DAT09
012600170803     C                   parm                    oUBS717DAT10
012700170803     C                   parm                    oUBS717DAT11
012800170803     C                   parm                    oUBS717DAT12
012900170803     C                   parm                    oUBS717DAT13
013000170803     C                   parm                    oUBS717DAT14
013100170803     C                   parm                    oUBS717DAT15
013200170803     C                   parm                    oUBS717DAT16
013300171003     C                   parm                    oUBS717DAT17
013400171003     C                   parm                    oUBS717DAT18
013500171003     C                   parm                    oUBS717DAT19
013600171003     C                   parm                    oUBS717DAT20
013700171003     C                   parm                    oUBS717DAT21
013800171003     C                   parm                    oUBS717DAT22
013900171003     C                   parm                    oUBS717DAT23
014000171003     C                   parm                    oUBS717DAT24
014100171003     C                   parm                    oUBS717DAT25
014200170818     C***                parm                    oUBS717DAT26
014300170818     C***                parm                    oUBS717DAT27
014400170818     C***                parm                    oUBS717DAT28
014500170818     C***                parm                    oUBS717DAT29
014600170818     C***                parm                    oUBS717DAT30
014700170818     C***                parm                    oUBS717DAT31
014800170818     C***                parm                    oUBS717DAT32
014900170803     C*
015000000714     C                   endsr
015100000714
