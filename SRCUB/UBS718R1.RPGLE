000100170906     H DFTACTGRP(*NO) ACTGRP(*CALLER)
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
001600170906     D/COPY GAITRASRC/SRCPROTOPI,UBS718R
001700170906     D/COPY GAITRASRC/SRCPROTOPI,UBS718R1
001800170516
001900170804
002000010525
002100010525     C*-----------------------------------------------------------------------------
002200170412     C*
002300170412     C* Avvio il monitoring del intero flusso
002400170412     C                   monitor
002500170803     C*
002600170804     C* Effettuo il "wrap"
002700170804     C                   exsr      exeWrap
002800170412     C*
002900170412     C* Gestisco eventuale errore
003000170412     C                   on-error
003100170518     C
003200170906     C                   eval      oUBS718Esito = -1
003300170412     C                   dump(A)
003400170420     C                   seton                                        lr
003500170412     C*
003600170412     C* Arresto il monitoring
003700170412     C                   endmon
003800100624     C*
003900170420     C                   return
004000010525     C*
004100010525     C*-----------------------------------------------------------------------------
004200170803
004300170803
004400170803     C*
004500170804     C     exeWrap       begsr
004600170522     C*
004700170804     C* Chiamata al driver di esecuzione operazioni
004800170906     C                   call      'UBS718R'
004900170906     C                   parm                    iUBS718LANG
005000170908     C                   parm                    iUBS718FGS
005100170906     C                   parm                    iUBS718CCM
005200170906     C                   parm                    iUBS718REF
005300170906     C                   parm                    iUBS718REFTYP
005400170906     C                   parm                    iUBS718LBLTYP
005500170906     C                   parm                    iUBS718VPC
005600170906     C                   parm                    oUBS718Esito
005700170906     C                   parm                    oUBS718MSG
005800170925     C                   parm                    oUBS718NCL
005900170925     C                   parm                    oUBS718LNP
006000170925     C                   parm                    oUBS718LNA
006100170925     C                   parm                    oUBS718NRS
006200170925     C                   parm                    oUBS718ZNC
006300170925     C                   parm                    oUBS718NCD
006400170925     C                   parm                    oUBS718NCA
006500170906     C                   parm                    oUBS718DLEN
006600170906     C                   parm                    oUBS718DATI
006700170522     C*
006800170804     C* Se tutto OK => effettuo "wrapper" dati
006900170906     C                   if        oUBS718Esito = 0
007000170906     C                   eval      oUBS718DAT01=%subst(oUBS718DATI:000001:32000)
007100170906     C                   eval      oUBS718DAT02=%subst(oUBS718DATI:032001:32000)
007200170906     C                   eval      oUBS718DAT03=%subst(oUBS718DATI:064001:32000)
007300170906     C                   eval      oUBS718DAT04=%subst(oUBS718DATI:096001:32000)
007400170906     C                   eval      oUBS718DAT05=%subst(oUBS718DATI:128001:32000)
007500170906     C                   eval      oUBS718DAT06=%subst(oUBS718DATI:160001:32000)
007600170906     C                   eval      oUBS718DAT07=%subst(oUBS718DATI:192001:32000)
007700170906     C                   eval      oUBS718DAT08=%subst(oUBS718DATI:224001:32000)
007800170906     C                   eval      oUBS718DAT09=%subst(oUBS718DATI:256001:32000)
007900170906     C                   eval      oUBS718DAT10=%subst(oUBS718DATI:288001:32000)
008000170522     C                   endif
008100170419     C*
008200170419     C                   endsr
008300000714
008400050920
008500050920
008600000714      /TITLE Routine di *init PGM
008700000714     C*
008800000714     C     *inzsr        begsr
008900060103     C*
009000170803     C     *entry        plist
009100170906     C                   parm                    iUBS718LANG
009200170908     C                   parm                    iUBS718FGS
009300170906     C                   parm                    iUBS718CCM
009400170906     C                   parm                    iUBS718REF
009500170906     C                   parm                    iUBS718REFTYP
009600170906     C                   parm                    iUBS718LBLTYP
009700170906     C                   parm                    iUBS718VPC
009800170906     C                   parm                    oUBS718Esito
009900170906     C                   parm                    oUBS718MSG
010000170925     C                   parm                    oUBS718NCL
010100170925     C                   parm                    oUBS718LNP
010200170925     C                   parm                    oUBS718LNA
010300170925     C                   parm                    oUBS718NRS
010400170925     C                   parm                    oUBS718ZNC
010500170925     C                   parm                    oUBS718NCD
010600170925     C                   parm                    oUBS718NCA
010700170906     C                   parm                    oUBS718DLEN
010800170906     C                   parm                    oUBS718DAT01
010900170906     C                   parm                    oUBS718DAT02
011000170906     C                   parm                    oUBS718DAT03
011100170906     C                   parm                    oUBS718DAT04
011200170906     C                   parm                    oUBS718DAT05
011300170906     C                   parm                    oUBS718DAT06
011400170906     C                   parm                    oUBS718DAT07
011500170906     C                   parm                    oUBS718DAT08
011600170906     C                   parm                    oUBS718DAT09
011700170906     C                   parm                    oUBS718DAT10
011800170803     C*
011900000714     C                   endsr
012000000714
