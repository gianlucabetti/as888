000100080530     h dftactgrp(*no) actgrp(*caller)
000200170427      *__________________________________________________________________________
000300170427     FTntbe01l  IF   E           K DISK
000400060918      //
000500060918      // Definizione variabili.
000600060918      //
000700061017     D RTNesito        S             10I 0
000800061017     D RTNopcode       S             10
000900061017     D RTNstatus       S             10I 0
001000060918
001100170427     D Dpsp          E DS
001200170427     D Dpsps         E DS
001300170427     d  W              s              4  0
001400170427       // -?Dati da restituire?
001500170427     d skPWD           ds                  occurs(9990)
001600170427     d   pwdfil                       3    inz
001700170427     d   pwdcod                       2    inz
001800170427     d   pwddes                      15    inz
001900170427     d   pwdpad                      10    inz
002000060918      //********************************************************************************************
002100060918      //
002200060918      // Definizione parametri procedura.
002300060918      //
002400060918      //********************************************************************************************
002500060918     C     *ENTRY        PLIST
002600061017     C                   PARM                    rtnesito
002700061017     C                   PARM                    rtnopcode
002800061017     C                   PARM                    rtnstatus
002900170427     C*
003000170427     c                   z-add     0             w
003100170427     C                   move      'PSP'         tbecod
003200170427     c     'PSP'         setll     tntbe01l
003300170427     c                   do        *hival
003400170427     c     'PSP'         reade     tntbe01l
003500170427     c                   if        %eof(tntbe01l)
003600170427     c                   leave
003700170427     c                   endif
003800170427      *scarto elementi con flag
003900170427     c                   if        tbeke2 = 'FLAG'
004000170427     c                   iter
004100170427     c                   endif
004200170427     c                   if        tbeke2 <> *blank
004300170427      *cerco elementi pieni per rekord di tabella  max 11 per rekord
004400170427     c                   movel     tbeuni        dpsp
004500170427     c                   if        §PSPCOD01 <> *blank
004600170427     c                   add       1             w
004700170427     c     W             occur     Skpwd
004800170427     c                   movel     tbeke1        Pwdfil
004900170427     c                   move      §PSPCOD01     Pwdcod
005000170427     c                   move      §PSPDES01     Pwddes
005100170427     c                   movel(p)  §PSPPWD01     Pwdpad
005200170427     c                   endif
005300170427     c
005400170427     c                   if        §PSPCOD02 <> *blank
005500170427     c                   add       1             w
005600170427     c     W             occur     Skpwd
005700170427     c                   movel     tbeke1        Pwdfil
005800170427     c                   move      §PSPCOD02     Pwdcod
005900170427     c                   move      §PSPDES02     Pwddes
006000170427     c                   movel(p)  §PSPPWD02     Pwdpad
006100170427     c                   endif
006200170427     c                   if        §PSPCOD03 <> *blank
006300170427     c                   add       1             w
006400170427     c     W             occur     Skpwd
006500170427     c                   movel     tbeke1        Pwdfil
006600170427     c                   move      §PSPCOD03     Pwdcod
006700170427     c                   move      §PSPDES03     Pwddes
006800170427     c                   movel(p)  §PSPPWD03     Pwdpad
006900170427     c                   endif
007000170427     c                   if        §PSPCOD04 <> *blank
007100170427     c                   add       1             w
007200170427     c     W             occur     Skpwd
007300170427     c                   movel     tbeke1        Pwdfil
007400170427     c                   move      §PSPCOD04     Pwdcod
007500170427     c                   move      §PSPDES04     Pwddes
007600170427     c                   movel(p)  §PSPPWD04     Pwdpad
007700170427     c                   endif
007800170427     c                   if        §PSPCOD05 <> *blank
007900170427     c                   add       1             w
008000170427     c     W             occur     Skpwd
008100170427     c                   movel     tbeke1        Pwdfil
008200170427     c                   move      §PSPCOD05     Pwdcod
008300170427     c                   move      §PSPDES05     Pwddes
008400170427     c                   movel(p)  §PSPPWD05     Pwdpad
008500170427     c                   endif
008600170427     c                   if        §PSPCOD06 <> *blank
008700170427     c                   add       1             w
008800170427     c     W             occur     Skpwd
008900170427     c                   movel     tbeke1        Pwdfil
009000170427     c                   move      §PSPCOD06     Pwdcod
009100170427     c                   move      §PSPDES06     Pwddes
009200170427     c                   movel(p)  §PSPPWD06     Pwdpad
009300170427     c                   endif
009400170427     c                   if        §PSPCOD07 <> *blank
009500170427     c                   add       1             w
009600170427     c     W             occur     Skpwd
009700170427     c                   movel     tbeke1        Pwdfil
009800170427     c                   move      §PSPCOD07     Pwdcod
009900170427     c                   move      §PSPDES07     Pwddes
010000170427     c                   movel(p)  §PSPPWD07     Pwdpad
010100170427     c                   endif
010200170427     c                   if        §PSPCOD08 <> *blank
010300170427     c                   add       1             w
010400170427     c     W             occur     Skpwd
010500170427     c                   movel     tbeke1        Pwdfil
010600170427     c                   move      §PSPCOD08     Pwdcod
010700170427     c                   move      §PSPDES08     Pwddes
010800170427     c                   movel(p)  §PSPPWD08     Pwdpad
010900170427     c                   endif
011000170427     c                   if        §PSPCOD09 <> *blank
011100170427     c                   add       1             w
011200170427     c     W             occur     Skpwd
011300170427     c                   movel     tbeke1        Pwdfil
011400170427     c                   move      §PSPCOD09     Pwdcod
011500170427     c                   move      §PSPDES09     Pwddes
011600170427     c                   movel(p)  §PSPPWD09     Pwdpad
011700170427     c                   endif
011800170427     c                   if        §PSPCOD10 <> *blank
011900170427     c                   add       1             w
012000170427     c     W             occur     Skpwd
012100170427     c                   movel     tbeke1        Pwdfil
012200170427     c                   move      §PSPCOD10     Pwdcod
012300170427     c                   move      §PSPDES10     Pwddes
012400170427     c                   movel(p)  §PSPPWD10     Pwdpad
012500170427     c                   endif
012600170427     c                   if        §PSPCOD11 <> *blank
012700170427     c                   add       1             w
012800170427     c     W             occur     Skpwd
012900170427     c                   movel     tbeke1        Pwdfil
013000170427     c                   move      §PSPCOD11     Pwdcod
013100170427     c                   move      §PSPDES11     Pwddes
013200170427     c                   movel(p)  §PSPPWD11     Pwdpad
013300170427     c                   endif
013400170427     c                   else
013500170427     c                   movel     tbeuni        dpsps
013600170427     c                   if        §PSPCODs <> *blank
013700170427     c                   add       1             w
013800170427     c     W             occur     Skpwd
013900170427     c                   movel     tbeke1        Pwdfil
014000170427     c                   move      §PSPCODS      Pwdcod
014100170427     c                   move      §PSPDESS      Pwddes
014200170427     c                   movel     §PSPPWDS      Pwdpad
014300170427     c                   endif
014400170427     c                   endif
014500170427     c                   enddo
014600170427      *
014700170427      /free
014800170427        if w > 0;
014900170427
015000170427           exec sql   set result sets
015100170427                      for return to client
015200170427                      array :Skpwd for :W rows;
015300170427            else;
015400170427                   rtnopcode='Tab-No-ReP';
015500170427                   rtnstatus = -1;
015600170427                   rtnesito = -1;
015700170427         endif;
015800170427
015900170427      /end-free
016000170427     c                   seton                                        lr
