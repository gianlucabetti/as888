000100080703     h dftactgrp(*no) actgrp(*caller)
000200080703     D Filiale         S              3
000300080703     D codicepc        S              1
000400080704     d Statement       s            256
000500080703     C     *ENTRY        PLIST
000600080703     C                   PARM                    codicepc
000700080703     C                   PARM                    Filiale
000800080703      /FREE
000900080703       *INLR = *ON;
001000080703       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
001100080704       eval Statement =
001200080704       'DELETE FROM FITR600F WHERE tr6fil=''' + filiale + '''' +
001300080704       ' and tr6cpc=''' + codicepc + '''';
001400080704       EXEC SQL
001500080704       execute immediate :Statement;
