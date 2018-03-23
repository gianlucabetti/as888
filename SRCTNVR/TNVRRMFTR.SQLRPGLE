000100940224     H*--------------------------------------------------------------*
000600060928
000601170511     d aggio           s              1
000602170511     d w_ftafiv        s                   like(ftafiv)
000603170511     d w_ftadft        s                   like(ftadft)
000604170511     d w_ftanft        s                   like(ftanft)
000605170511     D §ftaRRN         s             10I 0 inz
000606170511     d $Finerec        s               n   inz(*off)
000607170511     D WrkStringaSql   S           4500
000608170511     D                                     VARYING
000609170511     d fnftads       e ds                  extname(fnfta00f) inz
000900900131     C*---------------------------------------------------------------*
001000071009     c     *entry        plist
001100170511     c                   parm                    aggio
001101170511
001102170511         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
001200071009     c*
001201170511       WrkStringaSql='select fnfta00f.*, rrn(fnfta00f)  from fnfta00f' +
001206170511         ' order by  ftafiv, ftadft, ftanft';
001208170511           $finerec=*off;
001209170511
001210170511           exec sql prepare STRINGASQL from :WrkStringaSql;
001211170511           exec sql declare FTACSR cursor for StringaSql;
001212170511           exec sql open FTAcsr;
001213170511           dow $finerec=*off;
001214170511              exec sql Fetch next from FTAcsr into :fnftads, :§ftarrn ;
001215170511              if sqlcod=100 or sqlcod<0;
001216170511                 $finerec = *on;
001217170511                 leave;
001218170511              endif;
001219170511              if ftafiv=w_ftafiv and
001220170511                 ftadft=w_ftadft and
001221170511                 ftanft=w_ftanft       ;
001222170511       // cancello record doppio
001223170511       if aggio='S' ;
001224170511     C/EXEC SQL
001225170511     C+ DELETE FROM FNFTA00F WHERE RRN(FNFTA00F)=:§FTARRN
001226170511     C/END-EXEC
001227170511       endif;
001228170511              else;
001229170511                 w_ftafiv=ftafiv;
001230170511                 w_ftadft=ftadft;
001231170511                 w_ftanft=ftanft;
001232170511              endif;
001233170511           enddo;
001800071009     c*
007400071009      *
007500071009     C                   SETON                                        LR
