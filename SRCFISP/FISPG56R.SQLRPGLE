000100170802     h dftactgrp(*no) actgrp(*caller)
000200170802      //----------------------------------------------------------------------------
000300170802      // Definizione variabili.
000400170802      //----------------------------------------------------------------------------
000500170802     d Profilo         S             10
000600170802     d IDdispositivo   S             35
000700170802     d RTNesito        S             10I 0
000800170802     d RTNopcode       S             10
000900170802     d RTNstatus       S             10I 0
001000170802
001100170802      //----------------------------------------------------------------------------
001200170802      // Definizione parametri procedura.
001300170802      //----------------------------------------------------------------------------
001400170802     c     *Entry        plist
001500170802     c                   parm                    profilo
001600170802     c                   parm                    iddispositivo
001700170802     c                   parm                    rtnesito
001800170802     c                   parm                    rtnopcode
001900170802     c                   parm                    rtnstatus
002000170802
002100170802      //----------------------------------------------------------------------------
002200170802      // Main.
002300170802      //----------------------------------------------------------------------------
002400170802       clear rtnesito;
002500170802       clear rtnopcode;
002600170802       clear rtnstatus;
002700170802       *inlr = *on;
002800170802
002900170802       exec sql set option closqlcsr = *endmod;
003000170802
003100170802       SELECT;
003200170802
003300170802      // Richiesta fatta con TUTTI (nel profilo mi aspetto TUTTI)
003400170802      // devo estrarre tutto quello che c'è su AS
003500170802       WHEN  profilo = 'TUTTI';
003600170804         exec sql DECLARE TOTDIST cursor for
003700170804         SELECT distinct PDOcodaut as AUT, PDOfgs as FIL, PDOndc as NDC
003800170804         from FIPDO00T
003900170804         UNION
004000170804         SELECT distinct PDCcodaut as AUT, PDCfgs as FIL, PDCndc as NDC
004100170804         from FIPDC00T
004200170804         for read only;
004300170804
004400170804         exec sql OPEN TOTDIST;
004500170804
004600170804         SELECT;
004700170804         WHEN  sqlcode < 0;
004800170804           rtnstatus = sqlCode;
004900170804           rtnopcode = 'SQLCODE';
005000170804           rtnesito  = -1;
005100170804           RETURN;
005200170804         WHEN  sqlcode = 100;
005300170804         WHEN  sqlcode > 0 and rtnopcode = ' ';
005400170804           rtnstatus = sqlCode;
005500170804           rtnopcode = 'TOTDIST';
005600170804         ENDSL;
005700170804
005800170804         exec sql set result sets for return to client
005900170804                  cursor TOTDIST;
006000170802
006100170802      // Richiesta fatta x FILIALE (nel profilo non c'è TUTTI e non c'è AU nei primi byte)
006200170802      // devo estrarre solo quello che è presente su AS della filiale richiesta
006300170802       WHEN  %subst(profilo:1:2) <> 'AU';
006400170804         exec sql DECLARE TOTDISTFIL cursor for
006500170804         SELECT distinct PDOcodaut as AUT, PDOfgs as FIL, PDOndc as NDC
006600170804         from FIPDO00T where digits(PDOfgs) = substr(:profilo, 1, 3)
006700170804         UNION
006800170804         SELECT distinct PDCcodaut as AUT, PDCfgs as FIL, PDCndc as NDC
006900170804         from FIPDC00T where digits(PDCfgs) = substr(:profilo, 1, 3)
007000170804         for read only;
007100170804
007200170804         exec sql OPEN TOTDISTFIL;
007300170804
007400170804         SELECT;
007500170804         WHEN  sqlcode < 0;
007600170804           rtnstatus = sqlCode;
007700170804           rtnopcode = 'SQLCODE';
007800170804           rtnesito  = -1;
007900170804           RETURN;
008000170804         WHEN  sqlcode = 100;
008100170804         WHEN  sqlcode > 0 and rtnopcode = ' ';
008200170804           rtnstatus = sqlCode;
008300170804           rtnopcode = 'TOTDISTFIL';
008400170804         ENDSL;
008500170804
008600170804         exec sql set result sets for return to client
008700170804                  cursor TOTDISTFIL;
008800170802
008900170802      // Richiesta fatta x AUTISTA
009000170802      // devo estrarre solo quello che è presente su AS della filiale richiesta
009100170802       OTHER;
009200170804         exec sql DECLARE TOTDISTAU cursor for
009300170804         SELECT distinct PDOcodaut as AUT, PDOfgs as FIL, PDOndc as NDC
009400170804         from FIPDO00T where digits(PDOcodaut) = substr(:profilo, 3, 7)
009500170804         UNION
009600170804         SELECT distinct PDCcodaut as AUT, PDCfgs as FIL, PDCndc as NDC
009700170804         from FIPDC00T where digits(PDCcodaut) = substr(:profilo, 3, 7)
009800170804         for read only;
009900170802
010000170804         exec sql OPEN TOTDISTAU;
010100170802
010200170804         SELECT;
010300170804         WHEN  sqlcode < 0;
010400170804           rtnstatus = sqlCode;
010500170804           rtnopcode = 'SQLCODE';
010600170804           rtnesito  = -1;
010700170804           RETURN;
010800170804         WHEN  sqlcode = 100;
010900170804         WHEN  sqlcode > 0 and rtnopcode = ' ';
011000170804           rtnstatus = sqlCode;
011100170804           rtnopcode = 'TOTDISTAU';
011200170804         ENDSL;
011300170802
011400170804         exec sql set result sets for return to client
011500170804                  cursor TOTDISTAU;
011600170804       ENDSL;
011700170802
011800170802       RETURN;
