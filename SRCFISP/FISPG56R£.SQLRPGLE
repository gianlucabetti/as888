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
004200170804         UNION
004300170804         SELECT distinct PDDcodaut as AUT, PDDfgs as FGS, PDDndc as NDC
004400170804         from FIPDD00T
004500170804         for read only;
004600170804
004700170804         exec sql OPEN TOTDIST;
004800170804
004900170804         SELECT;
005000170804         WHEN  sqlcode < 0;
005100170804           rtnstatus = sqlCode;
005200170804           rtnopcode = 'SQLCODE';
005300170804           rtnesito  = -1;
005400170804           RETURN;
005500170804         WHEN  sqlcode = 100;
005600170804         WHEN  sqlcode > 0 and rtnopcode = ' ';
005700170804           rtnstatus = sqlCode;
005800170804           rtnopcode = 'TOTDIST';
005900170804         ENDSL;
006000170804
006100170804         exec sql set result sets for return to client
006200170804                  cursor TOTDIST;
006300170802
006400170802      // Richiesta fatta x FILIALE (nel profilo non c'è TUTTI e non c'è AU nei primi byte)
006500170802      // devo estrarre solo quello che è presente su AS della filiale richiesta
006600170802       WHEN  %subst(profilo:1:2) <> 'AU';
006700170804         exec sql DECLARE TOTDISTFIL cursor for
006800170804         SELECT distinct PDOcodaut as AUT, PDOfgs as FIL, PDOndc as NDC
006900170804         from FIPDO00T where digits(PDOfgs) = substr(:profilo, 1, 3)
007000170804         UNION
007100170804         SELECT distinct PDCcodaut as AUT, PDCfgs as FIL, PDCndc as NDC
007200170804         from FIPDC00T where digits(PDCfgs) = substr(:profilo, 1, 3)
007300170804         UNION
007400170804         SELECT distinct PDDcodaut as AUT, PDDfgs as FGS, PDDndc as NDC
007500170804         from FIPDD00T where digits(PDDfgs) = substr(:profilo, 1, 3)
007600170804         for read only;
007700170804
007800170804         exec sql OPEN TOTDISTFIL;
007900170804
008000170804         SELECT;
008100170804         WHEN  sqlcode < 0;
008200170804           rtnstatus = sqlCode;
008300170804           rtnopcode = 'SQLCODE';
008400170804           rtnesito  = -1;
008500170804           RETURN;
008600170804         WHEN  sqlcode = 100;
008700170804         WHEN  sqlcode > 0 and rtnopcode = ' ';
008800170804           rtnstatus = sqlCode;
008900170804           rtnopcode = 'TOTDISTFIL';
009000170804         ENDSL;
009100170804
009200170804         exec sql set result sets for return to client
009300170804                  cursor TOTDISTFIL;
009400170802
009500170802      // Richiesta fatta x AUTISTA
009600170802      // devo estrarre solo quello che è presente su AS della filiale richiesta
009700170802       OTHER;
009800170804         exec sql DECLARE TOTDISTAU cursor for
009900170804         SELECT distinct PDOcodaut as AUT, PDOfgs as FIL, PDOndc as NDC
010000170804         from FIPDO00T where digits(PDOcodaut) = substr(:profilo, 3, 7)
010100170804         UNION
010200170804         SELECT distinct PDCcodaut as AUT, PDCfgs as FIL, PDCndc as NDC
010300170804         from FIPDC00T where digits(PDCcodaut) = substr(:profilo, 3, 7)
010400170804         UNION
010500170804         SELECT distinct PDDcodaut as AUT, PDDfgs as FGS, PDDndc as NDC
010600170804         from FIPDD00T where digits(PDDcodaut) = substr(:profilo, 3, 7)
010700170804         for read only;
010800170802
010900170804         exec sql OPEN TOTDISTAU;
011000170802
011100170804         SELECT;
011200170804         WHEN  sqlcode < 0;
011300170804           rtnstatus = sqlCode;
011400170804           rtnopcode = 'SQLCODE';
011500170804           rtnesito  = -1;
011600170804           RETURN;
011700170804         WHEN  sqlcode = 100;
011800170804         WHEN  sqlcode > 0 and rtnopcode = ' ';
011900170804           rtnstatus = sqlCode;
012000170804           rtnopcode = 'TOTDISTAU';
012100170804         ENDSL;
012200170802
012300170804         exec sql set result sets for return to client
012400170804                  cursor TOTDISTAU;
012500170804       ENDSL;
012600170802
012700170802       RETURN;
