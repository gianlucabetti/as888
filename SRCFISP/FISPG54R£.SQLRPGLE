000100080530     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700061017     D Profilo         S             10
000800061031     D IDdispositivo   S             35
000900061017     D RTNesito        S             10I 0
001000061017     D RTNopcode       S             10
001100061017     D RTNstatus       S             10I 0
001200060918
001300060918      //********************************************************************************************
001400060918      //
001500060918      // Definizione parametri procedura.
001600060918      //
001700060918      //********************************************************************************************
001800060918     C     *ENTRY        PLIST
001900061017     C                   PARM                    Profilo
002000061031     C                   PARM                    IDdispositivo
002100061017     C                   PARM                    rtnesito
002200061017     C                   PARM                    rtnopcode
002300061017     C                   PARM                    rtnstatus
002400060918      /FREE
002500060918
002600060918       //*******************************************************************************************
002700060918       //
002800060918       // Main.
002900060918       //
003000060918       //*******************************************************************************************
003100061017       CLEAR rtnesito;
003200061017       CLEAR rtnopcode;
003300061017       CLEAR rtnstatus;
003400061030       *INLR = *ON;
003500060918      /END-FREE
003600080530     c* x chiudere i cursori
003700080530     C/EXEC SQL
003800080530     C+ SET OPTION CLOSQLCSR = *ENDMOD
003900080530     C/END-EXEC
004000080428      *___ scarico tabella PSP password x SPC  _____________________________
004100080409     C/EXEC SQL
004200090629     C+ DECLARE TBLPARAM CURSOR FOR
004300140331     C+ SELECT case when (a.tbeke2 ='1' or a.tbeke2='2') then 'A' when
004400140331     C+ a.tbeke2= ' ' then 'B' when a.tbeke2= 'FLAG' then 'C' end as
004500140331     C+ idparam , case when (a.tbeke2 ='1' or a.tbeke2='2') then
004600140331     C+ substr(a.tbeuni, 1, 242) concat substr(b.tbeuni, 1, 242) else
004700140331     C+ substr(a.tbeuni, 1, 242) end as des from tntbe00f as a left outer
004800140331     C+ join (select * from tntbe00f where tbecod = 'PSP' and tbeke1 =
004900140331     C+ substr(:profilo, 3, 3) and tbeke2 = '2') as b on a.tbecod =
005000140331     C+ b.tbecod and a.tbeke1 = b.tbeke1 where a.tbeatb = ' ' and a.tbecod
005100140331     C+ = 'PSP' and (( a.tbeke1 = substr(:profilo, 3, 3) and (a.tbeke2 =
005200140331     C+ '1' )) or (a.tbeke2 = 'FLAG' and a.tbeke1 = (select min(tbeke1)
005300140331     C+ from tntbe00f where tbecod ='PSP' and (tbeke1 = substr(:profilo,
005400140331     C+ 3, 3) or tbeke1 = '999') and tbeke2 = 'FLAG') ) or
005500140331     C+ (a.tbeke1 = '046'))
005600140331     C+ FOR READ ONLY
005700080409     C/END-EXEC
005800080409     C
005900080409     C/EXEC SQL
006000080428     C+ OPEN Tblparam
006100080409     C/END-EXEC
006200080409      /FREE
006300080409
006400080409       select;
006500080409       When sqlCode < 0;
006600080409         RTNstatus = sqlCode;
006700080409         RTNopcode = 'SQLCODE';
006800080409         RTNesito  = -1;
006900080409         RETURN;
007000080409       When sqlCode = 100;
007100080409       When sqlCode > 0 and RTNopcode = ' ';
007200080409         RTNstatus = sqlCode;
007300080428         RTNopcode = 'TBLPARAM';
007400080409       Endsl;
007500080409
007600080409      /END-FREE
007700080428      *___ scarico tabella CMR causali mancato ritiro ORM o chiusura ORM ___
007800080428     C/EXEC SQL
007900080428     C+ DECLARE TNTBE CURSOR FOR
008000080428     C+ SELECT SUBSTR(TBEKE1, 1, 3) as idcausa,
008100080428     C+ substr(TBEUNI, 1, 35) as descrizione,
008200080428     C+ substr(TBEUNI, 114, 1) as ormfsync,
008300080428     C+ substr(TBEUNI, 115, 1) as ormfsign
008400080428     C+ FROM TNTBE00F
008500080428     C+ WHERE tbecod = 'CMR' and tbelin = ' ' and substr(tbeuni, 113, 1) = 'S'
008600080428     C+ FOR READ ONLY
008700080428     C/END-EXEC
008800080428     C
008900080428     C/EXEC SQL
009000080428     C+ OPEN Tntbe
009100080428     C/END-EXEC
009200080428      /FREE
009300080428
009400080428       select;
009500080428       When sqlCode < 0;
009600080428         RTNstatus = sqlCode;
009700080428         RTNopcode = 'SQLCODE';
009800080428         RTNesito  = -1;
009900080428         RETURN;
010000080428       When sqlCode = 100;
010100080428       When sqlCode > 0 and RTNopcode = ' ';
010200080428         RTNstatus = sqlCode;
010300080428         RTNopcode = 'TNTBE';
010400080428       Endsl;
010500080428
010600080428      /END-FREE
010700080222      *_____________scarico tabella 2A + 2Z esiti consegna NO riserva ______
010800080307      * invia codice, decodifica, tipo mancata consegna, flag da inserire
010900080307      * data consegna richiesta, RITORNO INFO PDA SUBITO
011000080220      * se da inviare a PDA (2z tbluni pos.31)
011100080220     C/EXEC SQL
011200080307     C+ DECLARE TAB2A CURSOR FOR SELECT SUBSTR(a.TBLKEY, 1, 3) as idcmc,
011300080307     C+ substr(a.TBLUNI, 1, 50) as des, substr(a.TBLUNI, 79, 1) as ftc,
011400080307     C+ substr(b.TBLUNI, 17, 1) as dcr, substr(b.TBLUNI, 32, 1) as fsync
011500080307     C+ FROM TABEL00F a join tabel00f b on a.tblkey = b.tblkey WHERE
011600080307     C+ a.tblflg = ' ' and b.tblflg=' ' and a.tblcod = '2A' and
011700080307     C+ b.tblcod='2Z' and substr(b.tbluni, 31, 1) <> 'N' and
011800080311     C+ substr(a.tbluni, 85, 1) <> 'R' AND a.TBLKUT = 1 and b.tblkut= 1
011900080307     C+ FOR READ ONLY
012000080220     C/END-EXEC
012100080220     C
012200080220     C/EXEC SQL
012300080220     C+ OPEN TAB2a
012400080220     C/END-EXEC
012500080220      /FREE
012600080220
012700080220       select;
012800080220       When sqlCode < 0;
012900080220         RTNstatus = sqlCode;
013000080220         RTNopcode = 'SQLCODE';
013100080220         RTNesito  = -1;
013200080220         RETURN;
013300080307       When sqlCode = 100;
013400080307       When sqlCode > 0 and RTNopcode = ' ';
013500080307         RTNstatus = sqlCode;
013600080307         RTNopcode = 'TAB2A';
013700080220       Endsl;
013800080220
013900080220      /END-FREE
014000080222      *_____________scarico tabella 2A + 2Z solo riserva ___________________
014100131127      * invia codice, decodifica, obbligo note
014200080222      * se da inviare a PDA (2z tbluni pos.31)
014300080222     C/EXEC SQL
014400080307     C+ DECLARE TAB2A_RISERVA CURSOR FOR SELECT SUBSTR(a.TBLKEY, 1, 3) as
014500131127     C+ idriserv, substr(a.TBLUNI, 1, 20) as des,
014600131127     C+ substr(b.TBLUNI, 33, 1) as obbnote
014700131127     C+ FROM TABEL00F a join
014800080307     C+ tabel00f b on a.tblkey = b.tblkey WHERE a.tblflg = ' ' and
014900080307     C+ b.tblflg=' ' and a.tblcod = '2A' and b.tblcod='2Z' and
015000080307     C+ substr(b.tbluni, 31, 1) <> 'N' and substr(a.tbluni, 85, 1) = 'R'
015100080307     C+ AND a.TBLKUT = 1 and b.tblkut= 1 FOR READ ONLY
015200080222     C/END-EXEC
015300080222     C
015400080222     C/EXEC SQL
015500080222     C+ OPEN TAB2a_riserva
015600080222     C/END-EXEC
015700080222      /FREE
015800080222
015900080222       select;
016000080222       When sqlCode < 0;
016100080222         RTNstatus = sqlCode;
016200080222         RTNopcode = 'SQLCODE';
016300080222         RTNesito  = -1;
016400080222         RETURN;
016500080307       When sqlCode = 100;
016600080307       When sqlCode > 0 and RTNopcode = ' ';
016700080307         RTNstatus = sqlCode;
016800080307         RTNopcode = 'TAB2A_RIS';
016900080222       Endsl;
017000080222
017100080222      /END-FREE
017200070118
017300080222      *__________________scarico tabella 3E Anomalie spunte No quelle 8 ____
017400080214      * invia codice e decodifica solo se anomalia ancora utilizzata
017500080214      * (tbluni pos.36) e se da inviare a PDA (tbluni pos.37)
017600060918     C/EXEC SQL
017700080307     C+ DECLARE TAB3E CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 1) as idcan,
017800120709     C+ substr(TBLUNI, 1, 20) as des FROM TABEL00F WHERE tblflg = ' ' and
017900080307     C+ tblcod = '3E' and substr(tbluni, 36, 1) <> 'N' and substr(tbluni,
018000080307     C+ 37, 1) <> 'N' AND TBLKUT = 1 and substr(tbluni, 38, 1) <> '8' FOR
018100080307     C+ READ ONLY
018200060918     C/END-EXEC
018300061016     C
018400060918     C/EXEC SQL
018500070116     C+ OPEN TAB3E
018600060918     C/END-EXEC
018700060918      /FREE
018800060918
018900061017       select;
019000061017       When sqlCode < 0;
019100061017         RTNstatus = sqlCode;
019200061017         RTNopcode = 'SQLCODE';
019300061017         RTNesito  = -1;
019400060918         RETURN;
019500080307       When sqlCode = 100;
019600080307       When sqlCode > 0 and RTNopcode = ' ';
019700080307         RTNstatus = sqlCode;
019800080307         RTNopcode = 'TAB3E';
019900061017       Endsl;
020000060918
020100060918      /END-FREE
020200061017
020300080214      *__________________scarico tabella 1P Consegne particolari____________
020400080214      * invia codice e decodifica solo se utilizzabile in partenza
020500080214      * (tbluni pos.29) e se da inviare a PDA (tbluni pos.84)
020600070118     C/EXEC SQL
020700080307     C+ DECLARE TAB1P CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 1) as idtc ,
020800080307     C+ substr(TBLUNI, 1, 25) as des FROM TABEL00F WHERE tblflg = ' ' and
020900080307     C+ tblcod = '1P' and substr(tbluni, 29, 1) = 'S' and substr(tbluni,
021000080307     C+ 84, 1) <> 'N' AND TBLKUT = 1 FOR READ ONLY
021100070118     C/END-EXEC
021200070118     C
021300070118     C/EXEC SQL
021400070118     C+ OPEN TAB1P
021500070118     C/END-EXEC
021600070118      /FREE
021700070118
021800070118       select;
021900070118       When sqlCode < 0;
022000070118         RTNstatus = sqlCode;
022100070118         RTNopcode = 'SQLCODE';
022200070118         RTNesito  = -1;
022300070118         RETURN;
022400080307       When sqlCode = 100;
022500080307       When sqlCode > 0 and RTNopcode = ' ';
022600080307         RTNstatus = sqlCode;
022700080307         RTNopcode = 'TAB1P';
022800070118       Endsl;
022900070118
023000070118      /END-FREE
023100080214      *__________________scarico tabella 1A Tipo incasso____________________
023200080214      * invia codice, decodifica, flag tipo incasso e tipo intestazione
023300080214      * assegno solo se da inviare a PDA (tbluni pos.76)
023400070118     C/EXEC SQL
023500080307     C+ DECLARE TAB1A CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 2) as idtic,
023600080307     C+ substr(TBLUNI, 1, 25) as des, substr(TBLUNI, 27, 1) as fca,
023700080307     C+ substr(TBLUNI, 28, 1) as fmb, substr(TBLUNI, 32, 1) as tipsomma
023800080307     C+ FROM TABEL00F WHERE tblflg = ' ' and tblcod = '1A' and
023900080307     C+ substr(tbluni, 76, 1) <> 'N' AND TBLKUT = 1 FOR READ ONLY
024000070118     C/END-EXEC
024100070118     C
024200070118     C/EXEC SQL
024300070118     C+ OPEN TAB1A
024400070118     C/END-EXEC
024500070118      /FREE
024600070118
024700070118       select;
024800070118       When sqlCode < 0;
024900070118         RTNstatus = sqlCode;
025000070118         RTNopcode = 'SQLCODE';
025100070118         RTNesito  = -1;
025200070118         RETURN;
025300080307       When sqlCode = 100;
025400080307       When sqlCode > 0 and RTNopcode = ' ';
025500080307         RTNstatus = sqlCode;
025600080307         RTNopcode = 'TAB1A';
025700070118       Endsl;
025800070118      /END-FREE
025900080214      *__________________scarico tabella 7Q Particolarità giacenze _________
026000080214      * invia codice e nota LDV solo se è di tipo giacenza
026100080214      * (tblkey pos. 1=Q) stampabile in bolla (tblkey pos. 4=S) e
026200080214      * se da inviare a PDA (tbluni pos.89)
026300070118     C/EXEC SQL
026400080307     C+ DECLARE TAB7Q CURSOR FOR SELECT SUBSTR(TBLKEY, 2, 2) as idgga,
026500080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
026600080307     C+ tblcod = '7V' and substr(tblkey, 1, 1) = 'Q' and substr(tblkey,
026700080307     C+ 4, 1) = 'S' and substr(tbluni, 89, 1) <> 'N' AND TBLKUT = 1 FOR
026800080307     C+ READ ONLY
026900070118     C/END-EXEC
027000070118     C
027100070118     C/EXEC SQL
027200070118     C+ OPEN TAB7Q
027300070118     C/END-EXEC
027400070118      /FREE
027500070118
027600070118       select;
027700070118       When sqlCode < 0;
027800070118         RTNstatus = sqlCode;
027900070118         RTNopcode = 'SQLCODE';
028000070118         RTNesito  = -1;
028100070118         RETURN;
028200080307       When sqlCode = 100;
028300080307       When sqlCode > 0 and RTNopcode = ' ';
028400080307         RTNstatus = sqlCode;
028500080307         RTNopcode = 'TAB7Q';
028600070118       Endsl;
028700070118
028800070118      /END-FREE
028900070118
029000080214      *__________________scarico tabella 7R Particolarità consegne _________
029100080214      * invia codice e nota LDV solo se è di tipo consegna
029200080214      * (tblkey pos. 1=R) stampabile in bolla (tblkey pos. 4=S) e
029300080214      * se da inviare a PDA (tbluni pos.89)
029400070118     C/EXEC SQL
029500080307     C+ DECLARE TAB7R CURSOR FOR SELECT SUBSTR(TBLKEY, 2, 2) as idgma,
029600080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
029700080307     C+ tblcod = '7V' and substr(tblkey, 1, 1) = 'R' and substr(tblkey,
029800080307     C+ 4, 1) = 'S' and substr(tbluni, 89, 1) <> 'N' AND TBLKUT = 1 FOR
029900080307     C+ READ ONLY
030000070118     C/END-EXEC
030100070118     C
030200070118     C/EXEC SQL
030300070118     C+ OPEN TAB7R
030400070118     C/END-EXEC
030500070118      /FREE
030600070118
030700070118       select;
030800070118       When sqlCode < 0;
030900070118         RTNstatus = sqlCode;
031000070118         RTNopcode = 'SQLCODE';
031100070118         RTNesito  = -1;
031200070118         RETURN;
031300080307       When sqlCode = 100;
031400080307       When sqlCode > 0 and RTNopcode = ' ';
031500080307         RTNstatus = sqlCode;
031600080307         RTNopcode = 'TAB7R';
031700070118       Endsl;
031800070118
031900070118      /END-FREE
032000080214      *__________________scarico tabella 7S Particolarità varie ____________
032100080214      * invia codice e nota LDV solo se è di tipo varie
032200080214      * (tblkey pos. 1=S) stampabile in bolla (tblkey pos. 4=S) e
032300080214      * se da inviare a PDA (tbluni pos.89)
032400070118     C/EXEC SQL
032500080307     C+ DECLARE TAB7S CURSOR FOR SELECT SUBSTR(TBLKEY, 2, 2) as idgva,
032600080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
032700080307     C+ tblcod = '7V' and substr(tblkey, 1, 1) = 'S' and substr(tblkey,
032800080307     C+ 4, 1) = 'S' and substr(tbluni, 89, 1) <> 'N' AND TBLKUT = 1 FOR
032900080307     C+ READ ONLY
033000070118     C/END-EXEC
033100070118     C
033200070118     C/EXEC SQL
033300070118     C+ OPEN TAB7S
033400070118     C/END-EXEC
033500070118      /FREE
033600070118
033700070118       select;
033800070118       When sqlCode < 0;
033900070118         RTNstatus = sqlCode;
034000070118         RTNopcode = 'SQLCODE';
034100070118         RTNesito  = -1;
034200070118         RETURN;
034300080307       When sqlCode = 100;
034400080307       When sqlCode > 0 and RTNopcode = ' ';
034500080307         RTNstatus = sqlCode;
034600080307         RTNopcode = 'TAB7S';
034700070118       Endsl;
034800070118
034900070118      /END-FREE
035000080214      *__________________scarico tabella 7P particolarità contrassegno______
035100080214      * invia codice e decodifica
035200080214      * se da inviare a PDA (tbluni pos.89)
035300080214     C/EXEC SQL
035400080307     C+ DECLARE TAB7P CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 2) as idgca,
035500080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
035600080307     C+ tblcod = '7P' and substr(tbluni, 54, 1) <> 'N' AND TBLKUT = 1 FOR
035700080307     C+ READ ONLY
035800080214     C/END-EXEC
035900080214     C
036000080214     C/EXEC SQL
036100080214     C+ OPEN TAB7P
036200080214     C/END-EXEC
036300080214      /FREE
036400080214
036500080214       select;
036600080214       When sqlCode < 0;
036700080214         RTNstatus = sqlCode;
036800080214         RTNopcode = 'SQLCODE';
036900080214         RTNesito  = -1;
037000080307       When sqlCode = 100;
037100080214         RETURN;
037200080307       When sqlCode > 0 and RTNopcode = ' ';
037300080307         RTNstatus = sqlCode;
037400080307         RTNopcode = 'TAB7P';
037500080214       Endsl;
037600080214
037700080214      /END-FREE
037800080214      *__________________scarico tabella GCH TNTBE Giorni di chiusura ______
037900080214      * invia codice, decodifica solo se da inviare a PDA (tbluni pos.36)
038000070118     C/EXEC SQL
038100080307     C+ DECLARE TABGCH CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 2) as idgc,
038200080307     C+ substr(TBEUNI, 1, 35) as des FROM TNTBE00F WHERE tbeatb = ' ' and
038300080307     C+ tbecod = 'GCH' and tbelin = ' ' and substr(tbeuni, 36, 1) <> 'N'
038400080307     C+ FOR READ ONLY
038500070118     C/END-EXEC
038600070118     C
038700070118     C/EXEC SQL
038800070118     C+ OPEN Tabgch
038900070118     C/END-EXEC
039000070118      /FREE
039100070118
039200070118       select;
039300070118       When sqlCode < 0;
039400070118         RTNstatus = sqlCode;
039500070118         RTNopcode = 'SQLCODE';
039600070118         RTNesito  = -1;
039700070118         RETURN;
039800080307       When sqlCode = 100;
039900080307       When sqlCode > 0 and RTNopcode = ' ';
040000080307         RTNstatus = sqlCode;
040100080307         RTNopcode = 'TABGCH';
040200070118       Endsl;
040300070118
040400070118      /END-FREE
040500080214      *_______scarico tabella DOC TNTBE documenti da consegnare x PDA ______
040600080214      * invia codice, decodifica solo se da inviare a PDA (tbluni pos.41)
040700080214     C/EXEC SQL
040800080307     C+ DECLARE TABDOC CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as iddocum,
040900080307     C+ substr(TBEUNI, 1, 40) as des FROM TNTBE00F WHERE tbeatb = ' ' and
041000080307     C+ tbecod = 'DOC' and tbelin = ' ' and substr(tbeuni, 41, 1) <> 'N'
041100080307     C+ FOR READ ONLY
041200080214     C/END-EXEC
041300080214     C
041400080214     C/EXEC SQL
041500080214     C+ OPEN Tabdoc
041600080214     C/END-EXEC
041700080214      /FREE
041800080214
041900080214       select;
042000080214       When sqlCode < 0;
042100080214         RTNstatus = sqlCode;
042200080214         RTNopcode = 'SQLCODE';
042300080214         RTNesito  = -1;
042400080214         RETURN;
042500080307       When sqlCode = 100;
042600080307       When sqlCode > 0 and RTNopcode = ' ';
042700080307         RTNstatus = sqlCode;
042800080307         RTNopcode = 'TABDOC';
042900080214       Endsl;
043000080214
043100080214      /END-FREE
043200080214      *_______scarico tabella TSA TNTBE tipo servizio per aut. _____________
043300080214      * invia codice, decodifica solo se da inviare a PDA (tbluni pos.26)
043400080214     C/EXEC SQL
043500080307     C+ DECLARE TABTSA CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as
043600080307     C+ idserviz, substr(TBEUNI, 1, 25) as des FROM TNTBE00F WHERE tbeatb
043700080307     C+ = ' ' and tbecod = 'TSA' and tbelin = ' ' and substr(tbeuni, 26,
043800080307     C+ 1) <> 'N' FOR READ ONLY
043900080214     C/END-EXEC
044000080214     C
044100080214     C/EXEC SQL
044200080214     C+ OPEN Tabtsa
044300080214     C/END-EXEC
044400080214      /FREE
044500080214
044600080214       select;
044700080214       When sqlCode < 0;
044800080214         RTNstatus = sqlCode;
044900080214         RTNopcode = 'SQLCODE';
045000080214         RTNesito  = -1;
045100080214         RETURN;
045200080307       When sqlCode = 100;
045300080307       When sqlCode > 0 and RTNopcode = ' ';
045400080307         RTNstatus = sqlCode;
045500080307         RTNopcode = 'TABTSA';
045600080214       Endsl;
045700080214
045800080214      /END-FREE
045900131127      *__________________scarico tabella ASA TNTBE Assistenza Pda x CON ____
046000101108      * invia codice, decodifica
046100101108     C/EXEC SQL
046200101108     C+ DECLARE TABASA CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as idasa,
046300101108     C+ substr(TBEUNI, 1, 35) as des FROM TNTBE00F WHERE tbeatb = ' ' and
046400101108     C+ tbecod = 'ASA' and tbelin = ' '
046500101108     C+ FOR READ ONLY
046600101108     C/END-EXEC
046700101108     C
046800101108     C/EXEC SQL
046900101108     C+ OPEN Tabasa
047000101108     C/END-EXEC
047100101108      /FREE
047200101108
047300101108       select;
047400101108       When sqlCode < 0;
047500101108         RTNstatus = sqlCode;
047600101108         RTNopcode = 'SQLCODE';
047700101108         RTNesito  = -1;
047800101108         RETURN;
047900101108       When sqlCode = 100;
048000101108       When sqlCode > 0 and RTNopcode = ' ';
048100101108         RTNstatus = sqlCode;
048200101108         RTNopcode = 'TABASA';
048300101108       Endsl;
048400101108
048500101108      /END-FREE
048600131127      *__________________scarico tabella RAS TNTBE Assistenza Pda x ORM ____
048700131127      * invia codice, decodifica
048800131127     C/EXEC SQL
048900131127     C+ DECLARE TABRAS CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as idRAS,
049000131127     C+ substr(TBEUNI, 1, 35) as des FROM TNTBE00F WHERE tbeatb = ' ' and
049100131127     C+ tbecod = 'RAS' and tbelin = ' ' and substr(tbeuni, 36,
049200131127     C+ 1) = 'S' FOR READ ONLY
049300131127     C/END-EXEC
049400131127     C
049500131127     C/EXEC SQL
049600131127     C+ OPEN Tabras
049700131127     C/END-EXEC
049800131127      /FREE
049900131127
050000131127       select;
050100131127       When sqlCode < 0;
050200131127         RTNstatus = sqlCode;
050300131127         RTNopcode = 'SQLCODE';
050400131127         RTNesito  = -1;
050500131127         RETURN;
050600131127       When sqlCode = 100;
050700131127       When sqlCode > 0 and RTNopcode = ' ';
050800131127         RTNstatus = sqlCode;
050900131127         RTNopcode = 'TABRAS';
051000131127       Endsl;
051100131127
051200131127      /END-FREE
051300131127      *__________________scarico tabella ORP TNTBE ora stimata CONS/ORM ____
051400131127      * invia codice
051500131127     C/EXEC SQL
051600131204     C+ DECLARE TABORP CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 4) as idORP
051700131204     C+ FROM TNTBE00F WHERE tbeatb = ' ' and
051800131127     C+ tbecod = 'ORP' and tbelin = ' '
051900131127     C+ FOR READ ONLY
052000131127     C/END-EXEC
052100131127     C
052200131127     C/EXEC SQL
052300131127     C+ OPEN Taborp
052400131127     C/END-EXEC
052500131127      /FREE
052600131127
052700131127       select;
052800131127       When sqlCode < 0;
052900131127         RTNstatus = sqlCode;
053000131127         RTNopcode = 'SQLCODE';
053100131127         RTNesito  = -1;
053200131127         RETURN;
053300131127       When sqlCode = 100;
053400131127       When sqlCode > 0 and RTNopcode = ' ';
053500131127         RTNstatus = sqlCode;
053600131127         RTNopcode = 'TABORP';
053700131127       Endsl;
053800131127
053900131127      /END-FREE
054000070118     C/EXEC SQL
054100070118     C+ SET RESULT SETS for return to client
054200080428     c+                 cursor tblparam,
054300080428     c+                 cursor tntbe,
054400080409     c+                 cursor tab3e,
054500070118     c+                 cursor tab1p,
054600070118     c+                 cursor tab1a,
054700080214     c+                 cursor tab7p,
054800070118     c+                 cursor tab7q,
054900070118     c+                 cursor tab7r,
055000070118     c+                 cursor tab7s,
055100080220     c+                 cursor tab2a,
055200080222     c+                 cursor tab2a_riserva,
055300080220     c+                 cursor tabgch,
055400080214     c+                 cursor tabtsa,
055500101102     c+                 cursor tabdoc,
055600131127     c+                 cursor tabasa,
055700131127     c+                 cursor tabras,
055800131127     c+                 cursor taborp
055900070118     C/END-EXEC
056000070118      /FREE
056100070118
056200070118       IF sqlCode < 0;
056300070118         RTNstatus = sqlCode;
056400070118         RTNopcode = 'SQLCODE';
056500070118         RTNesito  = -1;
056600070118         RETURN;
056700070118       ENDIF;
056800070116
056900060918       RETURN;
057000060918
057100060918      /END-FREE
