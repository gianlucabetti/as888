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
004300090629     C+ SELECT case when a.tbeke2 <>' ' then 'A' else 'B' end as idparam ,
004400090629     C+ case when a.tbeke2 <> ' ' then substr(a.tbeuni, 1, 242) concat
004500090629     C+ substr(b.tbeuni, 1, 242) else substr(a.tbeuni, 1, 242) end as des
004600090629     C+ from tntbe00f as a left outer join (select * from tntbe00f where
004700090629     C+ tbecod = 'PSP' and tbeke1 = substr(:profilo, 3, 3) and tbeke2 =
004800090629     C+ '2') as b on a.tbecod = b.tbecod and a.tbeke1 = b.tbeke1 where
004900090629     C+ a.tbeatb = ' ' and a.tbecod = 'PSP' and (( a.tbeke1 =
005000090629     C+ substr(:profilo, 3, 3) and a.tbeke2 = '1') or (a.tbeke1 = '046'))
005100090629     C+ FOR READ ONLY
005200080409     C/END-EXEC
005300080409     C
005400080409     C/EXEC SQL
005500080428     C+ OPEN Tblparam
005600080409     C/END-EXEC
005700080409      /FREE
005800080409
005900080409       select;
006000080409       When sqlCode < 0;
006100080409         RTNstatus = sqlCode;
006200080409         RTNopcode = 'SQLCODE';
006300080409         RTNesito  = -1;
006400080409         RETURN;
006500080409       When sqlCode = 100;
006600080409       When sqlCode > 0 and RTNopcode = ' ';
006700080409         RTNstatus = sqlCode;
006800080428         RTNopcode = 'TBLPARAM';
006900080409       Endsl;
007000080409
007100080409      /END-FREE
007200080428      *___ scarico tabella CMR causali mancato ritiro ORM o chiusura ORM ___
007300080428     C/EXEC SQL
007400080428     C+ DECLARE TNTBE CURSOR FOR
007500080428     C+ SELECT SUBSTR(TBEKE1, 1, 3) as idcausa,
007600080428     C+ substr(TBEUNI, 1, 35) as descrizione,
007700080428     C+ substr(TBEUNI, 114, 1) as ormfsync,
007800080428     C+ substr(TBEUNI, 115, 1) as ormfsign
007900080428     C+ FROM TNTBE00F
008000080428     C+ WHERE tbecod = 'CMR' and tbelin = ' ' and substr(tbeuni, 113, 1) = 'S'
008100080428     C+ FOR READ ONLY
008200080428     C/END-EXEC
008300080428     C
008400080428     C/EXEC SQL
008500080428     C+ OPEN Tntbe
008600080428     C/END-EXEC
008700080428      /FREE
008800080428
008900080428       select;
009000080428       When sqlCode < 0;
009100080428         RTNstatus = sqlCode;
009200080428         RTNopcode = 'SQLCODE';
009300080428         RTNesito  = -1;
009400080428         RETURN;
009500080428       When sqlCode = 100;
009600080428       When sqlCode > 0 and RTNopcode = ' ';
009700080428         RTNstatus = sqlCode;
009800080428         RTNopcode = 'TNTBE';
009900080428       Endsl;
010000080428
010100080428      /END-FREE
010200080222      *_____________scarico tabella 2A + 2Z esiti consegna NO riserva ______
010300080307      * invia codice, decodifica, tipo mancata consegna, flag da inserire
010400080307      * data consegna richiesta, RITORNO INFO PDA SUBITO
010500080220      * se da inviare a PDA (2z tbluni pos.31)
010600080220     C/EXEC SQL
010700080307     C+ DECLARE TAB2A CURSOR FOR SELECT SUBSTR(a.TBLKEY, 1, 3) as idcmc,
010800080307     C+ substr(a.TBLUNI, 1, 50) as des, substr(a.TBLUNI, 79, 1) as ftc,
010900080307     C+ substr(b.TBLUNI, 17, 1) as dcr, substr(b.TBLUNI, 32, 1) as fsync
011000080307     C+ FROM TABEL00F a join tabel00f b on a.tblkey = b.tblkey WHERE
011100080307     C+ a.tblflg = ' ' and b.tblflg=' ' and a.tblcod = '2A' and
011200080307     C+ b.tblcod='2Z' and substr(b.tbluni, 31, 1) <> 'N' and
011300080311     C+ substr(a.tbluni, 85, 1) <> 'R' AND a.TBLKUT = 1 and b.tblkut= 1
011400080307     C+ FOR READ ONLY
011500080220     C/END-EXEC
011600080220     C
011700080220     C/EXEC SQL
011800080220     C+ OPEN TAB2a
011900080220     C/END-EXEC
012000080220      /FREE
012100080220
012200080220       select;
012300080220       When sqlCode < 0;
012400080220         RTNstatus = sqlCode;
012500080220         RTNopcode = 'SQLCODE';
012600080220         RTNesito  = -1;
012700080220         RETURN;
012800080307       When sqlCode = 100;
012900080307       When sqlCode > 0 and RTNopcode = ' ';
013000080307         RTNstatus = sqlCode;
013100080307         RTNopcode = 'TAB2A';
013200080220       Endsl;
013300080220
013400080220      /END-FREE
013500080222      *_____________scarico tabella 2A + 2Z solo riserva ___________________
013600131127      * invia codice, decodifica, obbligo note
013700080222      * se da inviare a PDA (2z tbluni pos.31)
013800080222     C/EXEC SQL
013900080307     C+ DECLARE TAB2A_RISERVA CURSOR FOR SELECT SUBSTR(a.TBLKEY, 1, 3) as
014000131127     C+ idriserv, substr(a.TBLUNI, 1, 20) as des,
014100131127     C+ substr(b.TBLUNI, 33, 1) as obbnote
014200131127     C+ FROM TABEL00F a join
014300080307     C+ tabel00f b on a.tblkey = b.tblkey WHERE a.tblflg = ' ' and
014400080307     C+ b.tblflg=' ' and a.tblcod = '2A' and b.tblcod='2Z' and
014500080307     C+ substr(b.tbluni, 31, 1) <> 'N' and substr(a.tbluni, 85, 1) = 'R'
014600080307     C+ AND a.TBLKUT = 1 and b.tblkut= 1 FOR READ ONLY
014700080222     C/END-EXEC
014800080222     C
014900080222     C/EXEC SQL
015000080222     C+ OPEN TAB2a_riserva
015100080222     C/END-EXEC
015200080222      /FREE
015300080222
015400080222       select;
015500080222       When sqlCode < 0;
015600080222         RTNstatus = sqlCode;
015700080222         RTNopcode = 'SQLCODE';
015800080222         RTNesito  = -1;
015900080222         RETURN;
016000080307       When sqlCode = 100;
016100080307       When sqlCode > 0 and RTNopcode = ' ';
016200080307         RTNstatus = sqlCode;
016300080307         RTNopcode = 'TAB2A_RIS';
016400080222       Endsl;
016500080222
016600080222      /END-FREE
016700070118
016800080222      *__________________scarico tabella 3E Anomalie spunte No quelle 8 ____
016900080214      * invia codice e decodifica solo se anomalia ancora utilizzata
017000080214      * (tbluni pos.36) e se da inviare a PDA (tbluni pos.37)
017100060918     C/EXEC SQL
017200080307     C+ DECLARE TAB3E CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 1) as idcan,
017300120709     C+ substr(TBLUNI, 1, 20) as des FROM TABEL00F WHERE tblflg = ' ' and
017400080307     C+ tblcod = '3E' and substr(tbluni, 36, 1) <> 'N' and substr(tbluni,
017500080307     C+ 37, 1) <> 'N' AND TBLKUT = 1 and substr(tbluni, 38, 1) <> '8' FOR
017600080307     C+ READ ONLY
017700060918     C/END-EXEC
017800061016     C
017900060918     C/EXEC SQL
018000070116     C+ OPEN TAB3E
018100060918     C/END-EXEC
018200060918      /FREE
018300060918
018400061017       select;
018500061017       When sqlCode < 0;
018600061017         RTNstatus = sqlCode;
018700061017         RTNopcode = 'SQLCODE';
018800061017         RTNesito  = -1;
018900060918         RETURN;
019000080307       When sqlCode = 100;
019100080307       When sqlCode > 0 and RTNopcode = ' ';
019200080307         RTNstatus = sqlCode;
019300080307         RTNopcode = 'TAB3E';
019400061017       Endsl;
019500060918
019600060918      /END-FREE
019700061017
019800080214      *__________________scarico tabella 1P Consegne particolari____________
019900080214      * invia codice e decodifica solo se utilizzabile in partenza
020000080214      * (tbluni pos.29) e se da inviare a PDA (tbluni pos.84)
020100070118     C/EXEC SQL
020200080307     C+ DECLARE TAB1P CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 1) as idtc ,
020300080307     C+ substr(TBLUNI, 1, 25) as des FROM TABEL00F WHERE tblflg = ' ' and
020400080307     C+ tblcod = '1P' and substr(tbluni, 29, 1) = 'S' and substr(tbluni,
020500080307     C+ 84, 1) <> 'N' AND TBLKUT = 1 FOR READ ONLY
020600070118     C/END-EXEC
020700070118     C
020800070118     C/EXEC SQL
020900070118     C+ OPEN TAB1P
021000070118     C/END-EXEC
021100070118      /FREE
021200070118
021300070118       select;
021400070118       When sqlCode < 0;
021500070118         RTNstatus = sqlCode;
021600070118         RTNopcode = 'SQLCODE';
021700070118         RTNesito  = -1;
021800070118         RETURN;
021900080307       When sqlCode = 100;
022000080307       When sqlCode > 0 and RTNopcode = ' ';
022100080307         RTNstatus = sqlCode;
022200080307         RTNopcode = 'TAB1P';
022300070118       Endsl;
022400070118
022500070118      /END-FREE
022600080214      *__________________scarico tabella 1A Tipo incasso____________________
022700080214      * invia codice, decodifica, flag tipo incasso e tipo intestazione
022800080214      * assegno solo se da inviare a PDA (tbluni pos.76)
022900070118     C/EXEC SQL
023000080307     C+ DECLARE TAB1A CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 2) as idtic,
023100080307     C+ substr(TBLUNI, 1, 25) as des, substr(TBLUNI, 27, 1) as fca,
023200080307     C+ substr(TBLUNI, 28, 1) as fmb, substr(TBLUNI, 32, 1) as tipsomma
023300080307     C+ FROM TABEL00F WHERE tblflg = ' ' and tblcod = '1A' and
023400080307     C+ substr(tbluni, 76, 1) <> 'N' AND TBLKUT = 1 FOR READ ONLY
023500070118     C/END-EXEC
023600070118     C
023700070118     C/EXEC SQL
023800070118     C+ OPEN TAB1A
023900070118     C/END-EXEC
024000070118      /FREE
024100070118
024200070118       select;
024300070118       When sqlCode < 0;
024400070118         RTNstatus = sqlCode;
024500070118         RTNopcode = 'SQLCODE';
024600070118         RTNesito  = -1;
024700070118         RETURN;
024800080307       When sqlCode = 100;
024900080307       When sqlCode > 0 and RTNopcode = ' ';
025000080307         RTNstatus = sqlCode;
025100080307         RTNopcode = 'TAB1A';
025200070118       Endsl;
025300070118      /END-FREE
025400080214      *__________________scarico tabella 7Q Particolarità giacenze _________
025500080214      * invia codice e nota LDV solo se è di tipo giacenza
025600080214      * (tblkey pos. 1=Q) stampabile in bolla (tblkey pos. 4=S) e
025700080214      * se da inviare a PDA (tbluni pos.89)
025800070118     C/EXEC SQL
025900080307     C+ DECLARE TAB7Q CURSOR FOR SELECT SUBSTR(TBLKEY, 2, 2) as idgga,
026000080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
026100080307     C+ tblcod = '7V' and substr(tblkey, 1, 1) = 'Q' and substr(tblkey,
026200080307     C+ 4, 1) = 'S' and substr(tbluni, 89, 1) <> 'N' AND TBLKUT = 1 FOR
026300080307     C+ READ ONLY
026400070118     C/END-EXEC
026500070118     C
026600070118     C/EXEC SQL
026700070118     C+ OPEN TAB7Q
026800070118     C/END-EXEC
026900070118      /FREE
027000070118
027100070118       select;
027200070118       When sqlCode < 0;
027300070118         RTNstatus = sqlCode;
027400070118         RTNopcode = 'SQLCODE';
027500070118         RTNesito  = -1;
027600070118         RETURN;
027700080307       When sqlCode = 100;
027800080307       When sqlCode > 0 and RTNopcode = ' ';
027900080307         RTNstatus = sqlCode;
028000080307         RTNopcode = 'TAB7Q';
028100070118       Endsl;
028200070118
028300070118      /END-FREE
028400070118
028500080214      *__________________scarico tabella 7R Particolarità consegne _________
028600080214      * invia codice e nota LDV solo se è di tipo consegna
028700080214      * (tblkey pos. 1=R) stampabile in bolla (tblkey pos. 4=S) e
028800080214      * se da inviare a PDA (tbluni pos.89)
028900070118     C/EXEC SQL
029000080307     C+ DECLARE TAB7R CURSOR FOR SELECT SUBSTR(TBLKEY, 2, 2) as idgma,
029100080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
029200080307     C+ tblcod = '7V' and substr(tblkey, 1, 1) = 'R' and substr(tblkey,
029300080307     C+ 4, 1) = 'S' and substr(tbluni, 89, 1) <> 'N' AND TBLKUT = 1 FOR
029400080307     C+ READ ONLY
029500070118     C/END-EXEC
029600070118     C
029700070118     C/EXEC SQL
029800070118     C+ OPEN TAB7R
029900070118     C/END-EXEC
030000070118      /FREE
030100070118
030200070118       select;
030300070118       When sqlCode < 0;
030400070118         RTNstatus = sqlCode;
030500070118         RTNopcode = 'SQLCODE';
030600070118         RTNesito  = -1;
030700070118         RETURN;
030800080307       When sqlCode = 100;
030900080307       When sqlCode > 0 and RTNopcode = ' ';
031000080307         RTNstatus = sqlCode;
031100080307         RTNopcode = 'TAB7R';
031200070118       Endsl;
031300070118
031400070118      /END-FREE
031500080214      *__________________scarico tabella 7S Particolarità varie ____________
031600080214      * invia codice e nota LDV solo se è di tipo varie
031700080214      * (tblkey pos. 1=S) stampabile in bolla (tblkey pos. 4=S) e
031800080214      * se da inviare a PDA (tbluni pos.89)
031900070118     C/EXEC SQL
032000080307     C+ DECLARE TAB7S CURSOR FOR SELECT SUBSTR(TBLKEY, 2, 2) as idgva,
032100080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
032200080307     C+ tblcod = '7V' and substr(tblkey, 1, 1) = 'S' and substr(tblkey,
032300080307     C+ 4, 1) = 'S' and substr(tbluni, 89, 1) <> 'N' AND TBLKUT = 1 FOR
032400080307     C+ READ ONLY
032500070118     C/END-EXEC
032600070118     C
032700070118     C/EXEC SQL
032800070118     C+ OPEN TAB7S
032900070118     C/END-EXEC
033000070118      /FREE
033100070118
033200070118       select;
033300070118       When sqlCode < 0;
033400070118         RTNstatus = sqlCode;
033500070118         RTNopcode = 'SQLCODE';
033600070118         RTNesito  = -1;
033700070118         RETURN;
033800080307       When sqlCode = 100;
033900080307       When sqlCode > 0 and RTNopcode = ' ';
034000080307         RTNstatus = sqlCode;
034100080307         RTNopcode = 'TAB7S';
034200070118       Endsl;
034300070118
034400070118      /END-FREE
034500080214      *__________________scarico tabella 7P particolarità contrassegno______
034600080214      * invia codice e decodifica
034700080214      * se da inviare a PDA (tbluni pos.89)
034800080214     C/EXEC SQL
034900080307     C+ DECLARE TAB7P CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 2) as idgca,
035000080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
035100080307     C+ tblcod = '7P' and substr(tbluni, 54, 1) <> 'N' AND TBLKUT = 1 FOR
035200080307     C+ READ ONLY
035300080214     C/END-EXEC
035400080214     C
035500080214     C/EXEC SQL
035600080214     C+ OPEN TAB7P
035700080214     C/END-EXEC
035800080214      /FREE
035900080214
036000080214       select;
036100080214       When sqlCode < 0;
036200080214         RTNstatus = sqlCode;
036300080214         RTNopcode = 'SQLCODE';
036400080214         RTNesito  = -1;
036500080307       When sqlCode = 100;
036600080214         RETURN;
036700080307       When sqlCode > 0 and RTNopcode = ' ';
036800080307         RTNstatus = sqlCode;
036900080307         RTNopcode = 'TAB7P';
037000080214       Endsl;
037100080214
037200080214      /END-FREE
037300080214      *__________________scarico tabella GCH TNTBE Giorni di chiusura ______
037400080214      * invia codice, decodifica solo se da inviare a PDA (tbluni pos.36)
037500070118     C/EXEC SQL
037600080307     C+ DECLARE TABGCH CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 2) as idgc,
037700080307     C+ substr(TBEUNI, 1, 35) as des FROM TNTBE00F WHERE tbeatb = ' ' and
037800080307     C+ tbecod = 'GCH' and tbelin = ' ' and substr(tbeuni, 36, 1) <> 'N'
037900080307     C+ FOR READ ONLY
038000070118     C/END-EXEC
038100070118     C
038200070118     C/EXEC SQL
038300070118     C+ OPEN Tabgch
038400070118     C/END-EXEC
038500070118      /FREE
038600070118
038700070118       select;
038800070118       When sqlCode < 0;
038900070118         RTNstatus = sqlCode;
039000070118         RTNopcode = 'SQLCODE';
039100070118         RTNesito  = -1;
039200070118         RETURN;
039300080307       When sqlCode = 100;
039400080307       When sqlCode > 0 and RTNopcode = ' ';
039500080307         RTNstatus = sqlCode;
039600080307         RTNopcode = 'TABGCH';
039700070118       Endsl;
039800070118
039900070118      /END-FREE
040000080214      *_______scarico tabella DOC TNTBE documenti da consegnare x PDA ______
040100080214      * invia codice, decodifica solo se da inviare a PDA (tbluni pos.41)
040200080214     C/EXEC SQL
040300080307     C+ DECLARE TABDOC CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as iddocum,
040400080307     C+ substr(TBEUNI, 1, 40) as des FROM TNTBE00F WHERE tbeatb = ' ' and
040500080307     C+ tbecod = 'DOC' and tbelin = ' ' and substr(tbeuni, 41, 1) <> 'N'
040600080307     C+ FOR READ ONLY
040700080214     C/END-EXEC
040800080214     C
040900080214     C/EXEC SQL
041000080214     C+ OPEN Tabdoc
041100080214     C/END-EXEC
041200080214      /FREE
041300080214
041400080214       select;
041500080214       When sqlCode < 0;
041600080214         RTNstatus = sqlCode;
041700080214         RTNopcode = 'SQLCODE';
041800080214         RTNesito  = -1;
041900080214         RETURN;
042000080307       When sqlCode = 100;
042100080307       When sqlCode > 0 and RTNopcode = ' ';
042200080307         RTNstatus = sqlCode;
042300080307         RTNopcode = 'TABDOC';
042400080214       Endsl;
042500080214
042600080214      /END-FREE
042700080214      *_______scarico tabella TSA TNTBE tipo servizio per aut. _____________
042800080214      * invia codice, decodifica solo se da inviare a PDA (tbluni pos.26)
042900080214     C/EXEC SQL
043000080307     C+ DECLARE TABTSA CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as
043100080307     C+ idserviz, substr(TBEUNI, 1, 25) as des FROM TNTBE00F WHERE tbeatb
043200080307     C+ = ' ' and tbecod = 'TSA' and tbelin = ' ' and substr(tbeuni, 26,
043300080307     C+ 1) <> 'N' FOR READ ONLY
043400080214     C/END-EXEC
043500080214     C
043600080214     C/EXEC SQL
043700080214     C+ OPEN Tabtsa
043800080214     C/END-EXEC
043900080214      /FREE
044000080214
044100080214       select;
044200080214       When sqlCode < 0;
044300080214         RTNstatus = sqlCode;
044400080214         RTNopcode = 'SQLCODE';
044500080214         RTNesito  = -1;
044600080214         RETURN;
044700080307       When sqlCode = 100;
044800080307       When sqlCode > 0 and RTNopcode = ' ';
044900080307         RTNstatus = sqlCode;
045000080307         RTNopcode = 'TABTSA';
045100080214       Endsl;
045200080214
045300080214      /END-FREE
045400131127      *__________________scarico tabella ASA TNTBE Assistenza Pda x CON ____
045500101108      * invia codice, decodifica
045600101108     C/EXEC SQL
045700101108     C+ DECLARE TABASA CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as idasa,
045800101108     C+ substr(TBEUNI, 1, 35) as des FROM TNTBE00F WHERE tbeatb = ' ' and
045900101108     C+ tbecod = 'ASA' and tbelin = ' '
046000101108     C+ FOR READ ONLY
046100101108     C/END-EXEC
046200101108     C
046300101108     C/EXEC SQL
046400101108     C+ OPEN Tabasa
046500101108     C/END-EXEC
046600101108      /FREE
046700101108
046800101108       select;
046900101108       When sqlCode < 0;
047000101108         RTNstatus = sqlCode;
047100101108         RTNopcode = 'SQLCODE';
047200101108         RTNesito  = -1;
047300101108         RETURN;
047400101108       When sqlCode = 100;
047500101108       When sqlCode > 0 and RTNopcode = ' ';
047600101108         RTNstatus = sqlCode;
047700101108         RTNopcode = 'TABASA';
047800101108       Endsl;
047900101108
048000101108      /END-FREE
048100131127      *__________________scarico tabella RAS TNTBE Assistenza Pda x ORM ____
048200131127      * invia codice, decodifica
048300131127     C/EXEC SQL
048400131127     C+ DECLARE TABRAS CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as idRAS,
048500131127     C+ substr(TBEUNI, 1, 35) as des FROM TNTBE00F WHERE tbeatb = ' ' and
048600131127     C+ tbecod = 'RAS' and tbelin = ' ' and substr(tbeuni, 36,
048700131127     C+ 1) = 'S' FOR READ ONLY
048800131127     C/END-EXEC
048900131127     C
049000131127     C/EXEC SQL
049100131127     C+ OPEN Tabras
049200131127     C/END-EXEC
049300131127      /FREE
049400131127
049500131127       select;
049600131127       When sqlCode < 0;
049700131127         RTNstatus = sqlCode;
049800131127         RTNopcode = 'SQLCODE';
049900131127         RTNesito  = -1;
050000131127         RETURN;
050100131127       When sqlCode = 100;
050200131127       When sqlCode > 0 and RTNopcode = ' ';
050300131127         RTNstatus = sqlCode;
050400131127         RTNopcode = 'TABRAS';
050500131127       Endsl;
050600131127
050700131127      /END-FREE
050800131127      *__________________scarico tabella ORP TNTBE ora stimata CONS/ORM ____
050900131127      * invia codice
051000131127     C/EXEC SQL
051100131204     C+ DECLARE TABORP CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 4) as idORP
051200131204     C+ FROM TNTBE00F WHERE tbeatb = ' ' and
051300131127     C+ tbecod = 'ORP' and tbelin = ' '
051400131127     C+ FOR READ ONLY
051500131127     C/END-EXEC
051600131127     C
051700131127     C/EXEC SQL
051800131127     C+ OPEN Taborp
051900131127     C/END-EXEC
052000131127      /FREE
052100131127
052200131127       select;
052300131127       When sqlCode < 0;
052400131127         RTNstatus = sqlCode;
052500131127         RTNopcode = 'SQLCODE';
052600131127         RTNesito  = -1;
052700131127         RETURN;
052800131127       When sqlCode = 100;
052900131127       When sqlCode > 0 and RTNopcode = ' ';
053000131127         RTNstatus = sqlCode;
053100131127         RTNopcode = 'TABORP';
053200131127       Endsl;
053300131127
053400131127      /END-FREE
053500070118     C/EXEC SQL
053600070118     C+ SET RESULT SETS for return to client
053700080428     c+                 cursor tblparam,
053800080428     c+                 cursor tntbe,
053900080409     c+                 cursor tab3e,
054000070118     c+                 cursor tab1p,
054100070118     c+                 cursor tab1a,
054200080214     c+                 cursor tab7p,
054300070118     c+                 cursor tab7q,
054400070118     c+                 cursor tab7r,
054500070118     c+                 cursor tab7s,
054600080220     c+                 cursor tab2a,
054700080222     c+                 cursor tab2a_riserva,
054800080220     c+                 cursor tabgch,
054900080214     c+                 cursor tabtsa,
055000101102     c+                 cursor tabdoc,
055100131127     c+                 cursor tabasa,
055200131127     c+                 cursor tabras,
055300131127     c+                 cursor taborp
055400070118     C/END-EXEC
055500070118      /FREE
055600070118
055700070118       IF sqlCode < 0;
055800070118         RTNstatus = sqlCode;
055900070118         RTNopcode = 'SQLCODE';
056000070118         RTNesito  = -1;
056100070118         RETURN;
056200070118       ENDIF;
056300070116
056400060918       RETURN;
056500060918
056600060918      /END-FREE
