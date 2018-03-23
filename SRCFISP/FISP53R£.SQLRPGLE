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
013600080222      * invia codice, decodifica,
013700080222      * se da inviare a PDA (2z tbluni pos.31)
013800080222     C/EXEC SQL
013900080307     C+ DECLARE TAB2A_RISERVA CURSOR FOR SELECT SUBSTR(a.TBLKEY, 1, 3) as
014000080307     C+ idriserv, substr(a.TBLUNI, 1, 20) as des FROM TABEL00F a join
014100080307     C+ tabel00f b on a.tblkey = b.tblkey WHERE a.tblflg = ' ' and
014200080307     C+ b.tblflg=' ' and a.tblcod = '2A' and b.tblcod='2Z' and
014300080307     C+ substr(b.tbluni, 31, 1) <> 'N' and substr(a.tbluni, 85, 1) = 'R'
014400080307     C+ AND a.TBLKUT = 1 and b.tblkut= 1 FOR READ ONLY
014500080222     C/END-EXEC
014600080222     C
014700080222     C/EXEC SQL
014800080222     C+ OPEN TAB2a_riserva
014900080222     C/END-EXEC
015000080222      /FREE
015100080222
015200080222       select;
015300080222       When sqlCode < 0;
015400080222         RTNstatus = sqlCode;
015500080222         RTNopcode = 'SQLCODE';
015600080222         RTNesito  = -1;
015700080222         RETURN;
015800080307       When sqlCode = 100;
015900080307       When sqlCode > 0 and RTNopcode = ' ';
016000080307         RTNstatus = sqlCode;
016100080307         RTNopcode = 'TAB2A_RIS';
016200080222       Endsl;
016300080222
016400080222      /END-FREE
016500070118
016600080222      *__________________scarico tabella 3E Anomalie spunte No quelle 8 ____
016700080214      * invia codice e decodifica solo se anomalia ancora utilizzata
016800080214      * (tbluni pos.36) e se da inviare a PDA (tbluni pos.37)
016900060918     C/EXEC SQL
017000080307     C+ DECLARE TAB3E CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 1) as idcan,
017100080307     C+ substr(TBLUNI, 1, 25) as des FROM TABEL00F WHERE tblflg = ' ' and
017200080307     C+ tblcod = '3E' and substr(tbluni, 36, 1) <> 'N' and substr(tbluni,
017300080307     C+ 37, 1) <> 'N' AND TBLKUT = 1 and substr(tbluni, 38, 1) <> '8' FOR
017400080307     C+ READ ONLY
017500060918     C/END-EXEC
017600061016     C
017700060918     C/EXEC SQL
017800070116     C+ OPEN TAB3E
017900060918     C/END-EXEC
018000060918      /FREE
018100060918
018200061017       select;
018300061017       When sqlCode < 0;
018400061017         RTNstatus = sqlCode;
018500061017         RTNopcode = 'SQLCODE';
018600061017         RTNesito  = -1;
018700060918         RETURN;
018800080307       When sqlCode = 100;
018900080307       When sqlCode > 0 and RTNopcode = ' ';
019000080307         RTNstatus = sqlCode;
019100080307         RTNopcode = 'TAB3E';
019200061017       Endsl;
019300060918
019400060918      /END-FREE
019500061017
019600080214      *__________________scarico tabella 1P Consegne particolari____________
019700080214      * invia codice e decodifica solo se utilizzabile in partenza
019800080214      * (tbluni pos.29) e se da inviare a PDA (tbluni pos.84)
019900070118     C/EXEC SQL
020000080307     C+ DECLARE TAB1P CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 1) as idtc ,
020100080307     C+ substr(TBLUNI, 1, 25) as des FROM TABEL00F WHERE tblflg = ' ' and
020200080307     C+ tblcod = '1P' and substr(tbluni, 29, 1) = 'S' and substr(tbluni,
020300080307     C+ 84, 1) <> 'N' AND TBLKUT = 1 FOR READ ONLY
020400070118     C/END-EXEC
020500070118     C
020600070118     C/EXEC SQL
020700070118     C+ OPEN TAB1P
020800070118     C/END-EXEC
020900070118      /FREE
021000070118
021100070118       select;
021200070118       When sqlCode < 0;
021300070118         RTNstatus = sqlCode;
021400070118         RTNopcode = 'SQLCODE';
021500070118         RTNesito  = -1;
021600070118         RETURN;
021700080307       When sqlCode = 100;
021800080307       When sqlCode > 0 and RTNopcode = ' ';
021900080307         RTNstatus = sqlCode;
022000080307         RTNopcode = 'TAB1P';
022100070118       Endsl;
022200070118
022300070118      /END-FREE
022400080214      *__________________scarico tabella 1A Tipo incasso____________________
022500080214      * invia codice, decodifica, flag tipo incasso e tipo intestazione
022600080214      * assegno solo se da inviare a PDA (tbluni pos.76)
022700070118     C/EXEC SQL
022800080307     C+ DECLARE TAB1A CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 2) as idtic,
022900080307     C+ substr(TBLUNI, 1, 25) as des, substr(TBLUNI, 27, 1) as fca,
023000080307     C+ substr(TBLUNI, 28, 1) as fmb, substr(TBLUNI, 32, 1) as tipsomma
023100080307     C+ FROM TABEL00F WHERE tblflg = ' ' and tblcod = '1A' and
023200080307     C+ substr(tbluni, 76, 1) <> 'N' AND TBLKUT = 1 FOR READ ONLY
023300070118     C/END-EXEC
023400070118     C
023500070118     C/EXEC SQL
023600070118     C+ OPEN TAB1A
023700070118     C/END-EXEC
023800070118      /FREE
023900070118
024000070118       select;
024100070118       When sqlCode < 0;
024200070118         RTNstatus = sqlCode;
024300070118         RTNopcode = 'SQLCODE';
024400070118         RTNesito  = -1;
024500070118         RETURN;
024600080307       When sqlCode = 100;
024700080307       When sqlCode > 0 and RTNopcode = ' ';
024800080307         RTNstatus = sqlCode;
024900080307         RTNopcode = 'TAB1A';
025000070118       Endsl;
025100070118      /END-FREE
025200080214      *__________________scarico tabella 7Q Particolarità giacenze _________
025300080214      * invia codice e nota LDV solo se è di tipo giacenza
025400080214      * (tblkey pos. 1=Q) stampabile in bolla (tblkey pos. 4=S) e
025500080214      * se da inviare a PDA (tbluni pos.89)
025600070118     C/EXEC SQL
025700080307     C+ DECLARE TAB7Q CURSOR FOR SELECT SUBSTR(TBLKEY, 2, 2) as idgga,
025800080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
025900080307     C+ tblcod = '7V' and substr(tblkey, 1, 1) = 'Q' and substr(tblkey,
026000080307     C+ 4, 1) = 'S' and substr(tbluni, 89, 1) <> 'N' AND TBLKUT = 1 FOR
026100080307     C+ READ ONLY
026200070118     C/END-EXEC
026300070118     C
026400070118     C/EXEC SQL
026500070118     C+ OPEN TAB7Q
026600070118     C/END-EXEC
026700070118      /FREE
026800070118
026900070118       select;
027000070118       When sqlCode < 0;
027100070118         RTNstatus = sqlCode;
027200070118         RTNopcode = 'SQLCODE';
027300070118         RTNesito  = -1;
027400070118         RETURN;
027500080307       When sqlCode = 100;
027600080307       When sqlCode > 0 and RTNopcode = ' ';
027700080307         RTNstatus = sqlCode;
027800080307         RTNopcode = 'TAB7Q';
027900070118       Endsl;
028000070118
028100070118      /END-FREE
028200070118
028300080214      *__________________scarico tabella 7R Particolarità consegne _________
028400080214      * invia codice e nota LDV solo se è di tipo consegna
028500080214      * (tblkey pos. 1=R) stampabile in bolla (tblkey pos. 4=S) e
028600080214      * se da inviare a PDA (tbluni pos.89)
028700070118     C/EXEC SQL
028800080307     C+ DECLARE TAB7R CURSOR FOR SELECT SUBSTR(TBLKEY, 2, 2) as idgma,
028900080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
029000080307     C+ tblcod = '7V' and substr(tblkey, 1, 1) = 'R' and substr(tblkey,
029100080307     C+ 4, 1) = 'S' and substr(tbluni, 89, 1) <> 'N' AND TBLKUT = 1 FOR
029200080307     C+ READ ONLY
029300070118     C/END-EXEC
029400070118     C
029500070118     C/EXEC SQL
029600070118     C+ OPEN TAB7R
029700070118     C/END-EXEC
029800070118      /FREE
029900070118
030000070118       select;
030100070118       When sqlCode < 0;
030200070118         RTNstatus = sqlCode;
030300070118         RTNopcode = 'SQLCODE';
030400070118         RTNesito  = -1;
030500070118         RETURN;
030600080307       When sqlCode = 100;
030700080307       When sqlCode > 0 and RTNopcode = ' ';
030800080307         RTNstatus = sqlCode;
030900080307         RTNopcode = 'TAB7R';
031000070118       Endsl;
031100070118
031200070118      /END-FREE
031300080214      *__________________scarico tabella 7S Particolarità varie ____________
031400080214      * invia codice e nota LDV solo se è di tipo varie
031500080214      * (tblkey pos. 1=S) stampabile in bolla (tblkey pos. 4=S) e
031600080214      * se da inviare a PDA (tbluni pos.89)
031700070118     C/EXEC SQL
031800080307     C+ DECLARE TAB7S CURSOR FOR SELECT SUBSTR(TBLKEY, 2, 2) as idgva,
031900080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
032000080307     C+ tblcod = '7V' and substr(tblkey, 1, 1) = 'S' and substr(tblkey,
032100080307     C+ 4, 1) = 'S' and substr(tbluni, 89, 1) <> 'N' AND TBLKUT = 1 FOR
032200080307     C+ READ ONLY
032300070118     C/END-EXEC
032400070118     C
032500070118     C/EXEC SQL
032600070118     C+ OPEN TAB7S
032700070118     C/END-EXEC
032800070118      /FREE
032900070118
033000070118       select;
033100070118       When sqlCode < 0;
033200070118         RTNstatus = sqlCode;
033300070118         RTNopcode = 'SQLCODE';
033400070118         RTNesito  = -1;
033500070118         RETURN;
033600080307       When sqlCode = 100;
033700080307       When sqlCode > 0 and RTNopcode = ' ';
033800080307         RTNstatus = sqlCode;
033900080307         RTNopcode = 'TAB7S';
034000070118       Endsl;
034100070118
034200070118      /END-FREE
034300080214      *__________________scarico tabella 7P particolarità contrassegno______
034400080214      * invia codice e decodifica
034500080214      * se da inviare a PDA (tbluni pos.89)
034600080214     C/EXEC SQL
034700080307     C+ DECLARE TAB7P CURSOR FOR SELECT SUBSTR(TBLKEY, 1, 2) as idgca,
034800080307     C+ substr(TBLUNI, 1, 50) as des FROM TABEL00F WHERE tblflg = ' ' and
034900080307     C+ tblcod = '7P' and substr(tbluni, 54, 1) <> 'N' AND TBLKUT = 1 FOR
035000080307     C+ READ ONLY
035100080214     C/END-EXEC
035200080214     C
035300080214     C/EXEC SQL
035400080214     C+ OPEN TAB7P
035500080214     C/END-EXEC
035600080214      /FREE
035700080214
035800080214       select;
035900080214       When sqlCode < 0;
036000080214         RTNstatus = sqlCode;
036100080214         RTNopcode = 'SQLCODE';
036200080214         RTNesito  = -1;
036300080307       When sqlCode = 100;
036400080214         RETURN;
036500080307       When sqlCode > 0 and RTNopcode = ' ';
036600080307         RTNstatus = sqlCode;
036700080307         RTNopcode = 'TAB7P';
036800080214       Endsl;
036900080214
037000080214      /END-FREE
037100080214      *__________________scarico tabella GCH TNTBE Giorni di chiusura ______
037200080214      * invia codice, decodifica solo se da inviare a PDA (tbluni pos.36)
037300070118     C/EXEC SQL
037400080307     C+ DECLARE TABGCH CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 2) as idgc,
037500080307     C+ substr(TBEUNI, 1, 35) as des FROM TNTBE00F WHERE tbeatb = ' ' and
037600080307     C+ tbecod = 'GCH' and tbelin = ' ' and substr(tbeuni, 36, 1) <> 'N'
037700080307     C+ FOR READ ONLY
037800070118     C/END-EXEC
037900070118     C
038000070118     C/EXEC SQL
038100070118     C+ OPEN Tabgch
038200070118     C/END-EXEC
038300070118      /FREE
038400070118
038500070118       select;
038600070118       When sqlCode < 0;
038700070118         RTNstatus = sqlCode;
038800070118         RTNopcode = 'SQLCODE';
038900070118         RTNesito  = -1;
039000070118         RETURN;
039100080307       When sqlCode = 100;
039200080307       When sqlCode > 0 and RTNopcode = ' ';
039300080307         RTNstatus = sqlCode;
039400080307         RTNopcode = 'TABGCH';
039500070118       Endsl;
039600070118
039700070118      /END-FREE
039800080214      *_______scarico tabella DOC TNTBE documenti da consegnare x PDA ______
039900080214      * invia codice, decodifica solo se da inviare a PDA (tbluni pos.41)
040000080214     C/EXEC SQL
040100080307     C+ DECLARE TABDOC CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as iddocum,
040200080307     C+ substr(TBEUNI, 1, 40) as des FROM TNTBE00F WHERE tbeatb = ' ' and
040300080307     C+ tbecod = 'DOC' and tbelin = ' ' and substr(tbeuni, 41, 1) <> 'N'
040400080307     C+ FOR READ ONLY
040500080214     C/END-EXEC
040600080214     C
040700080214     C/EXEC SQL
040800080214     C+ OPEN Tabdoc
040900080214     C/END-EXEC
041000080214      /FREE
041100080214
041200080214       select;
041300080214       When sqlCode < 0;
041400080214         RTNstatus = sqlCode;
041500080214         RTNopcode = 'SQLCODE';
041600080214         RTNesito  = -1;
041700080214         RETURN;
041800080307       When sqlCode = 100;
041900080307       When sqlCode > 0 and RTNopcode = ' ';
042000080307         RTNstatus = sqlCode;
042100080307         RTNopcode = 'TABDOC';
042200080214       Endsl;
042300080214
042400080214      /END-FREE
042500080214      *_______scarico tabella TSA TNTBE tipo servizio per aut. _____________
042600080214      * invia codice, decodifica solo se da inviare a PDA (tbluni pos.26)
042700080214     C/EXEC SQL
042800080307     C+ DECLARE TABTSA CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as
042900080307     C+ idserviz, substr(TBEUNI, 1, 25) as des FROM TNTBE00F WHERE tbeatb
043000080307     C+ = ' ' and tbecod = 'TSA' and tbelin = ' ' and substr(tbeuni, 26,
043100080307     C+ 1) <> 'N' FOR READ ONLY
043200080214     C/END-EXEC
043300080214     C
043400080214     C/EXEC SQL
043500080214     C+ OPEN Tabtsa
043600080214     C/END-EXEC
043700080214      /FREE
043800080214
043900080214       select;
044000080214       When sqlCode < 0;
044100080214         RTNstatus = sqlCode;
044200080214         RTNopcode = 'SQLCODE';
044300080214         RTNesito  = -1;
044400080214         RETURN;
044500080307       When sqlCode = 100;
044600080307       When sqlCode > 0 and RTNopcode = ' ';
044700080307         RTNstatus = sqlCode;
044800080307         RTNopcode = 'TABTSA';
044900080214       Endsl;
045000080214
045100080214      /END-FREE
045200101108      *__________________scarico tabella ASS TNTBE Assistenza Pda x SPC ____
045300101102      * invia codice, decodifica
045400101102     C/EXEC SQL
045500101102     C+ DECLARE TABASS CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as idass,
045600101108     C+ substr(TBEUNI, 1, 35) as des FROM TNTBE00F WHERE tbeatb = ' ' and
045700101102     C+ tbecod = 'ASS' and tbelin = ' '
045800101102     C+ FOR READ ONLY
045900101102     C/END-EXEC
046000101102     C
046100101102     C/EXEC SQL
046200101102     C+ OPEN Tabass
046300101102     C/END-EXEC
046400101102      /FREE
046500101102
046600101102       select;
046700101102       When sqlCode < 0;
046800101102         RTNstatus = sqlCode;
046900101102         RTNopcode = 'SQLCODE';
047000101102         RTNesito  = -1;
047100101102         RETURN;
047200101102       When sqlCode = 100;
047300101102       When sqlCode > 0 and RTNopcode = ' ';
047400101102         RTNstatus = sqlCode;
047500101102         RTNopcode = 'TABASS';
047600101102       Endsl;
047700101102
047800101102      /END-FREE
047900101108      *__________________scarico tabella ASA TNTBE Assistenza Pda x AUT ____
048000101108      * invia codice, decodifica
048100101108     C/EXEC SQL
048200101108     C+ DECLARE TABASA CURSOR FOR SELECT SUBSTR(TBEKE1, 1, 1) as idasa,
048300101108     C+ substr(TBEUNI, 1, 35) as des FROM TNTBE00F WHERE tbeatb = ' ' and
048400101108     C+ tbecod = 'ASA' and tbelin = ' '
048500101108     C+ FOR READ ONLY
048600101108     C/END-EXEC
048700101108     C
048800101108     C/EXEC SQL
048900101108     C+ OPEN Tabasa
049000101108     C/END-EXEC
049100101108      /FREE
049200101108
049300101108       select;
049400101108       When sqlCode < 0;
049500101108         RTNstatus = sqlCode;
049600101108         RTNopcode = 'SQLCODE';
049700101108         RTNesito  = -1;
049800101108         RETURN;
049900101108       When sqlCode = 100;
050000101108       When sqlCode > 0 and RTNopcode = ' ';
050100101108         RTNstatus = sqlCode;
050200101108         RTNopcode = 'TABASA';
050300101108       Endsl;
050400101108
050500101108      /END-FREE
050600070118     C/EXEC SQL
050700070118     C+ SET RESULT SETS for return to client
050800080428     c+                 cursor tblparam,
050900080428     c+                 cursor tntbe,
051000080409     c+                 cursor tab3e,
051100070118     c+                 cursor tab1p,
051200070118     c+                 cursor tab1a,
051300080214     c+                 cursor tab7p,
051400070118     c+                 cursor tab7q,
051500070118     c+                 cursor tab7r,
051600070118     c+                 cursor tab7s,
051700080220     c+                 cursor tab2a,
051800080222     c+                 cursor tab2a_riserva,
051900080220     c+                 cursor tabgch,
052000080214     c+                 cursor tabtsa,
052100101102     c+                 cursor tabdoc,
052200101108     c+                 cursor tabass,
052300101108     c+                 cursor tabasa
052400070118     C/END-EXEC
052500070118      /FREE
052600070118
052700070118       IF sqlCode < 0;
052800070118         RTNstatus = sqlCode;
052900070118         RTNopcode = 'SQLCODE';
053000070118         RTNesito  = -1;
053100070118         RETURN;
053200070118       ENDIF;
053300070116
053400060918       RETURN;
053500060918
053600060918      /END-FREE
