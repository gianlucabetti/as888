000100150424-- default -> contrassegni
000200150424INSERT INTO ANCBA00F
000300150424SELECT I.CBASOCIETA, I.CBAKCC, I.CBAKSC, '', '01.01.0001', I.CBACCB, I.CBAABI, I.CBACAB,
000400150424I.CBAENTITA, I.CBACLIFOR, 'CA', CURRENT DATE, I.CBACDCTRL, I.CBAKCCNSBA, I.CBAKSCNSBA, I.CBAIBAN,
000500150424I.CBAFLAG, I.CBACIN, I.CBABIC, I.CBASTATO, I.CBABANK, I.CBABRANCH
000600150424FROM ANCBA00F AS I
000700150424EXCEPTION JOIN ANCBA00F AS O ON I.CBASOCIETA = O.CBASOCIETA AND I.CBACLIFOR = O.CBACLIFOR AND
000800150424I.CBAKCC = O.CBAKCC AND I.CBAKSC = O.CBAKSC AND I.CBAENTITA = O.CBAENTITA AND 'CA' = O.CBATPAPPO
000900150424WHERE I.CBASOCIETA = '201' AND I.CBACLIFOR = 'C' AND I.CBATPAPPO = '' AND (I.CBAIBAN <> '' OR
001000150424I.CBABIC <> '')
001100150424;
001200150424-- nota accredito -> contrassegni
001300150424INSERT INTO ANCBA00F
001400150424SELECT I.CBASOCIETA, I.CBAKCC, I.CBAKSC, '', '01.01.0001', I.CBACCB, I.CBAABI, I.CBACAB,
001500150424I.CBAENTITA, I.CBACLIFOR, 'CA', CURRENT DATE, I.CBACDCTRL, I.CBAKCCNSBA, I.CBAKSCNSBA, I.CBAIBAN,
001600150424I.CBAFLAG, I.CBACIN, I.CBABIC, I.CBASTATO, I.CBABANK, I.CBABRANCH
001700150424FROM ANCBA00F AS I
001800150424EXCEPTION JOIN ANCBA00F AS O ON I.CBASOCIETA = O.CBASOCIETA AND I.CBACLIFOR = O.CBACLIFOR AND
001900150424I.CBAKCC = O.CBAKCC AND I.CBAKSC = O.CBAKSC AND I.CBAENTITA = O.CBAENTITA AND 'CA' = O.CBATPAPPO
002000150424WHERE I.CBASOCIETA = '201' AND I.CBACLIFOR = 'C' AND I.CBATPAPPO = 'NA' AND (I.CBAIBAN <> '' OR
002100150424I.CBABIC <> '')
002200150424;
002300150424-- nota accredito -> danni
002400150424INSERT INTO ANCBA00F
002500150424SELECT I.CBASOCIETA, I.CBAKCC, I.CBAKSC, '', '01.01.0001', I.CBACCB, I.CBAABI, I.CBACAB,
002600150424I.CBAENTITA, I.CBACLIFOR, 'DN', CURRENT DATE, I.CBACDCTRL, I.CBAKCCNSBA, I.CBAKSCNSBA, I.CBAIBAN,
002700150424I.CBAFLAG, I.CBACIN, I.CBABIC, I.CBASTATO, I.CBABANK, I.CBABRANCH
002800150424FROM ANCBA00F AS I
002900150424EXCEPTION JOIN ANCBA00F AS O ON I.CBASOCIETA = O.CBASOCIETA AND I.CBACLIFOR = O.CBACLIFOR AND
003000150424I.CBAKCC = O.CBAKCC AND I.CBAKSC = O.CBAKSC AND I.CBAENTITA = O.CBAENTITA AND 'DN' = O.CBATPAPPO
003100150424WHERE I.CBASOCIETA = '201' AND I.CBACLIFOR = 'C' AND I.CBATPAPPO = 'NA' AND (I.CBAIBAN <> '' OR
003200150424I.CBABIC <> '')
003300150424;
003400150424-- danni -> contrassegni
003500150424INSERT INTO ANCBA00F
003600150424SELECT I.CBASOCIETA, I.CBAKCC, I.CBAKSC, '', '01.01.0001', I.CBACCB, I.CBAABI, I.CBACAB,
003700150424I.CBAENTITA, I.CBACLIFOR, 'CA', CURRENT DATE, I.CBACDCTRL, I.CBAKCCNSBA, I.CBAKSCNSBA, I.CBAIBAN,
003800150424I.CBAFLAG, I.CBACIN, I.CBABIC, I.CBASTATO, I.CBABANK, I.CBABRANCH
003900150424FROM ANCBA00F AS I
004000150424EXCEPTION JOIN ANCBA00F AS O ON I.CBASOCIETA = O.CBASOCIETA AND I.CBACLIFOR = O.CBACLIFOR AND
004100150424I.CBAKCC = O.CBAKCC AND I.CBAKSC = O.CBAKSC AND I.CBAENTITA = O.CBAENTITA AND 'CA' = O.CBATPAPPO
004200150424WHERE I.CBASOCIETA = '201' AND I.CBACLIFOR = 'C' AND I.CBATPAPPO = 'DN' AND (I.CBAIBAN <> '' OR
004300150424I.CBABIC <> '')
004400150424;
004500150424-- danni -> note accredito
004600150424INSERT INTO ANCBA00F
004700150424SELECT I.CBASOCIETA, I.CBAKCC, I.CBAKSC, '', '01.01.0001', I.CBACCB, I.CBAABI, I.CBACAB,
004800150424I.CBAENTITA, I.CBACLIFOR, 'NA', CURRENT DATE, I.CBACDCTRL, I.CBAKCCNSBA, I.CBAKSCNSBA, I.CBAIBAN,
004900150424I.CBAFLAG, I.CBACIN, I.CBABIC, I.CBASTATO, I.CBABANK, I.CBABRANCH
005000150424FROM ANCBA00F AS I
005100150424EXCEPTION JOIN ANCBA00F AS O ON I.CBASOCIETA = O.CBASOCIETA AND I.CBACLIFOR = O.CBACLIFOR AND
005200150424I.CBAKCC = O.CBAKCC AND I.CBAKSC = O.CBAKSC AND I.CBAENTITA = O.CBAENTITA AND 'NA' = O.CBATPAPPO
005300150424WHERE I.CBASOCIETA = '201' AND I.CBACLIFOR = 'C' AND I.CBATPAPPO = 'DN' AND (I.CBAIBAN <> '' OR
005400150424I.CBABIC <> '')
005500150424;
005600150424-- contrassegni -> danni
005700150424INSERT INTO ANCBA00F
005800150424SELECT I.CBASOCIETA, I.CBAKCC, I.CBAKSC, '', '01.01.0001', I.CBACCB, I.CBAABI, I.CBACAB,
005900150424I.CBAENTITA, I.CBACLIFOR, 'DN', CURRENT DATE, I.CBACDCTRL, I.CBAKCCNSBA, I.CBAKSCNSBA, I.CBAIBAN,
006000150424I.CBAFLAG, I.CBACIN, I.CBABIC, I.CBASTATO, I.CBABANK, I.CBABRANCH
006100150424FROM ANCBA00F AS I
006200150424EXCEPTION JOIN ANCBA00F AS O ON I.CBASOCIETA = O.CBASOCIETA AND I.CBACLIFOR = O.CBACLIFOR AND
006300150424I.CBAKCC = O.CBAKCC AND I.CBAKSC = O.CBAKSC AND I.CBAENTITA = O.CBAENTITA AND 'DN' = O.CBATPAPPO
006400150424WHERE I.CBASOCIETA = '201' AND I.CBACLIFOR = 'C' AND I.CBATPAPPO = 'CA' AND (I.CBAIBAN <> '' OR
006500150424I.CBABIC <> '')
006600150424;
006700150424-- contrassegni -> note accredito
006800150424INSERT INTO ANCBA00F
006900150424SELECT I.CBASOCIETA, I.CBAKCC, I.CBAKSC, '', '01.01.0001', I.CBACCB, I.CBAABI, I.CBACAB,
007000150424I.CBAENTITA, I.CBACLIFOR, 'NA', CURRENT DATE, I.CBACDCTRL, I.CBAKCCNSBA, I.CBAKSCNSBA, I.CBAIBAN,
007100150424I.CBAFLAG, I.CBACIN, I.CBABIC, I.CBASTATO, I.CBABANK, I.CBABRANCH
007200150424FROM ANCBA00F AS I
007300150424EXCEPTION JOIN ANCBA00F AS O ON I.CBASOCIETA = O.CBASOCIETA AND I.CBACLIFOR = O.CBACLIFOR AND
007400150424I.CBAKCC = O.CBAKCC AND I.CBAKSC = O.CBAKSC AND I.CBAENTITA = O.CBAENTITA AND 'NA' = O.CBATPAPPO
007500150424WHERE I.CBASOCIETA = '201' AND I.CBACLIFOR = 'C' AND I.CBATPAPPO = 'CA' AND (I.CBAIBAN <> '' OR
007600150424I.CBABIC <> '')
007700150424;
007800150424-- tipo pagamento contrassegno = bonifico nazionale
007900150424UPDATE YBACL00F
008000150424SET ACLTPC = '2', ACLDPS = CASE WHEN ACLDPS BETWEEN 19000000 AND 19150000 THEN ACLDPS + 01000000
008100150424ELSE ACLDPS END
008200150424WHERE ACLSOCIETA = '201' AND ACLTPC = '' AND ACLKSC IN (SELECT CBAKSC FROM ANCBA00F WHERE CBASOCIETA
008300150424 = '201' AND CBACLIFOR = 'C' AND CBATPAPPO = 'CA' AND LEFT(CBAIBAN, 2) IN ('IT','SM'))
008400150424;
008500150424-- tipo pagamento contrassegno = bonifico estero.
008600150424UPDATE YBACL00F
008700150424SET ACLTPC = '6', ACLDPS = CASE WHEN ACLDPS BETWEEN 19000000 AND 19150000 THEN ACLDPS + 01000000
008800150424ELSE ACLDPS END
008900150424WHERE ACLSOCIETA = '201' AND ACLTPC = '' AND ACLKSC IN (SELECT CBAKSC FROM ANCBA00F WHERE CBASOCIETA
009000150424 = '201' AND CBACLIFOR = 'C' AND CBATPAPPO = 'CA' AND (LEFT(CBAIBAN, 2) NOT IN ('IT','SM','') OR
009100150424SUBSTR(CBABIC, 5, 2) NOT IN ('IT','SM','')) )
009200150424;
009300150424-- tipo pagamento danni = bonifico nazionale
009400150424UPDATE YBACL00F
009500150424SET ACLTIC = '2' CONCAT SUBSTR(ACLTIC, 2), ACLDPS = CASE WHEN ACLDPS BETWEEN 19000000 AND 19150000
009600150424THEN ACLDPS + 01000000 ELSE ACLDPS END
009700150424WHERE ACLSOCIETA = '201' AND SUBSTR(ACLTIC, 1, 1) = '' AND ACLKSC IN (SELECT CBAKSC FROM ANCBA00F
009800150424WHERE CBASOCIETA = '201' AND CBACLIFOR = 'C' AND CBATPAPPO = 'DN' AND LEFT(CBAIBAN, 2) IN
009900150424('IT','SM'))
010000150424;
010100150424-- tipo pagamento danni = bonifico estero.
010200150424UPDATE YBACL00F
010300150424SET ACLTIC = '6' CONCAT SUBSTR(ACLTIC, 2), ACLDPS = CASE WHEN ACLDPS BETWEEN 19000000 AND 19150000
010400150424THEN ACLDPS + 01000000 ELSE ACLDPS END
010500150424WHERE ACLSOCIETA = '201' AND SUBSTR(ACLTIC, 1, 1) = '' AND ACLKSC IN (SELECT CBAKSC FROM ANCBA00F
010600150424WHERE CBASOCIETA = '201' AND CBACLIFOR = 'C' AND CBATPAPPO = 'DN' AND (LEFT(CBAIBAN, 2) NOT IN
010700150424('IT','SM','') OR SUBSTR(CBABIC, 5, 2) NOT IN ('IT','SM','')) )
010800150424;
010900150424-- tipo pagamento nota accredito = bonifico nazionale
011000150424UPDATE YBACL00F
011100150424SET ACLTIC = SUBSTR(ACLTIC, 1, 1) CONCAT '2', ACLDPS = CASE WHEN ACLDPS BETWEEN 19000000 AND
01120015042419150000 THEN ACLDPS + 01000000 ELSE ACLDPS END
011300150424WHERE ACLSOCIETA = '201' AND SUBSTR(ACLTIC, 2, 1) = '' AND ACLKSC IN (SELECT CBAKSC FROM ANCBA00F
011400150424WHERE CBASOCIETA = '201' AND CBACLIFOR = 'C' AND CBATPAPPO = 'NA' AND LEFT(CBAIBAN, 2) IN
011500150424('IT','SM'))
011600150424;
011700150424-- tipo pagamento nota accredito = bonifico estero
011800150424UPDATE YBACL00F
011900150424SET ACLTIC = SUBSTR(ACLTIC, 1, 1) CONCAT '6', ACLDPS = CASE WHEN ACLDPS = 0 THEN 0 WHEN ACLDPS <
01200015042419150000 THEN ACLDPS + 01000000 ELSE ACLDPS END
012100150424WHERE ACLSOCIETA = '201' AND SUBSTR(ACLTIC, 2, 1) = '' AND ACLKSC IN (SELECT CBAKSC FROM ANCBA00F
012200150424WHERE CBASOCIETA = '201' AND CBACLIFOR = 'C' AND CBATPAPPO = 'NA' AND (LEFT(CBAIBAN, 2) NOT IN
012300150424('IT','SM','') OR SUBSTR(CBABIC, 5, 2) NOT IN ('IT','SM','')))
012400150424;
