000100100226
000200130305/* Testo : CHIUSURA SOTTOSISTEMI X SWITCH O SPEGNIMENTO            */
001500070606
001600130305 TNSYENDC:   PGM
004100980502/* E CHIUDO I SOTTOSISTEMI                                    */
004300030325             ENDSBS     SBS(QINTER001) OPTION(*IMMED)
004400030325             MONMSG     MSGID(CPF0000)
004500030325             ENDSBS     SBS(QINTER002) OPTION(*IMMED)
004600030325             MONMSG     MSGID(CPF0000)
004700030325             ENDSBS     SBS(QINTER005) OPTION(*IMMED)
004800030325             MONMSG     MSGID(CPF0000)
004900030325             ENDSBS     SBS(QINTER006) OPTION(*IMMED)
005000030325             MONMSG     MSGID(CPF0000)
005100030325             ENDSBS     SBS(QINTER007) OPTION(*IMMED)
005200030325             MONMSG     MSGID(CPF0000)
005300030325             ENDSBS     SBS(QINTER015) OPTION(*IMMED)
005400030325             MONMSG     MSGID(CPF0000)
005500030325             ENDSBS     SBS(QINTER020) OPTION(*IMMED)
005600030325             MONMSG     MSGID(CPF0000)
005700030325             ENDSBS     SBS(QINTER026) OPTION(*IMMED)
005800030325             MONMSG     MSGID(CPF0000)
005900030325             ENDSBS     SBS(QINTER028) OPTION(*IMMED)
006000030325             MONMSG     MSGID(CPF0000)
006100030325             ENDSBS     SBS(QINTER031) OPTION(*IMMED)
006200030325             MONMSG     MSGID(CPF0000)
006300030325             ENDSBS     SBS(QINTER032) OPTION(*IMMED)
006400030325             MONMSG     MSGID(CPF0000)
006500030325             ENDSBS     SBS(QINTER043) OPTION(*IMMED)
006600030325             MONMSG     MSGID(CPF0000)
006700030411             ENDSBS     SBS(QINTER046) OPTION(*IMMED)
006800030411             MONMSG     MSGID(CPF0000)
006900030325             ENDSBS     SBS(QINTER049) OPTION(*IMMED)
007000030325             MONMSG     MSGID(CPF0000)
007100030325             ENDSBS     SBS(QINTER050) OPTION(*IMMED)
007200030325             MONMSG     MSGID(CPF0000)
007300030325             ENDSBS     SBS(QINTER059) OPTION(*IMMED)
007400030325             MONMSG     MSGID(CPF0000)
007500030325             ENDSBS     SBS(QINTER062) OPTION(*IMMED)
007600030325             MONMSG     MSGID(CPF0000)
007700030325             ENDSBS     SBS(QINTER063) OPTION(*IMMED)
007800030325             MONMSG     MSGID(CPF0000)
007900030325             ENDSBS     SBS(QINTER066) OPTION(*IMMED)
008000030325             MONMSG     MSGID(CPF0000)
008100030325             ENDSBS     SBS(QINTER067) OPTION(*IMMED)
008200030325             MONMSG     MSGID(CPF0000)
008300030325             ENDSBS     SBS(QINTER082) OPTION(*IMMED)
008400030325             MONMSG     MSGID(CPF0000)
008500030325             ENDSBS     SBS(QINTER089) OPTION(*IMMED)
008600030325             MONMSG     MSGID(CPF0000)
008700030325             ENDSBS     SBS(QINTER093) OPTION(*IMMED)
008800030325             MONMSG     MSGID(CPF0000)
008900071019             ENDSBS     SBS(QINTER099) OPTION(*IMMED)
009000071019             MONMSG     MSGID(CPF0000)
009100090916             ENDSBS     SBS(QINTER114) OPTION(*IMMED)
009200061206             MONMSG     MSGID(CPF0000)
009300090916             ENDSBS     SBS(QINTER116) OPTION(*IMMED)
009400090916             MONMSG     MSGID(CPF0000)
009500100930             ENDSBS     SBS(QINTER166) OPTION(*IMMED)
009600100930             MONMSG     MSGID(CPF0000)
009700030325             ENDSBS     SBS(QINTER199) OPTION(*IMMED)
009800030325             MONMSG     MSGID(CPF0000)
009900100930             ENDSBS     SBS(QINTER200) OPTION(*IMMED)
010000100930             MONMSG     MSGID(CPF0000)
010100030325             ENDSBS     SBS(QINTERXXX) OPTION(*IMMED)
010200021014             MONMSG     MSGID(CPF0000)
011000970701             ENDSBS     SBS(GAICMZ) DELAY(60)
011100970701             MONMSG     MSGID(CPF0000)
011300940726             ENDPJ      SBS(QCMN) PGM(GAICMZ/CMZ104C)
011400920201             MONMSG     MSGID(CPF0000)
011401130523             ADDLIBLE   LIB(GAITRAOBJ) POSITION(*LAST)
011402130523             MONMSG     MSGID(CPF0000)
011403130305             GAITRAOBJ/ENDPDFSRV
011404130305             MONMSG     MSGID(CPF0000)
011600070606             ENDSBS     SBS(WIFIBAR) DELAY(60)
011700070606             MONMSG     MSGID(CPF0000)
011900080219             ENDSBS     SBS(WIFIBARCON) DELAY(60)
012000080219             MONMSG     MSGID(CPF0000)
012200080219             ENDSBS     SBS(WIFIBARRIT) DELAY(60)
012300080219             MONMSG     MSGID(CPF0000)
012500090318             ENDSBS     SBS(VDLBAR) DELAY(60)
012600090318             MONMSG     MSGID(CPF0000)
012800920201             ENDSBS     SBS(QCMN) DELAY(60)
012900920201             MONMSG     MSGID(CPF0000)
013100040603             ENDSBS     SBS(QBATCH) OPTION(*CNTRLD) DELAY(60)
013200040603             MONMSG     MSGID(CPF0000)
013201130305             ENDSBS     SBS(QPGMR) OPTION(*CNTRLD) DELAY(60)
013202130305             MONMSG     MSGID(CPF0000)
013400940726             ENDSBS     SBS(QSNADS) DELAY(30)
013500920201             MONMSG     MSGID(CPF0000)
013600011229             ENDSBS     SBS(STRATEBA1) DELAY(30)
013700011229             MONMSG     MSGID(CPF0000)
013800030325             ENDSBS     SBS(STRATEBA7) DELAY(30)
013900030325             MONMSG     MSGID(CPF0000)
014000011229             ENDSBS     SBS(STRATEGI) DELAY(30)
014100011229             MONMSG     MSGID(CPF0000)
014200971129             ENDSBS     SBS(GAIDTAQ) DELAY(45)
014300950225             MONMSG     MSGID(CPF0000)
016100130305 /* FORZO CHIUSURA (ALCUNI TARDANO A CHIUDERE... */
016200130305             DLYJOB     DLY(090)
016300000228             ENDSBS     SBS(GAICMZ) OPTION(*IMMED)
016400000228             MONMSG     MSGID(CPF0000)
016500000228             ENDSBS     SBS(QCMN) OPTION(*IMMED)
016600000228             MONMSG     MSGID(CPF0000)
016700000228             ENDSBS     SBS(GAIDTAQ) OPTION(*IMMED)
016800000228             MONMSG     MSGID(CPF0000)
016900070606             ENDSBS     SBS(WIFIBAR) OPTION(*IMMED)
017000070606             MONMSG     MSGID(CPF0000)
017100080219             ENDSBS     SBS(WIFIBARCON) OPTION(*IMMED)
017200080219             MONMSG     MSGID(CPF0000)
017300080219             ENDSBS     SBS(WIFIBARRIT) OPTION(*IMMED)
017400080219             MONMSG     MSGID(CPF0000)
017500090318             ENDSBS     SBS(VDLBAR) OPTION(*IMMED)
017600090318             MONMSG     MSGID(CPF0000)
018100020708
018101130305             ENDSBS     SBS(QBATCH) OPTION(*IMMED)
018102130305             MONMSG     MSGID(CPF0000)
018103130305             ENDSBS     SBS(QPGMR) OPTION(*IMMED)
018104130305             MONMSG     MSGID(CPF0000)
018105130305             ENDSBS     SBS(QSNADS) OPTION(*IMMED)
018106130305             MONMSG     MSGID(CPF0000)
018107130305             ENDSBS     SBS(STRATEBA1) OPTION(*IMMED)
018108130305             MONMSG     MSGID(CPF0000)
018109130305             ENDSBS     SBS(STRATEBA7) OPTION(*IMMED)
018110130305             MONMSG     MSGID(CPF0000)
018111130305             ENDSBS     SBS(STRATEGI) OPTION(*IMMED)
018112130305             MONMSG     MSGID(CPF0000)
018113130305             ENDSBS     SBS(GAIDTAQ) OPTION(*IMMED)
018114130305             MONMSG     MSGID(CPF0000)
018115130523
018116130523             ENDHOSTSVR SERVER(*ALL) ENDACTCNN(*DATABASE)
018117130523             MONMSG     MSGID(CPF0000)
018120130523
030100130523             DLYJOB     DLY(45)
032300130305             ENDPGM
