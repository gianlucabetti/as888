000100170724       // --------------------------------------------------
000101170724       // Prototype for procedure: Yibs34r
000102170724       // --------------------------------------------------
000103170724
000104170724       DCL-PR Yibs34r EXTPGM('YIBS34R');
000105170724         prmUsrPrf CHAR(10) CONST OPTIONS(*NOPASS:*OMIT);
000106170724         prmEsito INT(10) OPTIONS(*NOPASS);
000109170724         prmDdatiUte CHAR(999) OPTIONS(*NOPASS:*VARSIZE);
000110170724         prmDdatiuteSize INT(10) CONST OPTIONS(*NOPASS);
000111170726         prmAzute00f CHAR(999) OPTIONS(*NOPASS:*VARSIZE);
000112170726         prmAzute00fSize INT(10) CONST OPTIONS(*NOPASS);
000113170724         prmTibs34ds CHAR(1685) OPTIONS(*NOPASS:*VARSIZE);
000114170724         prmTibs34dsSize INT(10) CONST OPTIONS(*NOPASS);
000115170724       END-PR ;
