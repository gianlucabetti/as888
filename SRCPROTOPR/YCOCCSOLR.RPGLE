000001160219       // --------------------------------------------------
000002160219       // Prototype for procedure: Ycoccsolr
000003160219       // --------------------------------------------------
000004160219       DCL-PR Ycoccsolr EXTPGM('YCOCCSOLR');
000005160219         rqsOpCode INT(10) CONST;
000006160222         rpyOpCode INT(10) OPTIONS(*NOPASS);
000007160219         rqsFormato CHAR(10) CONST OPTIONS(*NOPASS);
000008160222         rqsData CHAR(502) CONST OPTIONS(*NOPASS:*VARSIZE);
000009160219         rqsDataSize INT(10) CONST OPTIONS(*NOPASS);
000010160219         rpyFormato CHAR(10) CONST OPTIONS(*NOPASS);
000011160222         rpyData CHAR(502) OPTIONS(*NOPASS:*VARSIZE);
000012160219         rpyDataSize INT(10) CONST OPTIONS(*NOPASS);
000013160219       END-PR ;
