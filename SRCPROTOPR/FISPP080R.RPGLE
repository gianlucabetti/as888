000100160610       // --------------------------------------------------
000101160610       // Prototype for procedure: FISPP080R
000102160610       // --------------------------------------------------
000103160610
000104160610       DCL-PR Fispp080r EXTPGM('FISPP080R');
000105160616         rqsOpc CHAR(10) CONST;
000106160616         rqsDta CHAR(9999) CONST;
000107160616         rqsLen PACKED(4) CONST;
000108160616         rqsCty CHAR(10) CONST;
000109160616         rqsCid CHAR(10) CONST;
000110160616         rpyOpc CHAR(10);
000111160616         rpyDta CHAR(9999);
000112160616         rpyLen PACKED(4);
000113160616       END-PR ;
