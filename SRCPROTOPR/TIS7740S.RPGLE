000100160317       // --------------------------------------------------
000101160317       // Prototype for procedure: Tis7740s
000102160317       // --------------------------------------------------
000103160317
000104160317       DCL-PR Tis7740s EXTPGM('TIS7740S');
000105160317         rqsOpc CHAR(10) CONST;
000106160317         rqsDta CHAR(9999) CONST;
000107160317         rqsLen PACKED(4) CONST;
000108160317         rqsCty CHAR(10) CONST;
000109160317         rqsCid CHAR(10) CONST;
000110160317         rpyOpc CHAR(10);
000111160317         rpyDta CHAR(9999);
000112160317         rpyLen PACKED(4);
000113160317       END-PR ;
