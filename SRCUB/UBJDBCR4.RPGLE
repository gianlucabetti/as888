000100000000     /*-                                                                            +
000200000000      * Copyright (c) 2006-2008 Scott C. Klement                                    +
000300000000      * All rights reserved.                                                        +
000400000000      *                                                                             +
000500000000      * Redistribution and use in source and binary forms, with or without          +
000600000000      * modification, are permitted provided that the following conditions          +
000700000000      * are met:                                                                    +
000800000000      * 1. Redistributions of source code must retain the above copyright           +
000900000000      *    notice, this list of conditions and the following disclaimer.            +
001000000000      * 2. Redistributions in binary form must reproduce the above copyright        +
001100000000      *    notice, this list of conditions and the following disclaimer in the      +
001200000000      *    documentation and/or other materials provided with the distribution.     +
001300000000      *                                                                             +
001400000000      * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND      +
001500000000      * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE       +
001600000000      * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  +
001700000000      * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE     +
001800000000      * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL  +
001900000000      * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS     +
002000000000      * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)       +
002100000000      * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT  +
002200000000      * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY   +
002300000000      * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF      +
002400000000      * SUCH DAMAGE.                                                                +
002500000000      *                                                                             +
002600000000      */                                                                            +
002700000000      * Service program to simplify the user of JDBC drivers from ILE RPG.
002800000000      *                                    Scott Klement, April 18, 2006
002900000000      *
003000000000      *   Before Compiling:
003100000000      *
003200000000      *   - You need the JDBC drivers (use pure Java ones) for your
003300000000      *     database to be installed on the iSeries.
003400000000      *
003500000000      *   - This was originally written for, and most testing has been
003600000000      *     done with the MySQL drivers.  But, other drivers should
003700000000      *     work as well.  For a MySQL slanted article, see the following:
003800000000      *        http://www.iseriesnetwork.com/article.cfm?id=52433
003900000000      *
004000000000      *   - You need the System Openness Includes (5722-SS1 opt 13)
004100000000      *       licensed program installed.
004200000000      *
004300000000      *   - You need JDK 1.3 or later (JDK 1.4 recommended!)
004400000000      *
004500000000      *
004600000000      *   To Compile:
004700000000      *     CRTRPGMOD mylib/JDBCR4 SRCFILE(xxx/QRPGLESRC) DBGVIEW(*LIST)
004800000000      *        ( Note: this is the JDBCR4.rpgle source member. )
004900000000      *
005000000000      *     CRTSRVPGM SRVPGM(mylib/JDBCR4) +
005100000000      *               EXPORT(*SRCFILE) SRCFILE(xxx/QSRVSRC)
005200000000      *        ( Note: this is the JDBCR4.bnd source member. )
005300000000      *
005400000000      *     CRTBNDDIR BNDDIR(mylib/JDBC)
005500000000      *     ADDBNDDIRE BNDDIR(mylib/JDBC) OBJ((JDBCR4 *SRVPGM))
005600000000     H NOMAIN
005700000000
005800090402      /define OS400_JVM_12
005900000000      /copy qsysinc/qrpglesrc,jni
006000090716      /copy GAITRASRC/SRCPROTOPR,UBJDBC_H
006100000000
006200000000     D new_Driver      PR                  like(Driver)
006300000000     D   driver_name                256A   varying const
006400000000     D jni_checkError  PR             1N
006500000000     D    errString                 256A   varying options(*omit)
006600000000     D jdbc_get_jni_env...
006700000000     D                 PR              *
006800000000     D jdbc_begin_object_group...
006900000000     D                 PR            10I 0
007000000000     D    peCapacity                 10I 0 value
007100000000     D jdbc_end_object_group...
007200000000     D                 PR            10I 0
007300000000     D   peOldObj                          like(jObject) const
007400000000     D                                     options(*nopass)
007500000000     D   peNewObj                          like(jObject)
007600000000     D                                     options(*nopass)
007700000000     D attach_jvm      PR              *
007800000000     D start_jvm       PR              *
007900000000     D JniVersion      PR            10P 5
008000000000     D Timezone        PR            10I 0
008100000000
008200000000     D SndPgmMsg       PR                  ExtPgm('QMHSNDPM')
008300000000     D   MessageID                    7A   Const
008400000000     D   QualMsgF                    20A   Const
008500000000     D   MsgData                     80A   Const
008600000000     D   MsgDtaLen                   10I 0 Const
008700000000     D   MsgType                     10A   Const
008800000000     D   CallStkEnt                  10A   Const
008900000000     D   CallStkCnt                  10I 0 Const
009000000000     D   MessageKey                   4A
009100000000     D   ErrorCode                32767A   options(*varsize)
009200000000     D status          PR             1N
009300000000     D   peMsgTxt                   256A   const
009400000000     D MsgKey          s              4A
009500000000     D ErrCode         s              8A   inz(x'0000000000000000')
009600000000
009700000000     D Object          s               O   CLASS(*JAVA:
009800000000     D                                     'java.lang.Object')
009900000000     D Driver          s               O   CLASS(*JAVA:
010000000000     D                                     'java.sql.Driver')
010100000000     D BigDecimal      s               O   CLASS(*JAVA:
010200000000     D                                     'java.math.BigDecimal')
010300000000     D jSqlDate        s               O   CLASS(*JAVA:
010400000000     D                                     'java.sql.Date')
010500000000     D jSqlTime        s               O   CLASS(*JAVA:
010600000000     D                                     'java.sql.Time')
010700000000     D jSqlTimestamp   s               O   CLASS(*JAVA:
010800000000     D                                     'java.sql.Timestamp')
010900000000
011000000000     D createStatement...
011100000000     D                 PR                  ExtProc(*JAVA:
011200000000     D                                     'java.sql.Connection':
011300000000     D                                     'createStatement')
011400000000     D                                     like(Statement)
011500000000
011600000000     D PrepareStatement...
011700000000     D                 PR                  ExtProc(*JAVA:
011800000000     D                                     'java.sql.Connection':
011900000000     D                                     'prepareStatement')
012000000000     D                                     like(PreparedStatement)
012100000000     D    sql                              like(jString) const
012200000000
012300000000     D PrepareCall...
012400000000     D                 PR                  ExtProc(*JAVA:
012500000000     D                                     'java.sql.Connection':
012600000000     D                                     'prepareCall')
012700000000     D                                     like(CallableStatement)
012800000000     D    sql                              like(jString) const
012900000000
013000000000     D executeQuery...
013100000000     D                 PR                  ExtProc(*JAVA:
013200000000     D                                     'java.sql.Statement':
013300000000     D                                     'executeQuery')
013400000000     D                                     like(ResultSet)
013500000000     D   query                             like(jString) const
013600000000
013700000000     D executeUpdate...
013800000000     D                 PR                  ExtProc(*JAVA:
013900000000     D                                     'java.sql.Statement':
014000000000     D                                     'executeUpdate')
014100000000     D                                     like(jInt)
014200000000     D   query                             like(jString) const
014300000000
014400000000     D getMetaData...
014500000000     D                 PR                  ExtProc(*JAVA:
014600000000     D                                     'java.sql.ResultSet':
014700000000     D                                     'getMetaData')
014800000000     D                                     like(ResultSetMetaData)
014900000000
015000000000     D getColumnCount...
015100000000     D                 PR                  ExtProc(*JAVA:
015200000000     D                                     'java.sql.ResultSetMetaData':
015300000000     D                                     'getColumnCount')
015400000000     D                                     like(jInt)
015500000000
015600000000     D getColumnName...
015700000000     D                 PR                  ExtProc(*JAVA:
015800000000     D                                     'java.sql.ResultSetMetaData':
015900000000     D                                     'getColumnName')
016000000000     D                                     like(jString)
016100000000     D    colIndex                         like(jInt) value
016200000000
016300000000     D getColumnDisplaySize...
016400000000     D                 PR                  ExtProc(*JAVA:
016500000000     D                                     'java.sql.ResultSetMetaData':
016600000000     D                                     'getColumnDisplaySize')
016700000000     D                                     like(jInt)
016800000000     D    colIndex                         like(jInt) value
016900000000
017000000000     D getColumnTypeName...
017100000000     D                 PR                  ExtProc(*JAVA:
017200000000     D                                     'java.sql.ResultSetMetaData':
017300000000     D                                     'getColumnTypeName')
017400000000     D                                     like(jString)
017500000000     D    colIndex                         like(jInt) value
017600000000
017700000000     D nextRec...
017800000000     D                 PR             1N   ExtProc(*JAVA:
017900000000     D                                     'java.sql.ResultSet':
018000000000     D                                     'next')
018100000000
018200000000     D prevRec...
018300000000     D                 PR             1N   ExtProc(*JAVA:
018400000000     D                                     'java.sql.ResultSet':
018500000000     D                                     'last')
018600000000
018700000000     D getColString...
018800000000     D                 PR                  ExtProc(*JAVA:
018900000000     D                                     'java.sql.ResultSet':
019000000000     D                                     'getString')
019100000000     D                                     like(jString)
019200000000     D    colIndex                         like(jInt) value
019300000000
019400000000     D getColStrByName...
019500000000     D                 PR                  ExtProc(*JAVA
019600000000     D                                     :'java.sql.ResultSet'
019700000000     D                                     :'getString')
019800000000     D                                     like(jString)
019900000000     D    colName                          like(jString)
020000000000
020100000000     D closeConn       PR                  ExtProc(*JAVA:
020200000000     D                                     'java.sql.Connection':
020300000000     D                                     'close')
020400000000
020500000000     D commitTrn       PR                  ExtProc(*JAVA:
020600000000     D                                     'java.sql.Connection':
020700000000     D                                     'commit')
020800000000
020900000000     D rollbackTrn     PR                  ExtProc(*JAVA:
021000000000     D                                     'java.sql.Connection':
021100000000     D                                     'rollback')
021200000000
021300000000     D s               PR                  like(jString)
021400000000     D                                     EXTPROC(*JAVA
021500000000     D                                     :'java.lang.String'
021600000000     D                                     :*CONSTRUCTOR)
021700000000     D create_from                32767A   VARYING const
021800000000
021900000000     D r               pr         32767A   varying
022000000000     D                                     extproc(*JAVA:
022100000000     D                                     'java.lang.String':
022200000000     D                                     'getBytes')
022300000000
022400000000     D new_Properties  PR                  ExtProc(*JAVA
022500000000     D                                     :'java.util.Properties'
022600000000     D                                     :*CONSTRUCTOR)
022700000000     D                                     like(Properties)
022800000000
022900000000     D setProperty     PR                  ExtProc(*JAVA
023000000000     D                                     :'java.util.Properties'
023100000000     D                                     :'setProperty')
023200000000     D                                     like(Object)
023300000000     D   key                               like(jString) const
023400000000     D   value                             like(jString) const
023500000000
023600000000     D registerDriver  PR                  ExtProc(*JAVA
023700000000     D                                     :'java.sql.DriverManager'
023800000000     D                                     :'registerDriver')
023900000000     D                                     static
024000000000     D   drv                               like(Driver)
024100000000
024200000000     D Driver_ConnProp...
024300000000     D                 PR                  like(Connection)
024400000000     D   drv                               like(Driver) value
024500000000     D   url                        256A   varying const options(*varsize)
024600000000     D   prop                              like(Properties) value
024700000000     D Driver_connect  PR                  like(Connection)
024800000000     D   drv                               like(Driver) value
024900000000     D   url                        256A   varying const options(*varsize)
025000000000     D   user                        50a   varying const options(*varsize)
025100000000     D   pass                        50a   varying const options(*varsize)
025200000000
025300000000     D new_BigDecimal  PR                  ExtProc(*JAVA
025400000000     D                                     :'java.math.BigDecimal'
025500000000     D                                     :*CONSTRUCTOR)
025600000000     D                                     like(BigDecimal)
025700000000     D   str                               like(jString) const
025800000000
025900000000     D new_Date        PR                  ExtProc(*JAVA
026000000000     D                                     :'java.sql.Date'
026100000000     D                                     :*CONSTRUCTOR)
026200000000     D                                     like(jSqlDate)
026300000000     D   milli                             like(jLong) value
026400000000
026500000000     D new_Time        PR                  ExtProc(*JAVA
026600000000     D                                     :'java.sql.Time'
026700000000     D                                     :*CONSTRUCTOR)
026800000000     D                                     like(jSqlTime)
026900000000     D   milli                             like(jLong) value
027000000000
027100000000     D new_Timestamp   PR                  ExtProc(*JAVA
027200000000     D                                     :'java.sql.Timestamp'
027300000000     D                                     :*CONSTRUCTOR)
027400000000     D                                     like(jSqlTimestamp)
027500000000     D   milli                             like(jLong) value
027600000000
027700000000     D setBigDecimal...
027800000000     D                 PR                  ExtProc(*JAVA
027900000000     D                                     :'java.sql.PreparedStatement'
028000000000     D                                     :'setBigDecimal')
028100000000     D   idx                               like(jInt) value
028200000000     D   dec                               like(BigDecimal) const
028300000000
028400000000     D setDate...
028500000000     D                 PR                  ExtProc(*JAVA
028600000000     D                                     :'java.sql.PreparedStatement'
028700000000     D                                     :'setDate')
028800000000     D   idx                               like(jInt) value
028900000000     D   date                              like(jSqlDate) const
029000000000
029100000000     D setTime...
029200000000     D                 PR                  ExtProc(*JAVA
029300000000     D                                     :'java.sql.PreparedStatement'
029400000000     D                                     :'setTime')
029500000000     D   idx                               like(jInt) value
029600000000     D   time                              like(jSqlTime) const
029700000000
029800000000     D setTimestamp...
029900000000     D                 PR                  ExtProc(*JAVA
030000000000     D                                     :'java.sql.PreparedStatement'
030100000000     D                                     :'setTimestamp')
030200000000     D   idx                               like(jInt) value
030300000000     D   ts                                like(jSqlTimestamp) const
030400000000
030500000000     D setDouble...
030600000000     D                 PR                  ExtProc(*JAVA
030700000000     D                                     :'java.sql.PreparedStatement'
030800000000     D                                     :'setDouble')
030900000000     D   idx                               like(jInt) value
031000000000     D   dubba                             like(jDouble) value
031100000000
031200000000     D setInt...
031300000000     D                 PR                  ExtProc(*JAVA
031400000000     D                                     :'java.sql.PreparedStatement'
031500000000     D                                     :'setInt')
031600000000     D   idx                               like(jInt) value
031700000000     D   int                               like(jInt) value
031800000000
031900000000     D setString...
032000000000     D                 PR                  ExtProc(*JAVA
032100000000     D                                     :'java.sql.PreparedStatement'
032200000000     D                                     :'setString')
032300000000     D   idx                               like(jInt) value
032400000000     D   str                               like(jString) const
032500000000
032600000000     D setByte...
032700000000     D                 PR                  ExtProc(*JAVA
032800000000     D                                     :'java.sql.PreparedStatement'
032900000000     D                                     :'setByte')
033000000000     D   idx                               like(jInt) value
033100000000     D   byte                              like(jByte) value
033200000000
033300000000     D registerOutParameter...
033400000000     D                 PR                  ExtProc(*JAVA
033500000000     D                                     :'java.sql.CallableStatement'
033600000000     D                                     :'registerOutParameter')
033700000000     D   idx                               like(jInt) value
033800000000     D   type                              like(jInt) value
033900000000
034000000000     D getString...
034100000000     D                 PR                  ExtProc(*JAVA:
034200000000     D                                     'java.sql.CallableStatement':
034300000000     D                                     'getString')
034400000000     D                                     like(jString)
034500000000     D    idx                              like(jInt) value
034600000000
034700000000     D getInt...
034800000000     D                 PR                  ExtProc(*JAVA:
034900000000     D                                     'java.sql.CallableStatement':
035000000000     D                                     'getInt')
035100000000     D                                     like(jInt)
035200000000     D    idx                              like(jInt) value
035300000000
035400000000     D getShort...
035500000000     D                 PR                  ExtProc(*JAVA:
035600000000     D                                     'java.sql.CallableStatement':
035700000000     D                                     'getShort')
035800000000     D                                     like(jShort)
035900000000     D    idx                              like(jInt) value
036000000000
036100000000     D getDouble...
036200000000     D                 PR                  ExtProc(*JAVA:
036300000000     D                                     'java.sql.CallableStatement':
036400000000     D                                     'getDouble')
036500000000     D                                     like(jDouble)
036600000000     D   idx                               like(jInt) value
036700000000
036800000000     D getBigDecimal...
036900000000     D                 PR                  ExtProc(*JAVA:
037000000000     D                                     'java.sql.CallableStatement':
037100000000     D                                     'getBigDecimal')
037200000000     D                                     like(BigDecimal)
037300000000     D   idx                               like(jInt) value
037400000000
037500000000     D getLong...
037600000000     D                 PR                  ExtProc(*JAVA:
037700000000     D                                     'java.sql.CallableStatement':
037800000000     D                                     'getLong')
037900000000     D                                     like(jLong)
038000000000     D   idx                               like(jInt) value
038100000000
038200000000     D getTimestamp...
038300000000     D                 PR                  ExtProc(*JAVA:
038400000000     D                                     'java.sql.CallableStatement':
038500000000     D                                     'getTimestamp')
038600000000     D                                     like(jSqlTimestamp)
038700000000     D   idx                               like(jInt) value
038800000000
038900000000     D getTime...
039000000000     D                 PR                  ExtProc(*JAVA:
039100000000     D                                     'java.sql.CallableStatement':
039200000000     D                                     'getTime')
039300000000     D                                     like(jSqlTime)
039400000000     D   idx                               like(jInt) value
039500000000
039600000000     D getDate...
039700000000     D                 PR                  ExtProc(*JAVA:
039800000000     D                                     'java.sql.CallableStatement':
039900000000     D                                     'getDate')
040000000000     D                                     like(jSqlDate)
040100000000     D   idx                               like(jInt) value
040200000000
040300000000     D getBoolean...
040400000000     D                 PR             1N   ExtProc(*JAVA:
040500000000     D                                     'java.sql.CallableStatement':
040600000000     D                                     'getBoolean')
040700000000     D   idx                               like(jInt) value
040800000000
040900000000     D prepExecuteQuery...
041000000000     D                 PR                  ExtPRoc(*JAVA
041100000000     D                                     :'java.sql.PreparedStatement'
041200000000     D                                     :'executeQuery')
041300000000     D                                     like(ResultSet)
041400000000
041500000000     D prepExecuteUpdate...
041600000000     D                 PR                  ExtPRoc(*JAVA
041700000000     D                                     :'java.sql.PreparedStatement'
041800000000     D                                     :'executeUpdate')
041900000000     D                                     like(jInt)
042000000000
042100000000     D callExecute...
042200000000     D                 PR             1N   ExtProc(*JAVA
042300000000     D                                     :'java.sql.CallableStatement'
042400000000     D                                     :'execute')
042500000000
042600000000     D prepExecute...
042700000000     D                 PR             1N   ExtProc(*JAVA
042800000000     D                                     :'java.sql.PreparedStatement'
042900000000     D                                     :'execute')
043000000000
043100000000     D Exception_getMessage...
043200000000     D                 PR                  ExtProc(*JAVA
043300000000     D                                     :'java.lang.Throwable'
043400000000     D                                     :'getMessage')
043500000000     D                                     like(jstring)
043600000000
043700000000     D rs_getStatement...
043800000000     D                 PR                  ExtProc(*JAVA
043900000000     D                                     :'java.sql.ResultSet'
044000000000     D                                     :'getStatement')
044100000000     D                                     like(Statement)
044200000000
044300000000     D stmt_Close      PR                  ExtProc(*JAVA
044400000000     D                                     :'java.sql.Statement'
044500000000     D                                     :'close')
044600000000
044700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
044800000000      * MySQL_Connect(): Create a connection to a MYSQL server
044900000000      *
045000000000      *    server = (input) mysql server to connect to
045100000000      *  database = (input) database to use on server
045200000000      *    userid = (input) userid to log in with
045300000000      *  password = (input) password to log in with
045400000000      *
045500000000      * Returns a connection handle or *NULL upon failure
045600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
045700000000     P MySQL_Connect   B                   export
045800000000     D MySQL_Connect   PI                  like(Connection)
045900000000     D   server                     256A   varying const options(*varsize)
046000000000     D   database                   256A   varying const options(*varsize)
046100000000     D   userid                      50A   varying const options(*varsize)
046200000000     D   password                    50A   varying const options(*varsize)
046300000000
046400000000     D url             s            256A   varying
046500000000     D conn            s                   like(Connection)
046600000000
046700000000      /free
046800000000         url = 'jdbc:mysql://' + %trim(server) + '/'
046900000000             + %trim(database);
047000000000
047100000000         conn = JDBC_connect('com.mysql.jdbc.Driver'
047200000000                            : url
047300000000                            : userid
047400000000                            : password );
047500000000
047600000000         return conn;
047700000000      /end-free
047800000000     P                 E
047900000000
048000000000
048100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
048200000000      * JDBC_Connect(): Create a connection using JDBC driver
048300000000      *
048400000000      *    driver = (input) Java class name of JDBC driver to use
048500000000      *       url = (input) JDBC URL of database to connect to
048600000000      *    userid = (input) userid to log in with
048700000000      *  password = (input) password to log in with
048800000000      *
048900000000      * Returns a connection handle or *NULL upon failure
049000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
049100000000     P JDBC_Connect    B                   export
049200000000     D JDBC_Connect    PI                  like(Connection)
049300000000     D   drivname                   256A   varying const options(*varsize)
049400000000     D   url                        256A   varying const options(*varsize)
049500000000     D   userid                      50A   varying const options(*varsize)
049600000000     D   password                    50A   varying const options(*varsize)
049700000000
049800000000     D drv             s                   like(Driver)
049900000000     D temp            s                   like(Connection)
050000000000     D conn            s                   like(Connection)
050100000000
050200000000      /free
050300000000          jdbc_begin_object_group(50);
050400000000
050500000000          monitor;
050600000000          // Find & Instantiate Driver
050700000000
050800000000             drv = new_Driver(drivname);
050900000000             if (drv = *NULL);
051000000000                jdbc_end_object_group();
051100000000                return *NULL;
051200000000             endif;
051300000000
051400000000          // Register with DriverManager
051500000000          //   and connect.
051600000000
051700000000             registerDriver(drv);
051800000000             temp = Driver_Connect( drv : url: userid: password);
051900000000             if (temp = *NULL);
052000000000                jdbc_end_object_group();
052100000000                return *NULL;
052200000000             endif;
052300000000
052400000000             jdbc_end_object_group(temp: conn);
052500000000             return conn;
052600000000          on-error;
052700000000             jdbc_end_object_group();
052800000000             return *NULL;
052900000000          endmon;
053000000000
053100000000      /end-free
053200000000     P                 E
053300000000
053400000000
053500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
053600000000      * JDBC_ConnProp(): Connect to database w/properties object
053700000000      *
053800000000      *    driver = (input) Java class name of JDBC driver to use
053900000000      *       url = (input) JDBC URL to connect to
054000000000      *      prop = (input) properties to use when connecting
054100000000      *
054200000000      * Returns a connection handle or *NULL upon failure
054300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
054400000000     P JDBC_ConnProp   B                   export
054500000000     D JDBC_ConnProp   PI                  like(Connection)
054600000000     D   drivname                   256A   varying const options(*varsize)
054700000000     D   url                        256A   varying const options(*varsize)
054800000000     D   prop                              like(Properties)
054900000000     D drv             s                   like(Driver)
055000000000     D temp            s                   like(Connection)
055100000000     D conn            s                   like(Connection)
055200000000      /free
055300000000          jdbc_begin_object_group(50);
055400000000
055500000000          monitor;
055600000000          // Find & Instantiate Driver
055700000000
055800000000             drv = new_Driver(drivname);
055900000000             if (drv = *NULL);
056000000000                jdbc_end_object_group();
056100000000                return *NULL;
056200000000             endif;
056300000000
056400000000          // Register with DriverManager
056500000000          //   and connect.
056600000000
056700000000             registerDriver(drv);
056800000000             temp = Driver_ConnProp( drv : url: prop );
056900000000             if (temp = *NULL);
057000000000                jdbc_end_object_group();
057100000000                return *NULL;
057200000000             endif;
057300000000
057400000000             jdbc_end_object_group(temp: conn);
057500000000             return conn;
057600000000          on-error;
057700000000             jdbc_end_object_group();
057800000000             return *NULL;
057900000000          endmon;
058000000000
058100000000      /end-free
058200000000     P                 E
058300000000
058400000000
058500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
058600000000      *  JDBC_Properties():  Create new properties object
058700000000      *
058800000000      *  returns the object.
058900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
059000000000     P JDBC_Properties...
059100000000     P                 B                   export
059200000000     D JDBC_Properties...
059300000000     D                 PI                  like(Properties)
059400000000     D prop            s                   like(Properties)
059500000000      /free
059600000000         jdbc_get_jni_env();
059700000000         prop = new_Properties();
059800000000         return prop;
059900000000      /end-free
060000000000     P                 E
060100000000
060200000000
060300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
060400000000      *  JDBC_setProp():  Set one property in Properties object
060500000000      *
060600000000      *     prop = (i/o) Properties object to set property in
060700000000      *      key = (input) property to set
060800000000      *    value = (input) value to assign to property
060900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
061000000000     P JDBC_setProp    B                   export
061100000000     D JDBC_setProp    PI
061200000000     D   prop                              like(Properties)
061300000000     D   key                        256A   varying const options(*varsize)
061400000000     D   value                      256A   varying const options(*varsize)
061500000000     D keystr          s                   like(jString)
061600000000     D valstr          s                   like(jString)
061700000000      /free
061800000000         keystr = s(key);
061900000000         valstr = s(value);
062000000000         setProperty(prop: keystr: valstr);
062100000000         DeleteLocalRef(JNIENV_P: keystr);
062200000000         DeleteLocalRef(JNIENV_P: valstr);
062300000000      /end-free
062400000000     P                 E
062500000000
062600000000
062700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
062800000000      *  JDBC_freeProp():  Release properties object
062900000000      *
063000000000      *     prop = (i/o) Properties object to release
063100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
063200000000     P JDBC_freeProp   B                   export
063300000000     D JDBC_freeProp   PI
063400000000     D   prop                              like(Properties)
063500000000      /free
063600000000         DeleteLocalRef(JNIENV_P: prop);
063700000000         prop = *NULL;
063800000000      /end-free
063900000000     P                 E
064000000000
064100000000
064200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
064300000000      * This calls the JNI functions to retrieve a JDBC driver
064400000000      * object based on the class name.
064500000000      *
064600000000      *    driver_name = (input) Java class name of JDBC driver
064700000000      *
064800000000      * returns new java.sql.Driver object
064900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
065000000000     P new_Driver      B
065100000000     D new_Driver      PI                  like(Driver)
065200000000     D   driver_name                256A   varying const
065300000000
065400000000     D QDCXLATE        PR                  ExtPgm('QDCXLATE')
065500000000     D   len                          5P 0 const
065600000000     D   data                     32702A   options(*varsize)
065700000000     D   table                       10A   const
065800000000
065900000000     D                 ds
066000000000     D   drv_name                   256A   varying
066100000000     D   drv_fixed                  256A   overlay(drv_name:3)
066200000000
066300000000     D CONSTRUCTOR     C                   x'3c696e69743e'
066400000000     D NPARMVOID       C                   x'282956'
066500000000
066600000000     D env             s               *   static inz(*NULL)
066700000000     D msg             s            256A   varying
066800000000     D cls             s                   like(jclass)
066900000000     D mid             s                   like(jmethodid)
067000000000     D obj             s                   like(Driver)
067100000000      /free
067200000000          if (env = *NULL);
067300000000              env = jdbc_get_jni_env();
067400000000          endif;
067500000000
067600000000          drv_name = %xlate('.': '/': driver_name);
067700000000          QDCXLATE(%len(drv_name): drv_fixed: 'QTCPASC');
067800000000
067900000000          cls = FindClass(env: drv_name);
068000000000          if (jni_CheckError(*omit));
068100000000             return *NULL;
068200000000          endif;
068300000000
068400000000          if ( cls = *NULL );
068500000000             msg = 'Class ' + driver_name + ' not found!';
068600000000             exsr DiagMsg;
068700000000             return *NULL;
068800000000          endif;
068900000000
069000000000          mid = GetMethodID(env: cls: CONSTRUCTOR: NPARMVOID);
069100000000          if (jni_CheckError(*omit));
069200000000             return *NULL;
069300000000          endif;
069400000000
069500000000          if ( mid = *NULL );
069600000000             msg = 'Constructor method for ' + driver_name
069700000000                 + ' not found!';
069800000000             exsr DiagMsg;
069900000000             return *NULL;
070000000000          endif;
070100000000
070200000000          obj = NewObject(env: cls: mid);
070300000000          if (jni_CheckError(*omit));
070400000000             return *NULL;
070500000000          endif;
070600000000
070700000000          if ( obj = *NULL );
070800000000               msg = 'Null returned when attempting to create '
070900000000                   + driver_name + ' object';
071000000000               exsr DiagMsg;
071100000000               return *NULL;
071200000000          endif;
071300000000
071400000000          return obj;
071500000000
071600000000          begsr DiagMsg;
071700000000             SndPgmMsg( 'CPF9897'
071800000000                      : 'QCPFMSG   *LIBL'
071900000000                      : msg
072000000000                      : %len(msg)
072100000000                      : '*DIAG'
072200000000                      : '*'
072300000000                      : 0
072400000000                      : MsgKey
072500000000                      : ErrCode );
072600000000          endsr;
072700000000      /end-free
072800000000     P                 E
072900000000
073000000000
073100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
073200000000      * Driver_ConnProp(): This calls the JNI routines (directly)
073300000000      *   to connect a driver to a data source.
073400000000      *
073500000000      * NOTE: I used the JNI routines instead of RPG's SQL support
073600000000      *       because I have more control over the error checking.
073700000000      *
073800000000      *      drv = (input) driver object to connect with
073900000000      *      url = (input) JDBC url to server
074000000000      *     prop = (input) connection properties
074100000000      *
074200000000      * Returns a Connection object, or *NULL upon failure
074300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
074400000000     P Driver_ConnProp...
074500000000     P                 B
074600000000     D Driver_ConnProp...
074700000000     D                 PI                  like(Connection)
074800000000     D   drv                               like(Driver) value
074900000000     D   url                        256A   varying const options(*varsize)
075000000000     D   prop                              like(Properties) value
075100000000
075200000000     D QDCXLATE        PR                  ExtPgm('QDCXLATE')
075300000000     D   len                          5P 0 const
075400000000     D   data                     32702A   options(*varsize)
075500000000     D   table                       10A   const
075600000000
075700000000     D clsDriver       s             15A   inz('java/sql/Driver')
075800000000     D methConn        s              7A   inz('connect')
075900000000     D sigConn         s             63A   inz('(Ljava/lang/String;-
076000000000     D                                     Ljava/util/Properties;)-
076100000000     D                                     Ljava/sql/Connection;')
076200000000     D CallConnectMethod...
076300000000     D                 PR                  LIKE(jobject)
076400000000     D                                     EXTPROC(*CWIDEN
076500000000     D                                     : JNINativeInterface.
076600000000     D                                       CallObjectMethod_P)
076700000000     D env                                 LIKE(JNIEnv_P) VALUE
076800000000     D obj                                 LIKE(jobject) VALUE
076900000000     D methodID                            LIKE(jmethodID) VALUE
077000000000     D drv                                 LIKE(Driver) VALUE
077100000000     D                                     options(*nopass)
077200000000     D url                                 LIKE(jString) VALUE
077300000000     D                                     options(*nopass)
077400000000     D prop                                LIKE(Properties) VALUE
077500000000     D                                     options(*nopass)
077600000000
077700000000     D msg             s            256A   varying
077800000000     D cls             s                   like(jclass)
077900000000     D mid             s                   like(jmethodid)
078000000000     D conn            s                   like(Connection)
078100000000     D str             s                   like(jString)
078200000000
078300000000      /free
078400000000
078500000000          QDCXLATE( %size(clsDriver)
078600000000                  : clsDriver
078700000000                  : 'QTCPASC');
078800000000          QDCXLATE( %size(methConn)
078900000000                  : methConn
079000000000                  : 'QTCPASC');
079100000000          QDCXLATE( %size(sigConn)
079200000000                  : sigConn
079300000000                  : 'QTCPASC');
079400000000
079500000000          cls = FindClass(JNIENV_P: clsDriver);
079600000000          if (jni_CheckError(*omit));
079700000000             return *NULL;
079800000000          endif;
079900000000
080000000000          if ( cls = *NULL );
080100000000               msg = 'Class java.sql.Driver not found!';
080200000000               exsr DiagMsg;
080300000000               return *NULL;
080400000000          endif;
080500000000
080600000000          mid = GetMethodID(JNIENV_P: cls: methConn: sigConn);
080700000000          if (jni_CheckError(*omit));
080800000000             return *NULL;
080900000000          endif;
081000000000
081100000000          if ( mid = *NULL );
081200000000               msg = 'Connect method not found in java.sql.Driver'
081300000000                   + ' class!';
081400000000               exsr DiagMsg;
081500000000               return *NULL;
081600000000          endif;
081700000000
081800000000          status('Connecting to database...');
081900000000
082000000000          str = s(url);
082100000000          conn = CallConnectMethod(JNIENV_P: drv: mid: str: prop);
082200000000          status('.');
082300000000          if (jni_CheckError(*omit));
082400000000             return *NULL;
082500000000          endif;
082600000000          DeleteLocalRef(JNIENV_P: str);
082700000000
082800000000          if ( conn = *NULL );
082900000000               msg = 'Unable to connect.';
083000000000               exsr DiagMsg;
083100000000               return *NULL;
083200000000          endif;
083300000000
083400000000          return conn;
083500000000
083600000000          begsr DiagMsg;
083700000000             SndPgmMsg( 'CPF9897'
083800000000                      : 'QCPFMSG   *LIBL'
083900000000                      : msg
084000000000                      : %len(msg)
084100000000                      : '*DIAG'
084200000000                      : '*'
084300000000                      : 0
084400000000                      : MsgKey
084500000000                      : ErrCode );
084600000000          endsr;
084700000000      /end-free
084800000000     P                 E
084900000000
085000000000
085100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
085200000000      * Driver_connect(): This calls the JNI routines (directly)
085300000000      *   to connect a driver to a data source.
085400000000      *
085500000000      * NOTE: I used the JNI routines instead of RPG's SQL support
085600000000      *       because I have more control over the error checking.
085700000000      *
085800000000      *      drv = (input) driver object to connect with
085900000000      *      url = (input) JDBC url to server
086000000000      *     user = (input) userid to connect with
086100000000      *     pass = (input) password to connect with
086200000000      *
086300000000      * Returns a Connection object, or *NULL upon failure
086400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
086500000000     P Driver_connect  B
086600000000     D Driver_connect  PI                  like(Connection)
086700000000     D   drv                               like(Driver) value
086800000000     D   url                        256A   varying const options(*varsize)
086900000000     D   user                        50a   varying const options(*varsize)
087000000000     D   pass                        50a   varying const options(*varsize)
087100000000
087200000000     D QDCXLATE        PR                  ExtPgm('QDCXLATE')
087300000000     D   len                          5P 0 const
087400000000     D   data                     32702A   options(*varsize)
087500000000     D   table                       10A   const
087600000000
087700000000     D clsDriver       s             15A   inz('java/sql/Driver')
087800000000     D methConn        s              7A   inz('connect')
087900000000     D sigConn         s             77A   inz('(Ljava/lang/String;-
088000000000     D                                     Ljava/lang/String;-
088100000000     D                                     Ljava/lang/String;)-
088200000000     D                                     Ljava/sql/Connection;')
088300000000     D CallConnectMethod...
088400000000     D                 PR                  LIKE(jobject)
088500000000     D                                     EXTPROC(*CWIDEN
088600000000     D                                     : JNINativeInterface.
088700000000     D                                       CallObjectMethod_P)
088800000000     D env                                 LIKE(JNIEnv_P) VALUE
088900000000     D obj                                 LIKE(jobject) VALUE
089000000000     D methodID                            LIKE(jmethodID) VALUE
089100000000     D drv                                 LIKE(Driver) VALUE
089200000000     D                                     options(*nopass)
089300000000     D url                                 LIKE(jString) VALUE
089400000000     D                                     options(*nopass)
089500000000     D userid                              LIKE(jString) VALUE
089600000000     D                                     options(*nopass)
089700000000     D passwd                              LIKE(jString) VALUE
089800000000     D                                     options(*nopass)
089900000000
090000000000     D msg             s            256A   varying
090100000000     D cls             s                   like(jclass)
090200000000     D mid             s                   like(jmethodid)
090300000000     D conn            s                   like(Connection)
090400000000     D urlstr          s                   like(jString)
090500000000     D userstr         s                   like(jString)
090600000000     D passstr         s                   like(jString)
090700000000
090800000000      /free
090900000000
091000000000          QDCXLATE( %size(clsDriver)
091100000000                  : clsDriver
091200000000                  : 'QTCPASC');
091300000000          QDCXLATE( %size(methConn)
091400000000                  : methConn
091500000000                  : 'QTCPASC');
091600000000          QDCXLATE( %size(sigConn)
091700000000                  : sigConn
091800000000                  : 'QTCPASC');
091900000000
092000000000          cls = FindClass(JNIENV_P: clsDriver);
092100000000          if (jni_CheckError(*omit));
092200000000             return *NULL;
092300000000          endif;
092400000000
092500000000          if ( cls = *NULL );
092600000000               msg = 'Class java.sql.Driver not found!';
092700000000               exsr DiagMsg;
092800000000               return *NULL;
092900000000          endif;
093000000000
093100000000          mid = GetMethodID(JNIENV_P: cls: methConn: sigConn);
093200000000          if (jni_CheckError(*omit));
093300000000             return *NULL;
093400000000          endif;
093500000000
093600000000          if ( mid = *NULL );
093700000000               msg = 'Connect method not found in java.sql.Driver'
093800000000                   + ' class!';
093900000000               exsr DiagMsg;
094000000000               return *NULL;
094100000000          endif;
094200000000
094300000000          status('Connecting to database...');
094400000000
094500000000          urlstr  = s(url);
094600000000          userstr = s(url);
094700000000          passstr = s(url);
094800000000
094900000000          conn = CallConnectMethod( JNIENV_P
095000000000                                  : drv
095100000000                                  : mid
095200000000                                  : urlstr
095300000000                                  : userstr
095400000000                                  : passstr );
095500000000          status('.');
095600000000          if (jni_CheckError(*omit));
095700000000             return *NULL;
095800000000          endif;
095900000000          DeleteLocalRef(JNIENV_P: urlstr);
096000000000          DeleteLocalRef(JNIENV_P: userstr);
096100000000          DeleteLocalRef(JNIENV_P: passstr);
096200000000
096300000000          if ( conn = *NULL );
096400000000               msg = 'Unable to connect.';
096500000000               exsr DiagMsg;
096600000000               return *NULL;
096700000000          endif;
096800000000
096900000000          return conn;
097000000000
097100000000          begsr DiagMsg;
097200000000             SndPgmMsg( 'CPF9897'
097300000000                      : 'QCPFMSG   *LIBL'
097400000000                      : msg
097500000000                      : %len(msg)
097600000000                      : '*DIAG'
097700000000                      : '*'
097800000000                      : 0
097900000000                      : MsgKey
098000000000                      : ErrCode );
098100000000          endsr;
098200000000      /end-free
098300000000     P                 E
098400000000
098500000000
098600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
098700000000      *  JDBC_ExecUpd():  Execute a Query that doesn't return results
098800000000      *
098900000000      *     conn = (input) database connection
099000000000      *      sql = (input) SQL Code to execute
099100000000      *
099200000000      *  returns a row count, or 0 where a row count is not applicable
099300000000      *        or -1 upon failure
099400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
099500000000     P JDBC_ExecUpd    B                   export
099600000000     D JDBC_ExecUpd    PI            10I 0
099700000000     D   conn                              like(Connection) const
099800000000     D   sql                      32767A   varying const options(*varsize)
099900000000     D rc              s             10I 0
100000000000     D stmt            s                   like(Statement)
100100000000      /free
100200000000          jdbc_begin_object_group(50);
100300000000
100400000000          monitor;
100500000000             stmt = createStatement( conn );
100600000000             rc = executeUpdate( stmt : s(sql));
100700000000             stmt_close(stmt);
100800000000             jdbc_end_object_group();
100900000000          on-error;
101000000000             jdbc_end_object_group();
101100000000             return -1;
101200000000          endmon;
101300000000          return rc;
101400000000      /end-free
101500000000     P                 E
101600000000
101700000000
101800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
101900000000      *  JDBC_ExecQry():  Execute a Query that returns a result
102000000000      *
102100000000      *     conn = (input) database connection
102200000000      *      sql = (input) SQL code to execute
102300000000      *
102400000000      *  Returns a result handle, or *NULL upon failure
102500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
102600000000     P JDBC_ExecQry    B                   export
102700000000     D JDBC_ExecQry    PI                  like(ResultSet)
102800000000     D   conn                              like(Connection) const
102900000000     D   sql                      32767A   varying const options(*varsize)
103000000000     D stmt            s                   like(Statement)
103100000000     D temp            s                   like(ResultSet)
103200000000     D rs              s                   like(ResultSet)
103300000000      /free
103400000000          jdbc_begin_object_group(50);
103500000000
103600000000          monitor;
103700000000             stmt = createStatement( conn );
103800000000             temp = executeQuery( stmt : s(sql));
103900000000             jdbc_end_object_group(temp: rs);
104000000000          on-error;
104100000000             jdbc_end_object_group();
104200000000             return *NULL;
104300000000          endmon;
104400000000
104500000000          return rs;
104600000000      /end-free
104700000000     P                 E
104800000000
104900000000
105000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
105100000000      * JDBC_PrepStmt(): Create a Prepared Statement
105200000000      *
105300000000      *    conn = (input) Connection to prepare statement for
105400000000      *     sql = (input) SQL statement to prepare
105500000000      *
105600000000      * Returns a prepared statement object, or *NULL upon error
105700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
105800000000     P JDBC_PrepStmt   B                   export
105900000000     D JDBC_PrepStmt   PI                  like(PreparedStatement)
106000000000     D   conn                              like(Connection) const
106100000000     D   sql                      32767A   varying const options(*varsize)
106200000000     D temp            s                   like(PreparedStatement)
106300000000     D stmt            s                   like(PreparedStatement)
106400000000      /free
106500000000          jdbc_begin_object_group(50);
106600000000
106700000000          monitor;
106800000000             temp = prepareStatement( conn : s(sql) );
106900000000             jdbc_end_object_group(temp: stmt);
107000000000          on-error;
107100000000             jdbc_end_object_group();
107200000000             return *NULL;
107300000000          endmon;
107400000000
107500000000          return stmt;
107600000000      /end-free
107700000000     P                 E
107800000000
107900000000
108000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
108100000000      * JDBC_PrepCall(): Create a Prepared Call
108200000000      *
108300000000      *    conn = (input) Connection to prepare call for
108400000000      *     sql = (input) SQL call to prepare
108500000000      *
108600000000      * Returns a prepared call object, or *NULL upon error
108700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
108800000000     P JDBC_PrepCall   B                   export
108900000000     D JDBC_PrepCall   PI                  like(CallableStatement)
109000000000     D   conn                              like(Connection) const
109100000000     D   sql                      32767A   varying const options(*varsize)
109200000000     D temp            s                   like(CallableStatement)
109300000000     D stmt            s                   like(CallableStatement)
109400000000      /free
109500000000          jdbc_begin_object_group(50);
109600000000
109700000000          monitor;
109800000000             temp = prepareCall( conn : s(sql) );
109900000000             jdbc_end_object_group(temp: stmt);
110000000000          on-error;
110100000000             jdbc_end_object_group();
110200000000             return *NULL;
110300000000          endmon;
110400000000          return stmt;
110500000000      /end-free
110600000000     P                 E
110700000000
110800000000
110900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
111000000000      *  JDBC_GetMetaData():  Get ResultSet MetaData
111100000000      *
111200000000      *       rs = (input) ResultSet handle
111300000000      *
111400000000      *  Returns a ResultSet MetaData object, or *NULL upon failure
111500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
111600000000     P JDBC_GetMetaData...
111700000000     P                 B                   export
111800000000     D JDBC_GetMetaData...
111900000000     D                 PI                  like(ResultSetMetaData)
112000000000     D   rs                                like(ResultSet)
112100000000     D temp            s                   like(ResultSetMetaData)
112200000000     D rsmd            s                   like(ResultSetMetaData)
112300000000      /free
112400000000          jdbc_begin_object_group(50);
112500000000          monitor;
112600000000             temp = getMetaData( rs );
112700000000             jdbc_end_object_group(temp: rsmd);
112800000000          on-error;
112900000000             jdbc_end_object_group();
113000000000             return *NULL;
113100000000          endmon;
113200000000          return rsmd;
113300000000      /end-free
113400000000     P                 E
113500000000
113600000000
113700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
113800000000      *  JDBC_GetColCount():  Get ResultSet Column Count
113900000000      *
114000000000      *       rsmd = (input) ResultSet MetaData handle
114100000000      *
114200000000      *  Returns column count
114300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
114400000000     P JDBC_GetColCount...
114500000000     P                 B                   export
114600000000     D JDBC_GetColCount...
114700000000     D                 PI            10I 0
114800000000     D   rsmd                              like(ResultSetMetaData)
114900000000     D colcnt          s                   like(jInt)
115000000000      /free
115100000000          jdbc_begin_object_group(50);
115200000000          monitor;
115300000000             colcnt = getColumnCount( rsmd );
115400000000          on-error;
115500000000             colcnt = 0;
115600000000          endmon;
115700000000          jdbc_end_object_group();
115800000000          return colcnt;
115900000000      /end-free
116000000000     P                 E
116100000000
116200000000
116300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
116400000000      *  JDBC_GetColName():  Get column name from ResultSet MetaData
116500000000      *
116600000000      *     rsmd = (input) ResultSet MetaData handle to use
116700000000      *      col = (input) column number to retrieve
116800000000      *
116900000000      *  Returns column name if successful, or '' otherwise
117000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
117100000000     P JDBC_GetColName...
117200000000     P                 B                   export
117300000000     D JDBC_GetColName...
117400000000     D                 PI           256A   varying
117500000000     D   rsmd                              like(ResultSetMetaData)
117600000000     D   col                         10I 0 value
117700000000     D result          s            256A   varying
117800000000     D str             s                   like(jString)
117900000000      /free
118000000000          jdbc_begin_object_group(10);
118100000000          monitor;
118200000000             str = getColumnName(rsmd: col);
118300000000             if (str = *NULL);
118400000000                result = '';
118500000000             else;
118600000000                result = r(str);
118700000000             endif;
118800000000          on-error;
118900000000             result = '';
119000000000          endmon;
119100000000          jdbc_end_object_group();
119200000000          return result;
119300000000      /end-free
119400000000     P                 E
119500000000
119600000000
119700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
119800000000      *  JDBC_GetColDspSize():  Get column display size from ResultSet MetaData
119900000000      *
120000000000      *     rsmd = (input) ResultSet MetaData handle to use
120100000000      *      col = (input) column number to retrieve
120200000000      *
120300000000      *  Returns column display size
120400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
120500000000     P JDBC_GetColDspSize...
120600000000     P                 B                   export
120700000000     D JDBC_GetColDspSize...
120800000000     D                 PI            10I 0
120900000000     D   rsmd                              like(ResultSetMetaData)
121000000000     D   col                         10I 0 value
121100000000     D colsize         s                   like(jInt)
121200000000      /free
121300000000          jdbc_begin_object_group(10);
121400000000          monitor;
121500000000             colsize = getColumnDisplaySize(rsmd: col);
121600000000          on-error;
121700000000             colsize = 0;
121800000000          endmon;
121900000000          jdbc_end_object_group();
122000000000          return colsize;
122100000000      /end-free
122200000000     P                 E
122300000000
122400000000
122500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
122600000000      *  JDBC_GetColTypeName():  Get column SQL type name from ResultSet MetaData
122700000000      *
122800000000      *     rsmd = (input) ResultSet MetaData handle to use
122900000000      *      col = (input) column number to retrieve
123000000000      *
123100000000      *  Returns column SQL type name if successful, or '' otherwise
123200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
123300000000     P JDBC_GetColTypName...
123400000000     P                 B                   export
123500000000     D JDBC_GetColTypName...
123600000000     D                 PI           256A   varying
123700000000     D   rsmd                              like(ResultSetMetaData)
123800000000     D   col                         10I 0 value
123900000000     D result          s            256A   varying
124000000000     D str             s                   like(jString)
124100000000      /free
124200000000          jdbc_begin_object_group(10);
124300000000          monitor;
124400000000             str = getColumnTypeName(rsmd: col);
124500000000             if (str = *NULL);
124600000000                result = '';
124700000000             else;
124800000000                result = r(str);
124900000000             endif;
125000000000          on-error;
125100000000             result = '';
125200000000          endmon;
125300000000          jdbc_end_object_group();
125400000000          return result;
125500000000      /end-free
125600000000     P                 E
125700000000
125800000000
125900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
126000000000      * JDBC_SetString(): Set a string in a prepared statement
126100000000      *
126200000000      *    prep = (input) Prepared statement to set string in
126300000000      *     idx = (input) Parameter index to set
126400000000      *     str = (input) String to set
126500000000      *    null = (input/optional) set field to NULL in database
126600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
126700000000     P JDBC_SetString...
126800000000     P                 B                   export
126900000000     D JDBC_SetString...
127000000000     D                 PI             1N
127100000000     D   prep                              like(PreparedStatement) const
127200000000     D   idx                         10I 0 value
127300000000     D   str                      32767A   varying const options(*varsize)
127400000000     D   null                         1N   const options(*nopass)
127500000000     D result          s              1n
127600000000      /free
127700000000          jdbc_begin_object_group(10);
127800000000
127900000000          monitor;
128000000000             if (%parms>=4 and null=*ON);
128100000000                setString( prep: idx: *NULL);
128200000000             else;
128300000000                setString( prep: idx: s(str));
128400000000             endif;
128500000000             result = *ON;
128600000000          on-error;
128700000000             result = *OFF;
128800000000          endmon;
128900000000
129000000000          jdbc_end_object_group();
129100000000          return result;
129200000000      /end-free
129300000000     P                 E
129400000000
129500000000
129600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
129700000000      * JDBC_SetInt(): Set an integer in a prepared statement
129800000000      *
129900000000      *    prep = (input) Prepared statement to set value in
130000000000      *     idx = (input) Parameter index to set
130100000000      *     int = (input) value to set
130200000000      *    null = (input/optional) set field to NULL in database
130300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
130400000000     P JDBC_SetInt     B                   export
130500000000     D JDBC_SetInt     PI             1N
130600000000     D   prep                              like(PreparedStatement) const
130700000000     D   idx                         10I 0 value
130800000000     D   int                         10I 0 value
130900000000     D   null                         1N   const options(*nopass)
131000000000     D result          s              1n
131100000000      /free
131200000000          jdbc_begin_object_group(10);
131300000000          monitor;
131400000000             if (%parms>=4 and null=*ON);
131500000000                setString( prep: idx: *NULL);
131600000000             else;
131700000000                setInt( prep: idx: int);
131800000000             endif;
131900000000             result = *ON;
132000000000          on-error;
132100000000             result = *OFF;
132200000000          endmon;
132300000000          jdbc_end_object_group();
132400000000          return result;
132500000000      /end-free
132600000000     P                 E
132700000000
132800000000
132900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
133000000000      * JDBC_SetDouble(): Set a floating point value in a prepared
133100000000      *                    statement
133200000000      *
133300000000      *    prep = (input) Prepared statement to set value in
133400000000      *     idx = (input) Parameter index to set
133500000000      *  double = (input) value to set
133600000000      *    null = (input/optional) set field to NULL in database
133700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
133800000000     P JDBC_SetDouble...
133900000000     P                 B                   export
134000000000     D JDBC_SetDouble...
134100000000     D                 PI             1N
134200000000     D   prep                              like(PreparedStatement) const
134300000000     D   idx                         10I 0 value
134400000000     D   double                       8F   value
134500000000     D   null                         1N   const options(*nopass)
134600000000     D result          s              1n
134700000000      /free
134800000000          jdbc_begin_object_group(10);
134900000000          monitor;
135000000000             if (%parms>=4 and null=*ON);
135100000000                setString( prep: idx: *NULL);
135200000000             else;
135300000000                setDouble( prep: idx: double);
135400000000             endif;
135500000000             result = *ON;
135600000000          on-error;
135700000000             result = *OFF;
135800000000          endmon;
135900000000          jdbc_end_object_group();
136000000000          return result;
136100000000      /end-free
136200000000     P                 E
136300000000
136400000000
136500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
136600000000      * JDBC_SetDecimal(): Set a decimal value in a prepared stmt
136700000000      *
136800000000      *    prep = (input) Prepared statement to set value in
136900000000      *     idx = (input) Parameter index to set
137000000000      *     dec = (input) value to set
137100000000      *    null = (input/optional) set field to NULL in database
137200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
137300000000     P JDBC_SetDecimal...
137400000000     P                 B                   export
137500000000     D JDBC_SetDecimal...
137600000000     D                 PI             1N
137700000000     D   prep                              like(PreparedStatement) const
137800000000     D   idx                         10I 0 value
137900000000     D   dec                         30P 9 value
138000000000     D   null                         1N   const options(*nopass)
138100000000     D result          s              1n
138200000000      /free
138300000000          jdbc_begin_object_group(10);
138400000000          monitor;
138500000000             if (%parms>=4 and null=*ON);
138600000000                setBigDecimal(prep: idx: *NULL);
138700000000             else;
138800000000                setBigDecimal(prep: idx: new_BigDecimal(s(%char(dec))));
138900000000             endif;
139000000000             result = *ON;
139100000000          on-error;
139200000000             result = *OFF;
139300000000          endmon;
139400000000          jdbc_end_object_group();
139500000000          return result;
139600000000      /end-free
139700000000     P                 E
139800000000
139900000000
140000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
140100000000      * JDBC_SetDate(): Set a date value in a prepared stmt
140200000000      *
140300000000      *    prep = (input) Prepared statement to set value in
140400000000      *     idx = (input) Parameter index to set
140500000000      *    date = (input) value to set
140600000000      *    null = (input/optional) set field to NULL in database
140700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
140800000000     P JDBC_SetDate...
140900000000     P                 B                   export
141000000000     D JDBC_SetDate...
141100000000     D                 PI             1N
141200000000     D   prep                              like(PreparedStatement) const
141300000000     D   idx                         10I 0 value
141400000000     D   date                          D   datfmt(*iso) const
141500000000     D   null                         1N   const options(*nopass)
141600000000     D epoch           s               z   static
141700000000     D                                     inz(z'1970-01-01-00.00.00.000000')
141800000000     D ts              s               z
141900000000     D ms              s                   like(jLong)
142000000000     D jdt             s                   like(jSqlDate)
142100000000     D rc              s              1N   inz(*ON)
142200000000      /free
142300000000          if (JniVersion() < 1.00004);
142400000000             SndPgmMsg( 'CPF9897'
142500000000                      : 'QCPFMSG   *LIBL'
142600000000                      : 'JDK Version 1.4 or later required!'
142700000000                      : 80
142800000000                      : '*ESCAPE'
142900000000                      : '*PGMBDY'
143000000000                      : 1
143100000000                      : MsgKey
143200000000                      : ErrCode );
143300000000             return *OFF;
143400000000          endif;
143500000000
143600000000          jdbc_begin_object_group(10);
143700000000
143800000000          monitor;
143900000000             ts = %timestamp(date);
144000000000             ms = (%diff(ts:epoch:*seconds) - timezone()) * 1000;
144100000000             jdt = new_Date(ms);
144200000000             if (jdt = *NULL);
144300000000                rc = *OFF;
144400000000             else;
144500000000                if (%parms>=4 and null=*ON);
144600000000                   setDate(prep: idx: *NULL);
144700000000                else;
144800000000                   setDate(prep: idx: jdt);
144900000000                endif;
145000000000                rc = *ON;
145100000000             endif;
145200000000          on-error;
145300000000             rc = *OFF;
145400000000          endmon;
145500000000
145600000000          jdbc_end_object_group();
145700000000          return rc;
145800000000      /end-free
145900000000     P                 E
146000000000
146100000000
146200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
146300000000      * JDBC_SetTime(): Set a time value in a prepared stmt
146400000000      *
146500000000      *    prep = (input) Prepared statement to set value in
146600000000      *     idx = (input) Parameter index to set
146700000000      *    time = (input) value to set
146800000000      *    null = (input/optional) set field to NULL in database
146900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
147000000000     P JDBC_SetTime...
147100000000     P                 B                   export
147200000000     D JDBC_SetTime...
147300000000     D                 PI             1N
147400000000     D   prep                              like(PreparedStatement) const
147500000000     D   idx                         10I 0 value
147600000000     D   time                          T   timfmt(*hms) const
147700000000     D   null                         1N   const options(*nopass)
147800000000     D epoch           s               z   static
147900000000     D                                     inz(z'1970-01-01-00.00.00.000000')
148000000000     D epdate          s               D   inz(d'1970-01-01') datfmt(*iso)
148100000000     D                                     static
148200000000     D ts              s               z
148300000000     D ms              s                   like(jLong)
148400000000     D jtm             s                   like(jSqlTime)
148500000000     D rc              s              1N   inz(*ON)
148600000000      /free
148700000000          if (JniVersion() < 1.00004);
148800000000             SndPgmMsg( 'CPF9897'
148900000000                      : 'QCPFMSG   *LIBL'
149000000000                      : 'JDK Version 1.4 or later required!'
149100000000                      : 80
149200000000                      : '*ESCAPE'
149300000000                      : '*PGMBDY'
149400000000                      : 1
149500000000                      : MsgKey
149600000000                      : ErrCode );
149700000000             return *OFF;
149800000000          endif;
149900000000
150000000000          jdbc_begin_object_group(10);
150100000000
150200000000          monitor;
150300000000             ts = epdate + time;
150400000000             ms = (%diff(ts:epoch:*seconds) - timezone()) * 1000;
150500000000             jtm = new_Time(ms);
150600000000             if (jtm = *NULL);
150700000000                rc = *OFF;
150800000000             else;
150900000000                if (%parms>=4 and null=*ON);
151000000000                   setTime(prep: idx: *NULL);
151100000000                else;
151200000000                   setTime(prep: idx: jtm);
151300000000                endif;
151400000000                rc = *ON;
151500000000             endif;
151600000000          on-error;
151700000000             rc = *OFF;
151800000000          endmon;
151900000000
152000000000          jdbc_end_object_group();
152100000000          return rc;
152200000000      /end-free
152300000000     P                 E
152400000000
152500000000
152600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
152700000000      * JDBC_SetTimestamp(): Set a timestamp value in a prepared stmt
152800000000      *
152900000000      *    prep = (input) Prepared statement to set value in
153000000000      *     idx = (input) Parameter index to set
153100000000      *      ts = (input) value to set
153200000000      *    null = (input/optional) set field to NULL in database
153300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
153400000000     P JDBC_SetTimestamp...
153500000000     P                 B                   export
153600000000     D JDBC_SetTimestamp...
153700000000     D                 PI             1N
153800000000     D   prep                              like(PreparedStatement) const
153900000000     D   idx                         10I 0 value
154000000000     D   ts                            Z   const
154100000000     D   null                         1N   const options(*nopass)
154200000000     D epoch           s               Z   static
154300000000     D                                     inz(z'1970-01-01-00.00.00.000000')
154400000000     D ms              s                   like(jLong)
154500000000     D jts             s                   like(jSqlTimestamp)
154600000000     D rc              s              1N   inz(*ON)
154700000000      /free
154800000000          if (JniVersion() < 1.00004);
154900000000             SndPgmMsg( 'CPF9897'
155000000000                      : 'QCPFMSG   *LIBL'
155100000000                      : 'JDK Version 1.4 or later required!'
155200000000                      : 80
155300000000                      : '*ESCAPE'
155400000000                      : '*PGMBDY'
155500000000                      : 1
155600000000                      : MsgKey
155700000000                      : ErrCode );
155800000000             return *OFF;
155900000000          endif;
156000000000
156100000000          jdbc_begin_object_group(10);
156200000000
156300000000          monitor;
156400000000             ms = (%diff(ts:epoch:*seconds) - timezone()) * 1000;
156500000000             jts = new_Timestamp(ms);
156600000000             if (jts = *NULL);
156700000000                rc = *OFF;
156800000000             else;
156900000000                if (%parms>=4 and null=*ON);
157000000000                   setTimestamp(prep: idx: *NULL);
157100000000                else;
157200000000                   setTimestamp(prep: idx: jts);
157300000000                endif;
157400000000                rc = *ON;
157500000000             endif;
157600000000          on-error;
157700000000             rc = *OFF;
157800000000          endmon;
157900000000
158000000000          jdbc_end_object_group();
158100000000          return rc;
158200000000      /end-free
158300000000     P                 E
158400000000
158500000000
158600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
158700000000      * JDBC_RegisterOutParameter(): Register an output parameter
158800000000      *                           returned from a stored-proc call
158900000000      *
159000000000      *    call = (input) Callable statement to register output for
159100000000      *     idx = (input) Parameter index to register
159200000000      *    type = (input) Data type to register
159300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
159400000000     P JDBC_RegisterOutParameter...
159500000000     P                 B                   export
159600000000     D JDBC_RegisterOutParameter...
159700000000     D                 PI
159800000000     D   call                              like(CallableStatement) const
159900000000     D   idx                         10I 0 value
160000000000     D   type                        10I 0 value
160100000000      /free
160200000000          jdbc_begin_object_group(10);
160300000000          monitor;
160400000000             registerOutParameter( call: idx: type);
160500000000          on-error;
160600000000             // ignore error
160700000000          endmon;
160800000000          jdbc_end_object_group();
160900000000          return;
161000000000      /end-free
161100000000     P                 E
161200000000
161300000000
161400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
161500000000      * JDBC_GetString(): Get a string from a called statement
161600000000      *
161700000000      *    call = (input) Callable statement to get string from
161800000000      *     idx = (input) Parameter index to get
161900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
162000000000     P JDBC_GetString...
162100000000     P                 B                   export
162200000000     D JDBC_GetString...
162300000000     D                 PI         32767A   varying
162400000000     D   call                              like(CallableStatement) const
162500000000     D   idx                         10I 0 value
162600000000     D result          s          32767A   varying
162700000000     D str             s                   like(jString)
162800000000      /free
162900000000          jdbc_begin_object_group(10);
163000000000
163100000000          monitor;
163200000000             str = getString( call: idx);
163300000000             if (str = *NULL);
163400000000                result = '';
163500000000             else;
163600000000                result = r(str);
163700000000             endif;
163800000000          on-error;
163900000000             result = '';
164000000000          endmon;
164100000000
164200000000          jdbc_end_object_group();
164300000000          return result;
164400000000      /end-free
164500000000     P                 E
164600000000
164700000000
164800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
164900000000      * JDBC_GetInt(): Get an integer from a called statement
165000000000      *
165100000000      *    call = (input) Callable statement to get string from
165200000000      *     idx = (input) Parameter index to get
165300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
165400000000     P JDBC_GetInt...
165500000000     P                 B                   export
165600000000     D JDBC_GetInt...
165700000000     D                 PI            10I 0
165800000000     D   call                              like(CallableStatement) const
165900000000     D   idx                         10I 0 value
166000000000     D result          s             10I 0
166100000000      /free
166200000000          jdbc_begin_object_group(10);
166300000000
166400000000          monitor;
166500000000             result = getInt( call: idx);
166600000000          on-error;
166700000000             result = 0;
166800000000          endmon;
166900000000
167000000000          jdbc_end_object_group();
167100000000          return result;
167200000000      /end-free
167300000000     P                 E
167400000000
167500000000
167600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
167700000000      * JDBC_GetShort(): Get a short from a called statement
167800000000      *
167900000000      *    call = (input) Callable statement to get string from
168000000000      *     idx = (input) Parameter index to get
168100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
168200000000     P JDBC_GetShort...
168300000000     P                 B                   export
168400000000     D JDBC_GetShort...
168500000000     D                 PI            10I 0
168600000000     D   call                              like(CallableStatement) const
168700000000     D   idx                         10I 0 value
168800000000     D result          s             10I 0
168900000000     D short           s                   like(jShort)
169000000000      /free
169100000000          jdbc_begin_object_group(10);
169200000000
169300000000          monitor;
169400000000             short = getShort( call: idx);
169500000000             if (short = 0);
169600000000                result = 0;
169700000000             else;
169800000000                result = short;
169900000000             endif;
170000000000          on-error;
170100000000             result = 0;
170200000000          endmon;
170300000000
170400000000          jdbc_end_object_group();
170500000000          return result;
170600000000      /end-free
170700000000     P                 E
170800000000
170900000000
171000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
171100000000      * JDBC_GetBoolean(): Get a boolean from a called statement
171200000000      *
171300000000      *    call = (input) Callable statement to get string from
171400000000      *     idx = (input) Parameter index to get
171500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
171600000000     P JDBC_GetBoolean...
171700000000     P                 B                   export
171800000000     D JDBC_GetBoolean...
171900000000     D                 PI             1N
172000000000     D   call                              like(CallableStatement) const
172100000000     D   idx                         10I 0 value
172200000000     D result          s              1N
172300000000      /free
172400000000          jdbc_begin_object_group(10);
172500000000          monitor;
172600000000             result = getBoolean( call: idx);
172700000000          on-error;
172800000000             result = *OFF;
172900000000          endmon;
173000000000          jdbc_end_object_group();
173100000000          return result;
173200000000      /end-free
173300000000     P                 E
173400000000
173500000000
173600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
173700000000      * JDBC_ExecPrepQry(): Execute a query from a prepared statement
173800000000      *
173900000000      *      prep = (input) prepared statement to execute
174000000000      *
174100000000      * Returns ResultSet object or *NULL upon failure
174200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
174300000000     P JDBC_ExecPrepQry...
174400000000     P                 B                   export
174500000000     D JDBC_ExecPrepQry...
174600000000     D                 PI                  like(ResultSet)
174700000000     D   prep                              like(PreparedStatement) const
174800000000     D temp            s                   like(ResultSet)
174900000000     D rs              s                   like(ResultSet)
175000000000      /free
175100000000          jdbc_begin_object_group(50);
175200000000          monitor;
175300000000             temp = PrepExecuteQuery(prep);
175400000000             jdbc_end_object_group(temp: rs);
175500000000          on-error;
175600000000             jdbc_end_object_group();
175700000000             return *null;
175800000000          endmon;
175900000000          return rs;
176000000000      /end-free
176100000000     P                 E
176200000000
176300000000
176400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
176500000000      * JDBC_ExecPrepUpd(): Execute SQL that doesn't return data
176600000000      *                     from a prepared statement
176700000000      *
176800000000      *      prep = (input) prepared statement to execute
176900000000      *
177000000000      * Returns 0 or a row count if successful
177100000000      *     or -1 upon failure
177200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
177300000000     P JDBC_ExecPrepUpd...
177400000000     P                 B                   export
177500000000     D JDBC_ExecPrepUpd...
177600000000     D                 PI            10I 0
177700000000     D   prep                              like(PreparedStatement) const
177800000000     D rc              s             10i 0
177900000000      /free
178000000000          jdbc_begin_object_group(50);
178100000000          monitor;
178200000000             rc = PrepExecuteUpdate(prep);
178300000000          on-error;
178400000000             rc = -1;
178500000000          endmon;
178600000000          jdbc_end_object_group();
178700000000          return rc;
178800000000      /end-free
178900000000     P                 E
179000000000
179100000000
179200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
179300000000      * JDBC_ExecCall(): Execute SQL statement that calls a stored
179400000000      *                     procedure
179500000000      *
179600000000      *      call = (input) callable statement to execute
179700000000      *
179800000000      * Returns *ON if first result is a ResultSet
179900000000      *      or *OFF if first result is an update count
180000000000      *
180100000000      * Use JDBC_moreResults(), JDBC_getUpdateCount() and
180200000000      *     JDBC_getResultSet() to get results of this function
180300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
180400000000     P JDBC_ExecCall...
180500000000     P                 B                   export
180600000000     D JDBC_ExecCall...
180700000000     D                 PI             1N
180800000000     D   call                              like(CallableStatement) const
180900000000     D rc              s              1n
181000000000      /free
181100000000          jdbc_begin_object_group(50);
181200000000          monitor;
181300000000             rc = CallExecute(call);
181400000000          on-error;
181500000000             rc = *OFF;
181600000000          endmon;
181700000000          jdbc_end_object_group();
181800000000          return rc;
181900000000      /end-free
182000000000     P                 E
182100000000
182200000000
182300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
182400000000      * JDBC_Execute(): Execute Prepared SQL Statement
182500000000      *
182600000000      *      stmt = (input) prepared statement to execute
182700000000      *
182800000000      * Returns *ON if first result is a ResultSet
182900000000      *      or *OFF if first result is an update count
183000000000      *
183100000000      * Use JDBC_moreResults(), JDBC_getUpdateCount() and
183200000000      *     JDBC_getResultSet() to get results of this function
183300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
183400000000     P JDBC_Execute...
183500000000     P                 B                   export
183600000000     D JDBC_Execute...
183700000000     D                 PI             1N
183800000000     D   stmt                              like(PreparedStatement) const
183900000000     D rc              s              1n
184000000000      /free
184100000000          jdbc_begin_object_group(50);
184200000000          monitor;
184300000000             rc = PrepExecute(stmt);
184400000000          on-error;
184500000000             rc = *OFF;
184600000000          endmon;
184700000000          return rc;
184800000000      /end-free
184900000000     P                 E
185000000000
185100000000
185200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
185300000000      * JDBC_FreePrepStmt(): Free prepared statement
185400000000      *
185500000000      *    prep = (input) Prepared Statement to Free
185600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
185700000000     P JDBC_FreePrepStmt...
185800000000     P                 B                   export
185900000000     D JDBC_FreePrepStmt...
186000000000     D                 PI
186100000000     D   prep                              like(PreparedStatement)
186200000000      /free
186300000000         stmt_close(prep);
186400000000         DeleteLocalRef(JNIENV_P: prep);
186500000000         prep = *NULL;
186600000000      /end-free
186700000000     P                 E
186800000000
186900000000
187000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
187100000000      * JDBC_FreeCallStmt(): Free callable statement
187200000000      *
187300000000      *    call = (input) Callable Statement to Free
187400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
187500000000     P JDBC_FreeCallStmt...
187600000000     P                 B                   export
187700000000     D JDBC_FreeCallStmt...
187800000000     D                 PI
187900000000     D   call                              like(CallableStatement)
188000000000      /free
188100000000         DeleteLocalRef(JNIENV_P: call);
188200000000         call = *NULL;
188300000000      /end-free
188400000000     P                 E
188500000000
188600000000
188700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
188800000000      *  JDBC_NextRow():  Position result to next row
188900000000      *
189000000000      *       rs = (input) Result handle to move
189100000000      *
189200000000      *  Returns *ON if successful, *OFF otherwise
189300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
189400000000     P JDBC_NextRow    B                   export
189500000000     D JDBC_NextRow    PI             1N
189600000000     D   rs                                like(ResultSet)
189700000000      /free
189800000000          monitor;
189900000000             return nextRec(rs);
190000000000          on-error;
190100000000             return *OFF;
190200000000          endmon;
190300000000      /end-free
190400000000     P                 E
190500000000
190600000000
190700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
190800000000      *  JDBC_GetCol():  Get column from current row
190900000000      *
191000000000      *       rs = (input) Result handle to use
191100000000      *      col = (input) column number to retrieve
191200000000      *  nullInd = (output/optional) will be set to *ON if the
191300000000      *             field is null, or *OFF otherwise.
191400000000      *
191500000000      *  Returns column value if successful, or '' otherwise
191600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
191700000000     P JDBC_GetCol     B                   export
191800000000     D JDBC_GetCol     PI         32767A   varying
191900000000     D   rs                                like(ResultSet)
192000000000     D   col                         10I 0 value
192100000000     D   nullInd                      1N   options(*nopass:*omit)
192200000000     D result          s          32767A   varying
192300000000     D str             s                   like(jstring)
192400000000     D null            s              1N   inz(*OFF)
192500000000      /free
192600000000          jdbc_begin_object_group(5);
192700000000          monitor;
192800000000             str = getColString(rs: col);
192900000000             if (str = *NULL);
193000000000                result = '';
193100000000                null = *ON;
193200000000             else;
193300000000                result = r(str);
193400000000             endif;
193500000000          on-error;
193600000000             null = *ON;
193700000000             result = '';
193800000000          endmon;
193900000000          jdbc_end_object_group();
194000000000          if (%parms >= 3 and %addr(nullInd)<>*NULL);
194100000000              nullInd = Null;
194200000000          endif;
194300000000          return result;
194400000000      /end-free
194500000000     P                 E
194600000000
194700000000
194800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
194900000000      *  JDBC_FreeResult(): Free result handle
195000000000      *
195100000000      *     rs = (input) Result handle to free
195200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
195300000000     P JDBC_FreeResult...
195400000000     P                 B                   export
195500000000     D JDBC_FreeResult...
195600000000     D                 PI
195700000000     D   rs                                like(ResultSet)
195800000000     D stmt            s                   like(Statement)
195900000000      /free
196000000000          stmt = rs_getStatement(rs);
196100000000          stmt_close(stmt);
196200000000          DeleteLocalRef(JNIENV_P: rs);
196300000000          DeleteLocalRef(JNIENV_P: stmt);
196400000000          rs = *NULL;
196500000000      /end-free
196600000000     P                 E
196700000000
196800000000
196900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
197000000000      *  JDBC_Commit():  Commit transaction
197100000000      *
197200000000      *     conn = (input) Connection to commit on
197300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
197400000000     P JDBC_Commit     B                   export
197500000000     D JDBC_Commit     PI
197600000000     D   conn                              like(Connection)
197700000000      /free
197800000000          commitTrn(conn);
197900000000      /end-free
198000000000     P                 E
198100000000
198200000000
198300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
198400000000      *  JDBC_Rollback():  Rollback transaction
198500000000      *
198600000000      *     conn = (input) Connection to rollback on
198700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
198800000000     P JDBC_Rollback   B                   export
198900000000     D JDBC_Rollback   PI
199000000000     D   conn                              like(Connection)
199100000000      /free
199200000000          rollbackTrn(conn);
199300000000      /end-free
199400000000     P                 E
199500000000
199600000000
199700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
199800000000      *  JDBC_Close():  Close connection to server
199900000000      *                   and free connection handle
200000000000      *
200100000000      *     conn = (input) Connection to close
200200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
200300000000     P JDBC_Close      B                   export
200400000000     D JDBC_Close      PI
200500000000     D   conn                              like(Connection)
200600000000      /free
200700000000          closeConn(conn);
200800000000          DeleteLocalRef(JNIENV_P: conn);
200900000000          conn = *NULL;
201000000000      /end-free
201100000000     P                 E
201200000000
201300000000
201400000000      *-----------------------------------------------------------------
201500000000      *  jdbc_get_jni_env():
201600000000      *
201700000000      *  Service program utility to get a pointer to the JNI environment
201800000000      *  you'll need this pointer in order to call many of the JNI
201900000000      *  routines.
202000000000      *
202100000000      *  returns the pointer, or *NULL upon error
202200000000      *-----------------------------------------------------------------
202300000000     P jdbc_get_jni_env...
202400000000     P                 B
202500000000     D jdbc_get_jni_env...
202600000000     D                 PI              *
202700000000
202800000000     D wwEnv           s               *
202900000000
203000000000      /free
203100000000        wwEnv = attach_jvm();
203200000000        if (wwEnv = *NULL);
203300000000           wwEnv = start_jvm();
203400000000        endif;
203500000000
203600000000        JNIENV_P = wwEnv;
203700000000        return wwEnv;
203800000000      /end-free
203900000000     P                 E
204000000000
204100000000
204200000000      *-----------------------------------------------------------------
204300000000      * jni_checkError():  Check for an error in JNI routines
204400000000      *-----------------------------------------------------------------
204500000000     P jni_checkError  B
204600000000     D jni_checkError  PI             1N
204700000000     D    errString                 256A   varying options(*omit)
204800000000
204900000000     D sleep           pr            10I 0 extproc('sleep')
205000000000     D   intv                        10I 0 value
205100000000
205200000000     D exc             s                   like(jthrowable)
205300000000     D str             s                   like(jstring)
205400000000     d msg             s            256A   varying
205500000000      /free
205600000000          if (%addr(errString) <> *NULL);
205700000000              %len(errString) = 0;
205800000000          endif;
205900000000
206000000000          exc = ExceptionOccurred(JNIENV_P);
206100000000          if (exc = *NULL);
206200000000              return *OFF;
206300000000          endif;
206400000000
206500000000          ExceptionDescribe(JNIENV_P);
206600000000          sleep(10);
206700000000
206800000000          str = Exception_getMessage(exc);
206900000000          msg = r(str);
207000000000          DeleteLocalRef(JNIENV_P: str);
207100000000
207200000000          SndPgmMsg( 'CPF9897'
207300000000                   : 'QCPFMSG   *LIBL'
207400000000                   : msg
207500000000                   : %len(msg)
207600000000                   : '*DIAG'
207700000000                   : '*'
207800000000                   : 0
207900000000                   : MsgKey
208000000000                   : ErrCode );
208100000000
208200000000          if (%addr(errString) <> *NULL);
208300000000              errString = msg;
208400000000          endif;
208500000000
208600000000          ExceptionClear(JNIENV_P);
208700000000          DeleteLocalRef(JNIENV_P: exc);
208800000000
208900000000          return *ON;
209000000000      /end-free
209100000000     P                 E
209200000000
209300000000
209400000000      *-----------------------------------------------------------------
209500000000      * jdbc_begin_object_group():  Start a new group of objects
209600000000      *    which will all be freed when jdbc_end_object_group()
209700000000      *    gets called.
209800000000      *
209900000000      *   peCapacity = maximum number of objects that can be
210000000000      *        referenced within this object group.
210100000000      *
210200000000      *  NOTE: According to the 1.2 JNI Spec, you can create more
210300000000      *        objects in the new frame than peCapacity allows.  The
210400000000      *        peCapacity is the guarenteed number.   When no object
210500000000      *        groups are used, 16 references are guarenteed, so if
210600000000      *        you specify 16 here, that would be comparable to a
210700000000      *        "default value".
210800000000      *
210900000000      * Returns 0 if successful, or -1 upon error
211000000000      *-----------------------------------------------------------------
211100000000     P jdbc_begin_object_group...
211200000000     P                 B
211300000000     D jdbc_begin_object_group...
211400000000     D                 PI            10I 0
211500000000     D    peCapacity                 10I 0 value
211600000000
211700000000     D wwEnv           s               *
211800000000     D wwRC            s             10I 0
211900000000
212000000000      /free
212100000000
212200000000       wwEnv = jdbc_get_jni_env();
212300000000       if (wwEnv = *NULL);
212400000000           return -1;
212500000000       endif;
212600000000
212700000000       if  ( PushLocalFrame (wwEnv: peCapacity) <> JNI_OK );
212800000000           return -1;
212900000000       else;
213000000000           return 0;
213100000000       endif;
213200000000
213300000000      /end-free
213400000000     P                 E
213500000000
213600000000
213700000000      *-----------------------------------------------------------------
213800000000      * jdbc_end_object_group():  Frees all Java objects that
213900000000      *    have been created since calling jdbc_begin_object_group()
214000000000      *
214100000000      *        peOldObj = (see below)
214200000000      *        peNewObj = Sometimes it's desirable to preserve one
214300000000      *            object by moving it from the current object group
214400000000      *            to the parent group.   These parameters allow you
214500000000      *            to make that move.
214600000000      *
214700000000      * Returns 0 if successful, or -1 upon error
214800000000      *-----------------------------------------------------------------
214900000000     P jdbc_end_object_group...
215000000000     P                 B
215100000000     D jdbc_end_object_group...
215200000000     D                 PI            10I 0
215300000000     D   peOldObj                          like(jObject) const
215400000000     D                                     options(*nopass)
215500000000     D   peNewObj                          like(jObject)
215600000000     D                                     options(*nopass)
215700000000
215800000000     D wwOld           s                   like(jObject) inz(*NULL)
215900000000     D wwNew           s                   like(jObject)
216000000000
216100000000      /free
216200000000
216300000000          jdbc_get_jni_env();
216400000000          if (JNIENV_p = *NULL);
216500000000              return -1;
216600000000          endif;
216700000000
216800000000          if %parms >= 2;
216900000000              wwOld = peOldObj;
217000000000          endif;
217100000000
217200000000          wwNew = PopLocalFrame (JNIENV_p: wwOld);
217300000000
217400000000          if %parms >= 2;
217500000000              peNewObj = wwNew;
217600000000          endif;
217700000000
217800000000          return 0;
217900000000
218000000000      /end-free
218100000000     P                 E
218200000000
218300000000
218400000000      *-----------------------------------------------------------------
218500000000      *  start_jvm():   Start the Java Virtual Machine (JVM)
218600000000      *
218700000000      *  NOTE: Originally, this called JNI routines to start a new JVM,
218800000000      *        but that meant that a classpath and other options needed
218900000000      *        to be set manually in the JNI invocation.
219000000000      *
219100000000      *        I decided that it would be better to reduce the complexity
219200000000      *        and let RPG start the JVM, so I merely create & destroy
219300000000      *        a string here so that RPG will automatically start the
219400000000      *        JVM for me.
219500000000      *
219600000000      *  returns a pointer to the JNI environment
219700000000      *          or *NULL upon failure.
219800000000      *-----------------------------------------------------------------
219900000000     P start_jvm       B
220000000000     D start_jvm       PI              *
220100000000
220200000000     D wwStr           s                   like(jString)
220300000000
220400000000      /free
220500000000         status('Starting Java Virtual Machine...');
220600000000         wwStr = s('Temp String');
220700000000         status('.');
220800000000         JNIENV_P = attach_jvm();
220900000000         DeleteLocalRef(JNIENV_P: wwStr);
221000000000         return JNIENV_P;
221100000000      /end-free
221200000000     P                 E
221300000000
221400000000
221500000000      *-----------------------------------------------------------------
221600000000      * attach_jvm():  Attach to JVM if it's running
221700000000      *
221800000000      * Returns a pointer to the JNI environment, or *NULL upon error
221900000000      *-----------------------------------------------------------------
222000000000     P attach_jvm      B
222100000000     D attach_jvm      PI              *
222200000000
222300000000     D dsAtt           ds                  likeds(JavaVMAttachArgs)
222400000000     D wwJVM           s                   like(JavaVM_p) dim(1)
222500000000     D wwJVMc          s                   like(jSize)
222600000000     D wwEnv           s               *   inz(*null) static
222700000000     D wwRC            s             10I 0
222800000000      /free
222900000000
223000000000        if (wwEnv <> *NULL);
223100000000           return wwEnv;
223200000000        endif;
223300000000
223400000000        status('Attaching RPG program to Java Virtual Machine...');
223500000000
223600000000        monitor;
223700000000           wwRC = JNI_GetCreatedJavaVMs(wwJVM: 1: wwJVMc);
223800000000
223900000000           if (wwRC <> JNI_OK  or  wwJVMc = 0);
224000000000               return *NULL;
224100000000           endif;
224200000000
224300000000           JavaVM_P = wwJVM(1);
224400000000           dsAtt = *ALLx'00';
224500000000           dsAtt.version = JNI_VERSION_1_2;
224600000000
224700000000           wwRC = AttachCurrentThread (wwJVM(1): wwEnv: %addr(dsAtt));
224800000000           if (wwRC <> JNI_OK);
224900000000               wwEnv = *NULL;
225000000000           endif;
225100000000
225200000000        on-error;
225300000000           wwEnv = *NULL;
225400000000        endmon;
225500000000
225600000000        status('.');
225700000000        return wwEnv;
225800000000
225900000000      /end-free
226000000000     P                 E
226100000000
226200000000
226300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
226400000000      * JniVersion(): Get version of current JNI environment
226500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
226600000000     P JniVersion      B
226700000000     D JniVersion      PI            10P 5
226800000000     D                 ds
226900000000     D  ver                          10I 0
227000000000     D  high                          5I 0 overlay(ver:1)
227100000000     D  low                           5I 0 overlay(ver:*NEXT)
227200000000     D result          s             10P 5 static
227300000000      /free
227400000000         if (result = 0);
227500000000            ver = GetVersion(jdbc_get_jni_env());
227600000000            result = high + (low / 100000);
227700000000         endif;
227800000000         return result;
227900000000      /end-free
228000000000     P                 E
228100000000
228200000000
228300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
228400000000      * Get UTC offset (in seconds) for timezone.
228500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
228600000000     P Timezone        B
228700000000     D Timezone        PI            10I 0
228800000000
228900000000     D CEEUTCO         PR
229000000000     D   hours                       10I 0
229100000000     D   mins                        10I 0
229200000000     D   secs                         8F
229300000000     D   fc                          12A   options(*omit)
229400000000
229500000000     D utc_offset_set  s              1N   inz(*OFF) static
229600000000     D utc_offset      s             10I 0           static
229700000000
229800000000     D junk1           s             10I 0 static
229900000000     D junk2           s             10I 0 static
230000000000     D junk3           s              8F   static
230100000000
230200000000      /free
230300000000
230400000000          if (utc_offset_set = *OFF);
230500000000             CEEUTCO(junk1:junk2:junk3:*omit);
230600000000             utc_offset = junk3;
230700000000             utc_offset_set = *ON;
230800000000          endif;
230900000000          return utc_offset;
231000000000
231100000000      /end-free
231200000000     P                 E
231300000000
231400000000
231500000000     P status          B                   export
231600000000     D status          PI             1N
231700000000     D   peMsgTxt                   256A   const
231800000000
231900000000     D SndPgmMsg       PR                  ExtPgm('QMHSNDPM')
232000000000     D   MessageID                    7A   Const
232100000000     D   QualMsgF                    20A   Const
232200000000     D   MsgData                    256A   Const
232300000000     D   MsgDtaLen                   10I 0 Const
232400000000     D   MsgType                     10A   Const
232500000000     D   CallStkEnt                  10A   Const
232600000000     D   CallStkCnt                  10I 0 Const
232700000000     D   MessageKey                   4A
232800000000     D   ErrorCode                    1A
232900000000
233000000000     D dsEC            DS
233100000000     D  dsECBytesP             1      4I 0 inz(256)
233200000000     D  dsECBytesA             5      8I 0 inz(0)
233300000000     D  dsECMsgID              9     15
233400000000     D  dsECReserv            16     16
233500000000     D  dsECMsgDta            17    256
233600000000
233700000000     D wwMsgLen        S             10I 0
233800000000     D wwTheKey        S              4A
233900000000
234000000000     c     ' '           checkr    peMsgTxt      wwMsgLen
234100000000     c                   if        wwMsgLen<1
234200000000     c                   return    *OFF
234300000000     c                   endif
234400000000
234500000000     c                   callp     SndPgmMsg('CPF9897': 'QCPFMSG   *LIBL':
234600000000     c                               peMsgTxt: wwMsgLen: '*STATUS':
234700000000     c                               '*EXT': 0: wwTheKey: dsEC)
234800000000
234900000000     c                   if        dsECBytesA>0
235000000000     c                   return    *off
235100000000     c                   else
235200000000     c                   return    *on
235300000000     c                   endif
235400000000     P                 E
235500000000
235600000000
235700000000
235800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
235900000000      *  JDBC_GetColByName():  Get column from current row by
236000000000      *                        column name
236100000000      *
236200000000      *          rs = (input) Result handle to use
236300000000      *  columnName = (input) column name to retrieve
236400000000      *     nullInd = (output/optional) will be set to *ON if the
236500000000      *               field is null, or *OFF otherwise.
236600000000      *
236700000000      *  Returns column value if successful, or '' otherwise
236800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
236900000000     P JDBC_GetColByName...
237000000000     P                 b                   Export
237100000000     D JDBC_GetColByName...
237200000000     D                 pi         32767a   varying
237300000000     D  rs                                 Like(ResultSet)
237400000000     D  ColumnName                32767a   varying Const options(*varsize)
237500000000     D  nullInd                       1N   options(*nopass:*omit)
237600000000
237700000000     D null            s              1n   inz(*off)
237800000000     D parm            s                   Like(jstring)
237900000000     D str             s                   Like(jstring)
238000000000     D retField        s          32767a
238100000000
238200000000      /free
238300000000        if (%len(ColumnName)=0 or ColumnName=*blank);
238400000000           null = *on;
238500000000           retField = '';
238600000000        else;
238700000000           jdbc_begin_object_group(100);
238800000000           Monitor;
238900000000             parm = s(ColumnName);
239000000000             str = getColStrByName(rs: parm);
239100000000             if (str = *null);
239200000000                null=*on;
239300000000                retField = '';
239400000000             else;
239500000000                null=*off;
239600000000                retField = r(str);
239700000000             endif;
239800000000           On-Error;
239900000000             retField = '';
240000000000             null = *on;
240100000000           Endmon;
240200000000           jdbc_end_object_group();
240300000000        Endif;
240400000000
240500000000        if (%parms >= 3 and %addr(nullInd)<>*NULL);
240600000000            nullInd = Null;
240700000000        endif;
240800000000
240900000000        Return retField;
241000000000      /end-free
241100000000     P                 e
241200000000
241300000000
241400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
241500000000      *  JDBC_getUpdateCount(): Get the number of rows modified
241600000000      *         by the last call to JDBC_ExecCall()
241700000000      *
241800000000      *        stmt = (input) PreparedStatement or CallableStatement
241900000000      *
242000000000      *  Returns number of modified rows, or -1 if the current
242100000000      *          result is a ResultSet or if there are no results
242200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
242300000000     P JDBC_getUpdateCount...
242400000000     P                 b                   Export
242500000000     D JDBC_getUpdateCount...
242600000000     D                 pi            10i 0
242700000000     D  stmt                               Like(Statement) const
242800000000     D getUpdateCount  pr            10i 0 extproc(*JAVA
242900000000     D                                     : 'java.sql.Statement'
243000000000     D                                     : 'getUpdatecount')
243100000000     c                   return    getUpdateCount(stmt)
243200000000     P                 E
243300000000
243400000000
243500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
243600000000      *  JDBC_getResultSet(): Get the first/next ResultSet
243700000000      *         returned by JDBC_ExecCall()
243800000000      *
243900000000      *        stmt = (input) PreparedStatement or CallableStatement
244000000000      *
244100000000      *  Returns the ResultSet or *NULL if the current result is
244200000000      *          not a ResultSet or if there are no more results
244300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
244400000000     P JDBC_getResultSet...
244500000000     P                 b                   Export
244600000000     D JDBC_getResultSet...
244700000000     D                 pi                  like(ResultSet)
244800000000     D  stmt                               Like(Statement) const
244900000000     D getResultSet    pr                  like(ResultSet)
245000000000     D                                     extproc(*JAVA
245100000000     D                                     : 'java.sql.Statement'
245200000000     D                                     : 'getResultSet')
245300000000     c                   return    getResultSet(stmt)
245400000000     P                 E
245500000000
245600000000
245700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
245800000000      *  JDBC_getMoreResults(): Move the results of this statement
245900000000      *         to the next result returned.
246000000000      *
246100000000      *        stmt = (input) PreparedStatement or CallableStatement
246200000000      *
246300000000      *  Returns *ON if the next result is a ResultSet object,
246400000000      *          or *OFF if it's an update count or there are no
246500000000      *          more results
246600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
246700000000     P JDBC_getMoreResults...
246800000000     P                 b                   Export
246900000000     D JDBC_getMoreResults...
247000000000     D                 pi             1n
247100000000     D  stmt                               Like(Statement) const
247200000000     D getMoreResults  pr             1n   extproc(*JAVA
247300000000     D                                     : 'java.sql.Statement'
247400000000     D                                     : 'getMoreResults')
247500000000     c                   return    getMoreResults(stmt)
247600000000     P                 E
