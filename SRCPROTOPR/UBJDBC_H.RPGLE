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
002700000000
002800000000      /if defined(JDBC_H_DEFINED)
002900000000      /eof
003000000000      /endif
003100000000      /define JDBC_H_DEFINED
003200000000
003300000000     D Statement       s               O   CLASS(*JAVA:
003400000000     D                                     'java.sql.Statement')
003500000000     D Connection      s               O   CLASS(*JAVA:
003600000000     D                                     'java.sql.Connection')
003700000000     D ResultSet       s               O   CLASS(*JAVA:
003800000000     D                                     'java.sql.ResultSet')
003900000000     D ResultSetMetaData...
004000000000     D                 s               O   CLASS(*JAVA:
004100000000     D                                     'java.sql.ResultSetMetaData')
004200000000     D PreparedStatement...
004300000000     D                 s               O   CLASS(*JAVA:
004400000000     D                                     'java.sql.PreparedStatement')
004500000000     D CallableStatement...
004600000000     D                 s               O   CLASS(*JAVA:
004700000000     D                                     'java.sql.CallableStatement')
004800000000     D Properties      s               O   CLASS(*JAVA:
004900000000     D                                     'java.util.Properties')
005000000000
005100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
005200000000      * MySQL_Connect(): Create a connection to a MYSQL server
005300000000      *
005400000000      *    server = (input) mysql server to connect to
005500000000      *  database = (input) database to use on server
005600000000      *    userid = (input) userid to log in with
005700000000      *  password = (input) password to log in with
005800000000      *
005900000000      * Returns a connection handle or *NULL upon failure
006000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
006100000000     D MySQL_Connect   PR                  like(Connection)
006200000000     D   server                     256A   varying const options(*varsize)
006300000000     D   database                   256A   varying const options(*varsize)
006400000000     D   userid                      50A   varying const options(*varsize)
006500000000     D   password                    50A   varying const options(*varsize)
006600000000
006700000000
006800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
006900000000      * JDBC_Connect(): Create a connection using JDBC driver
007000000000      *
007100000000      *    driver = (input) Java class name of JDBC driver to use
007200000000      *       url = (input) JDBC URL of database to connect to
007300000000      *    userid = (input) userid to log in with
007400000000      *  password = (input) password to log in with
007500000000      *
007600000000      * Returns a connection handle or *NULL upon failure
007700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
007800000000     D JDBC_Connect    PR                  like(Connection)
007900000000     D   driver                     256A   varying const options(*varsize)
008000000000     D   url                        256A   varying const options(*varsize)
008100000000     D   userid                      50A   varying const options(*varsize)
008200000000     D   password                    50A   varying const options(*varsize)
008300000000
008400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
008500000000      * JDBC_ConnProp(): Connect to database w/properties object
008600000000      *
008700000000      *    driver = (input) Java class name of JDBC driver to use
008800000000      *       url = (input) JDBC URL to connect to
008900000000      *      prop = (input) properties to use when connecting
009000000000      *
009100000000      * Returns a connection handle or *NULL upon failure
009200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
009300000000     D JDBC_ConnProp   PR                  like(Connection)
009400000000     D   driver                     256A   varying const options(*varsize)
009500000000     D   url                        256A   varying const options(*varsize)
009600000000     D   prop                              like(Properties)
009700000000
009800000000
009900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
010000000000      *  JDBC_Properties():  Create new properties object
010100000000      *
010200000000      *  returns the object.
010300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
010400000000     D JDBC_Properties...
010500000000     D                 PR                  like(Properties)
010600000000
010700000000
010800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
010900000000      *  JDBC_setProp():  Set one property in Properties object
011000000000      *
011100000000      *     prop = (i/o) Properties object to set property in
011200000000      *      key = (input) property to set
011300000000      *    value = (input) value to assign to property
011400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
011500000000     D JDBC_setProp    PR
011600000000     D   prop                              like(Properties)
011700000000     D   key                        256A   varying const options(*varsize)
011800000000     D   value                      256A   varying const options(*varsize)
011900000000
012000000000
012100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
012200000000      *  JDBC_freeProp():  Release properties object
012300000000      *
012400000000      *     prop = (i/o) Properties object to release
012500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
012600000000     D JDBC_freeProp   PR
012700000000     D   prop                              like(Properties)
012800000000
012900000000
013000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
013100000000      *  JDBC_ExecUpd():  Execute a Query that doesn't return results
013200000000      *
013300000000      *     conn = (input) database connection
013400000000      *      sql = (input) SQL Code to execute
013500000000      *
013600000000      *  returns a row count, or 0 where a row count is not applicable
013700000000      *        or -1 upon failure
013800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
013900000000     D JDBC_ExecUpd    PR            10I 0
014000000000     D   conn                              like(Connection) const
014100000000     D   sql                      32767A   varying const options(*varsize)
014200000000
014300000000
014400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
014500000000      *  JDBC_ExecQry():  Execute a Query that returns a result
014600000000      *
014700000000      *     conn = (input) database connection
014800000000      *      sql = (input) SQL code to execute
014900000000      *
015000000000      *  Returns a result handle, or *NULL upon failure
015100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
015200000000     D JDBC_ExecQry    PR                  like(ResultSet)
015300000000     D   conn                              like(Connection) const
015400000000     D   sql                      32767A   varying const options(*varsize)
015500000000
015600000000
015700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
015800000000      *  JDBC_GetMetaData():  Get ResultSet MetaData
015900000000      *
016000000000      *       rs = (input) Result handle to get metadata for
016100000000      *
016200000000      *  Returns a resultset metadata, or *NULL upon failure
016300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
016400000000     D JDBC_GetMetaData...
016500000000     D                 PR                  like(ResultSetMetaData)
016600000000     D   rs                                like(ResultSet)
016700000000
016800000000
016900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
017000000000      *  JDBC_GetColCount():  Get ResultSet Column Count
017100000000      *
017200000000      *       rsmd = (input) ResultSet MetaData handle
017300000000      *
017400000000      *  Returns column count
017500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
017600000000     D JDBC_GetColCount...
017700000000     D                 PR            10I 0
017800000000     D   rsmd                              like(ResultSetMetaData)
017900000000
018000000000
018100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
018200000000      *  JDBC_GetColName():  Get column name from ResultSet MetaData
018300000000      *
018400000000      *     rsmd = (input) ResultSet MetaData handle to use
018500000000      *      col = (input) column number to retrieve
018600000000      *
018700000000      *  Returns column name if successful, or '' otherwise
018800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
018900000000     D JDBC_GetColName...
019000000000     D                 PR           256A   varying
019100000000     D   rsmd                              like(ResultSetMetaData)
019200000000     D   col                         10I 0 value
019300000000
019400000000
019500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
019600000000      *  JDBC_GetColDspSize():  Get column display size from ResultSet MetaData
019700000000      *
019800000000      *     rsmd = (input) ResultSet MetaData handle to use
019900000000      *      col = (input) column number to retrieve
020000000000      *
020100000000      *  Returns column display size
020200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
020300000000     D JDBC_GetColDspSize...
020400000000     D                 PR            10I 0
020500000000     D   rsmd                              like(ResultSetMetaData)
020600000000     D   col                         10I 0 value
020700000000
020800000000
020900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
021000000000      *  JDBC_GetColTypName():  Get column SQL type name from ResultSet MetaData
021100000000      *
021200000000      *     rsmd = (input) ResultSet MetaData handle to use
021300000000      *      col = (input) column number to retrieve
021400000000      *
021500000000      *  Returns column SQL type name
021600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
021700000000     D JDBC_GetColTypName...
021800000000     D                 PR           256A   varying
021900000000     D   rsmd                              like(ResultSetMetaData)
022000000000     D   col                         10I 0 value
022100000000
022200000000
022300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
022400000000      *  JDBC_ NextRow():  Position result to next row
022500000000      *
022600000000      *       rs = (input) Result handle to move
022700000000      *
022800000000      *  Returns *ON if successful, *OFF otherwise
022900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
023000000000     D JDBC_NextRow    PR             1N
023100000000     D   rs                                like(ResultSet)
023200000000
023300000000
023400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
023500000000      *  JDBC_GetCol():  Get column from current row
023600000000      *
023700000000      *       rs = (input) Result handle to use
023800000000      *      col = (input) column number to retrieve
023900000000      *  nullInd = (output/optional) will be set to *ON if the
024000000000      *             field is null, or *OFF otherwise.
024100000000      *
024200000000      *  Returns column value if successful, or '' otherwise
024300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
024400000000     D JDBC_GetCol     PR         32767A   varying
024500000000     D   rs                                like(ResultSet)
024600000000     D   col                         10I 0 value
024700000000     D   nullInd                      1N   options(*nopass:*omit)
024800000000
024900000000
025000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
025100000000      *  JDBC_FreeResult(): Free result handle
025200000000      *
025300000000      *     rs = (input) Result handle to free
025400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
025500000000     D JDBC_FreeResult...
025600000000     D                 PR
025700000000     D   rs                                like(ResultSet)
025800000000
025900000000
026000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
026100000000      *  JDBC_Commit():  Commit transaction
026200000000      *
026300000000      *     conn = (input) Connection to commit on
026400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
026500000000     D JDBC_Commit     PR
026600000000     D   conn                              like(Connection)
026700000000
026800000000
026900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
027000000000      *  JDBC_Rollback():  Rollback transaction
027100000000      *
027200000000      *     conn = (input) Connection to rollback on
027300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
027400000000     D JDBC_Rollback   PR
027500000000     D   conn                              like(Connection)
027600000000
027700000000
027800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
027900000000      *  JDBC_Close():  Close connection to Server
028000000000      *                   and free connection handle
028100000000      *
028200000000      *     conn = (input) Connection to close
028300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
028400000000     D JDBC_Close      PR
028500000000     D   conn                              like(Connection)
028600000000
028700000000
028800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
028900000000      * JDBC_PrepStmt(): Create a Prepared Statement
029000000000      *
029100000000      *    conn = (input) Connection to prepare statement for
029200000000      *     sql = (input) SQL statement to prepare
029300000000      *
029400000000      * Returns a prepared statement object, or *NULL upon error
029500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
029600000000     D JDBC_PrepStmt   PR                  like(PreparedStatement)
029700000000     D   conn                              like(Connection) const
029800000000     D   sql                      32767A   varying const options(*varsize)
029900000000
030000000000
030100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
030200000000      * JDBC_PrepCall(): Create a Callable Statement
030300000000      *
030400000000      *    conn = (input) Connection to prepare call statement for
030500000000      *     sql = (input) SQL call statement to prepare
030600000000      *
030700000000      * Returns a callable statement object, or *NULL upon error
030800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
030900000000     D JDBC_PrepCall   PR                  like(CallableStatement)
031000000000     D   conn                              like(Connection) const
031100000000     D   sql                      32767A   varying const options(*varsize)
031200000000
031300000000
031400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
031500000000      * JDBC_SetString(): Set a string in a prepared statement
031600000000      *
031700000000      *    prep = (input) Prepared statement to set string in
031800000000      *     idx = (input) Parameter index to set
031900000000      *     str = (input) String to set
032000000000      *    null = (input/optional) set field to NULL in database
032100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
032200000000     D JDBC_SetString...
032300000000     D                 PR             1N
032400000000     D   prep                              like(PreparedStatement) const
032500000000     D   idx                         10I 0 value
032600000000     D   str                      32767A   varying const options(*varsize)
032700000000     D   null                         1N   const options(*nopass)
032800000000
032900000000
033000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
033100000000      * JDBC_SetInt(): Set an integer in a prepared statement
033200000000      *
033300000000      *    prep = (input) Prepared statement to set value in
033400000000      *     idx = (input) Parameter index to set
033500000000      *     int = (input) value to set
033600000000      *    null = (input/optional) set field to NULL in database
033700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
033800000000     D JDBC_SetInt     PR             1N
033900000000     D   prep                              like(PreparedStatement) const
034000000000     D   idx                         10I 0 value
034100000000     D   int                         10I 0 value
034200000000     D   null                         1N   const options(*nopass)
034300000000
034400000000
034500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
034600000000      * JDBC_SetDouble(): Set a floating point value in a prepared
034700000000      *                    statement
034800000000      *
034900000000      *    prep = (input) Prepared statement to set value in
035000000000      *     idx = (input) Parameter index to set
035100000000      *  double = (input) value to set
035200000000      *    null = (input/optional) set field to NULL in database
035300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
035400000000     D JDBC_SetDouble...
035500000000     D                 PR             1N
035600000000     D   prep                              like(PreparedStatement) const
035700000000     D   idx                         10I 0 value
035800000000     D   double                       8F   value
035900000000     D   null                         1N   const options(*nopass)
036000000000
036100000000
036200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
036300000000      * JDBC_SetDecimal(): Set a decimal value in a prepared stmt
036400000000      *
036500000000      *    prep = (input) Prepared statement to set value in
036600000000      *     idx = (input) Parameter index to set
036700000000      *     dec = (input) value to set
036800000000      *    null = (input/optional) set field to NULL in database
036900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
037000000000     D JDBC_SetDecimal...
037100000000     D                 PR             1N
037200000000     D   prep                              like(PreparedStatement) const
037300000000     D   idx                         10I 0 value
037400000000     D   dec                         30P 9 value
037500000000     D   null                         1N   const options(*nopass)
037600000000
037700000000
037800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
037900000000      * JDBC_SetDate(): Set a date value in a prepared stmt
038000000000      *
038100000000      *    prep = (input) Prepared statement to set value in
038200000000      *     idx = (input) Parameter index to set
038300000000      *    date = (input) value to set
038400000000      *    null = (input/optional) set field to NULL in database
038500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
038600000000     D JDBC_SetDate...
038700000000     D                 PR             1N
038800000000     D   prep                              like(PreparedStatement) const
038900000000     D   idx                         10I 0 value
039000000000     D   date                          D   datfmt(*iso) const
039100000000     D   null                         1N   const options(*nopass)
039200000000
039300000000
039400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
039500000000      * JDBC_SetTime(): Set a time value in a prepared stmt
039600000000      *
039700000000      *    prep = (input) Prepared statement to set value in
039800000000      *     idx = (input) Parameter index to set
039900000000      *    time = (input) value to set
040000000000      *    null = (input/optional) set field to NULL in database
040100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
040200000000     D JDBC_SetTime...
040300000000     D                 PR             1N
040400000000     D   prep                              like(PreparedStatement) const
040500000000     D   idx                         10I 0 value
040600000000     D   time                          T   timfmt(*hms) const
040700000000     D   null                         1N   const options(*nopass)
040800000000
040900000000
041000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
041100000000      * JDBC_SetTimestamp(): Set a timestamp value in a prepared stmt
041200000000      *
041300000000      *    prep = (input) Prepared statement to set value in
041400000000      *     idx = (input) Parameter index to set
041500000000      *      ts = (input) value to set
041600000000      *    null = (input/optional) set field to NULL in database
041700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
041800000000     D JDBC_SetTimestamp...
041900000000     D                 PR             1N
042000000000     D   prep                              like(PreparedStatement) const
042100000000     D   idx                         10I 0 value
042200000000     D   ts                            Z   const
042300000000     D   null                         1N   const options(*nopass)
042400000000
042500000000
042600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
042700000000      * JDBC_RegisterOutParameter(): Register an output parameter
042800000000      *                           returned from a stored-proc call
042900000000      *
043000000000      *    call = (input) Callable statement to register output for
043100000000      *     idx = (input) Parameter index to register
043200000000      *    type = (input) Data type to register
043300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
043400000000     D JDBC_RegisterOutParameter...
043500000000     D                 PR
043600000000     D   call                              like(CallableStatement) const
043700000000     D   idx                         10I 0 value
043800000000     D   type                        10I 0 value
043900000000
044000000000
044100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
044200000000      *  JDBC_GetString():  Get string from Callable Statement
044300000000      *
044400000000      *     call = (input) Callable Statement handle to use
044500000000      *      idx = (input) parameter index to retrieve
044600000000      *
044700000000      *  Returns string value if successful, or '' otherwise
044800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
044900000000     D JDBC_GetString...
045000000000     D                 PR         32767A   varying
045100000000     D   call                              like(CallableStatement) const
045200000000     D   idx                         10I 0 value
045300000000
045400000000
045500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
045600000000      *  JDBC_GetInt():  Get integer from Callable Statement
045700000000      *
045800000000      *     call = (input) Callable Statement handle to use
045900000000      *      idx = (input) parameter index to retrieve
046000000000      *
046100000000      *  Returns int value if successful, or 0 otherwise
046200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
046300000000     D JDBC_GetInt...
046400000000     D                 PR            10I 0
046500000000     D   call                              like(CallableStatement) const
046600000000     D   idx                         10I 0 value
046700000000
046800000000
046900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
047000000000      *  JDBC_GetShort():  Get short from Callable Statement
047100000000      *
047200000000      *     call = (input) Callable Statement handle to use
047300000000      *      idx = (input) parameter index to retrieve
047400000000      *
047500000000      *  Returns int value if successful, or 0 otherwise
047600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
047700000000     D JDBC_GetShort...
047800000000     D                 PR            10I 0
047900000000     D   call                              like(CallableStatement) const
048000000000     D   idx                         10I 0 value
048100000000
048200000000
048300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
048400000000      *  JDBC_GetBoolean():  Get boolean from Callable Statement
048500000000      *
048600000000      *     call = (input) Callable Statement handle to use
048700000000      *      idx = (input) parameter index to retrieve
048800000000      *
048900000000      *  Returns a boolean value if successful, or *OFF otherwise
049000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
049100000000     D JDBC_GetBoolean...
049200000000     D                 PR             1N
049300000000     D   call                              like(CallableStatement) const
049400000000     D   idx                         10I 0 value
049500000000
049600000000
049700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
049800000000      * JDBC_ExecPrepQry(): Execute a query from a prepared statement
049900000000      *
050000000000      *      prep = (input) prepared statement to execute
050100000000      *
050200000000      * Returns ResultSet object or *NULL upon failure
050300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
050400000000     D JDBC_ExecPrepQry...
050500000000     D                 PR                  like(ResultSet)
050600000000     D   prep                              like(PreparedStatement) const
050700000000
050800000000
050900000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
051000000000      * JDBC_ExecPrepUpd(): Execute SQL that doesn't return data
051100000000      *                     from a prepared statement
051200000000      *
051300000000      *      prep = (input) prepared statement to execute
051400000000      *
051500000000      * Returns 0 or a row count if successful
051600000000      *     or -1 upon failure
051700000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
051800000000     D JDBC_ExecPrepUpd...
051900000000     D                 PR            10I 0
052000000000     D   prep                              like(PreparedStatement) const
052100000000
052200000000
052300000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
052400000000      * JDBC_ExecCall(): Execute SQL statement that calls a stored
052500000000      *                     procedure
052600000000      *
052700000000      *      call = (input) callable statement to execute
052800000000      *
052900000000      * Returns *ON if first result is a ResultSet
053000000000      *      or *OFF if first result is an update count
053100000000      *
053200000000      * Use JDBC_moreResults(), JDBC_getUpdateCount() and
053300000000      *     JDBC_getResultSet() to get results of this function
053400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
053500000000     D JDBC_ExecCall...
053600000000     D                 PR             1N
053700000000     D   call                              like(CallableStatement) const
053800000000
053900000000
054000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
054100000000      * JDBC_FreePrepStmt(): Free prepared statement
054200000000      *
054300000000      *    prep = (input) Prepared Statement to Free
054400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
054500000000     D JDBC_FreePrepStmt...
054600000000     D                 PR
054700000000     D   prep                              like(PreparedStatement)
054800000000
054900000000
055000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
055100000000      * JDBC_FreeCallStmt(): Free callable statement
055200000000      *
055300000000      *    call = (input) Callable Statement to Free
055400000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
055500000000     D JDBC_FreeCallStmt...
055600000000     D                 PR
055700000000     D   call                              like(CallableStatement)
055800000000
055900000000
056000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
056100000000      *  JDBC_GetColByName():  Get column from current row by
056200000000      *                        column name
056300000000      *
056400000000      *          rs = (input) Result handle to use
056500000000      *  columnName = (input) column name to retrieve
056600000000      *     nullInd = (output/optional) will be set to *ON if the
056700000000      *               field is null, or *OFF otherwise.
056800000000      *
056900000000      *  Returns column value if successful, or '' otherwise
057000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
057100000000     D JDBC_GetColByName...
057200000000     D                 PR         32767a   varying
057300000000     D  rs                                 Like(ResultSet)
057400000000     D  ColumnName                32767a   varying Const options(*varsize)
057500000000     D  nullInd                       1N   options(*nopass:*omit)
057600000000
057700000000
057800000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
057900000000      *  JDBC_getUpdateCount(): Get the number of rows modified
058000000000      *         by the last call to JDBC_ExecCall()
058100000000      *
058200000000      *        stmt = (input) PreparedStatement or CallableStatement
058300000000      *
058400000000      *  Returns number of modified rows, or -1 if the current
058500000000      *          result is a ResultSet or if there are no results
058600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
058700000000     D JDBC_getUpdateCount...
058800000000     D                 pr            10i 0
058900000000     D  stmt                               Like(Statement) const
059000000000
059100000000
059200000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
059300000000      *  JDBC_getResultSet(): Get the first/next ResultSet
059400000000      *         returned by JDBC_ExecCall()
059500000000      *
059600000000      *        stmt = (input) PreparedStatement or CallableStatement
059700000000      *
059800000000      *  Returns the ResultSet or *NULL if the current result is
059900000000      *          not a ResultSet or if there are no more results
060000000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
060100000000     D JDBC_getResultSet...
060200000000     D                 pr                  like(ResultSet)
060300000000     D  stmt                               Like(Statement) const
060400000000
060500000000
060600000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
060700000000      *  JDBC_getMoreResults(): Move the results of this statement
060800000000      *         to the next result returned.
060900000000      *
061000000000      *        stmt = (input) PreparedStatement or CallableStatement
061100000000      *
061200000000      *  Returns *ON if the next result is a ResultSet object,
061300000000      *          or *OFF if it's an update count or there are no
061400000000      *          more results
061500000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
061600000000     D JDBC_getMoreResults...
061700000000     D                 pr             1n
061800000000     D  stmt                               Like(Statement) const
061900000000
062000000000
062100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
062200000000      * JDBC_Execute(): Execute Prepared SQL Statement
062300000000      *
062400000000      *      stmt = (input) prepared statement to execute
062500000000      *
062600000000      * Returns *ON if first result is a ResultSet
062700000000      *      or *OFF if first result is an update count
062800000000      *
062900000000      * Use JDBC_moreResults(), JDBC_getUpdateCount() and
063000000000      *     JDBC_getResultSet() to get results of this function
063100000000      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
063200000000     D JDBC_Execute...
063300000000     D                 PR             1N
063400000000     D   stmt                              like(PreparedStatement) const
