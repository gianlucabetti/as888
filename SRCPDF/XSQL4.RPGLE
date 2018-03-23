000001000000     H COPYRIGHT('Giuseppe Costagliola 1996, 2013 - beppecosta@yahoo.it')
000002000000     H DEBUG DFTACTGRP(*NO) ACTGRP('QILE')
000003000000
000004000000      *----------------------------------------------------------------
000005000000      ** entry parameter
000006000000      *----------------------------------------------------------------
000007000000     D psqlstmt        s           5000
000008000000     D logsqlstmt      s              4
000009000000     D sqlsysval       s              4
000010000000     D escapeerr       s              4
000011000000
000012000000      *----------------------------------------------------------------
000013000000      ** sql communication area
000014000000      *----------------------------------------------------------------
000015000000     D sqlca           ds
000016000000     D  sqlcaid                       8
000017000000     D  sqlcabc                       9b 0
000018000000     D  sqlcode                       9b 0
000019000000     D  sqlerrml                      4b 0
000020000000     D  sqlerrmc                     70
000021000000     D  sqlerrp                       8
000022000000     D  sqlerrd                      24
000023000000     D   sqler1                       9b 0 overlay(sqlerrd:1)
000024000000     D   sqler2                       9b 0 overlay(sqlerrd:5)
000025000000     D   sqler3                       9b 0 overlay(sqlerrd:9)
000026000000     D   sqler4                       9b 0 overlay(sqlerrd:13)
000027000000     D   sqler5                       9b 0 overlay(sqlerrd:17)
000028000000     D   sqler6                       9b 0 overlay(sqlerrd:21)
000029000000     D  sqlwarn                      11
000030000000     D  sqlstate                      5
000031000000
000032000000      *----------------------------------------------------------------
000033000000      ** sql descriptor area
000034000000      *----------------------------------------------------------------
000035000000     D sqlda           ds           200
000036000000     D  sqldaid                       8    inz('SQLDA')
000037000000     D  sqldabc                       9b 0 inz(150)
000038000000     D  sqln                          4b 0 inz(0)
000039000000     D  sqld                          4b 0 inz(0)
000040000000
000041000000      *----------------------------------------------------------------
000042000000      ** api qsqprced
000043000000      *----------------------------------------------------------------
000044000000     D sqlp0100        ds
000045000000     D  §function                     1    inz('0')
000046000000     D  §pkgname                     10    inz('SQLPACK')
000047000000     D  §pkglib                      10    inz('QTEMP')
000048000000     D  §mainpgm                     10    inz('XSQL4')
000049000000     D  §mainlib                     10    inz('*LIBL')
000050000000     D  §stmname                     18    inz('STMTNAME')
000051000000     D  §curname                     18    inz('CURSOR')
000052000000     D  §openopt                      1    inz(x'00')
000053000000     D  §claudesc                     1    inz('N')
000054000000     D  §commit                       1    inz('N')
000055000000     D  §datefmt                      3    inz('DMY')
000056000000     D  §datesep                      1    inz('/')
000057000000     D  §timefmt                      3    inz('HMS')
000058000000     D  §timesep                      1    inz(':')
000059000000     D  §namingopt                    3    inz('SYS')
000060000000     D  §decpos                       1    inz('.')
000061000000     D  §block                        4b 0 inz(0)
000062000000     D  §sqlstmtl                     4b 0 inz(0)
000063000000     D  §sqlstmt                      1    dim(5000)
000064000000
000065000000     D sqformat        ds
000066000000     D  §format                      10    inz('SQLP0100')
000067000000
000068000000      *----------------------------------------------------------------
000069000000      ** api job informations
000070000000      *----------------------------------------------------------------
000071000000     D jobi0100        ds            70
000072000000     D  jobi§bytes                    9b 0 inz(61)
000073000000     D  joby§avail                    9b 0
000074000000     D  jobi§jobn                    26    inz('*')
000075000000     D  jobi§jobi                    16    inz
000076000000     D  jobi§form                    10    inz('JOBI0100')
000077000000     D  jobi§type                     1
000078000000
000079000000      *----------------------------------------------------------------
000080000000      ** api object desctiption informations
000081000000      *----------------------------------------------------------------
000082000000     D objd0100        ds
000083000000     D  objd§bytes                    9b 0 inz(48)
000084000000     D  objd§avail                    9b 0
000085000000     D  objd§form                    10    inz('OBJD0100')
000086000000     D  objd§type                    10
000087000000     D                               10
000088000000     D  objd§rlib                    10
000089000000
000090000000      *----------------------------------------------------------------
000091000000      ** api retrieve system values
000092000000      *----------------------------------------------------------------
000093000000     D qsva§rcv£       s              9b 0 inz(120)
000094000000     D qsva§sys£       s              9b 0 inz(4)
000095000000
000096000000     D qsva§sysv       ds
000097000000     D  §datfmt                      10    inz('QDATFMT')
000098000000     D  §datsep                      10    inz('QDATSEP')
000099000000     D  §timsep                      10    inz('QTIMSEP')
000100000000     D  §decfmt                      10    inz('QDECFMT')
000101000000
000102000000     D qsva§rcv        ds
000103000000     D  §rtnval                       9b 0
000104000000     D  §sysoff                       9b 0 dim(4)
000105000000     D  §systable                   160
000106000000
000107000000     D qsystable       ds
000108000000     D  sysvaltab                    24    dim(4)
000109000000     D  sysvalname                   10    overlay(sysvaltab:1)
000110000000     D  sysvalue                      7    overlay(sysvaltab:17)
000111000000
000112000000      *----------------------------------------------------------------
000113000000      ** api send/receive program messages
000114000000      *----------------------------------------------------------------
000115000000     D §sndpgmmsg      ds
000116000000     D  §msgid                        7    inz('CPF9898')
000117000000     D  §msgfile                     20    inz('QCPFMSG   QSYS      ')
000118000000     D  §msgdata£                     9b 0 inz(512)
000119000000     D  §msgtype                     10    inz('*COMP     ')
000120000000     D  §msgmsgq                     11    inz('*       ')
000121000000     D  §msgstack                     9b 0 inz(1)
000122000000     D  §msgkey                       4
000123000000
000124000000     D §rcvpgmmsg      ds
000125000000     D  §msgbytes                     9b 0 inz(8)
000126000000     D  §msgformat                    8    inz('RCVM0100')
000127000000     D  §msgwait                      9b 0 inz(0)
000128000000     D  §msgaction                   10    inz('*OLD      ')
000129000000     D  §msgtyper                    10    inz('*ANY      ')
000130000000
000131000000      *----------------------------------------------------------------
000132000000      ** api error structure
000133000000      *----------------------------------------------------------------
000134000000     D §apierror       ds
000135000000     D  §rr1                          9b 0 inz(15)
000136000000     D  §rr2                          9b 0 inz(0)
000137000000     D  §rrmsgid                      7
000138000000
000139000000      *----------------------------------------------------------------
000140000000      ** miscellaneous fields
000141000000      *----------------------------------------------------------------
000142000000     D §msgdata        s            512
000143000000     D §rcv0100        s              8
000144000000     D dlyjob          s             10    inz('DLYJOB 1')
000145000000     D cmd£            s             15  5 inz(8)
000146000000     D psql£           s              5  0
000147000000     D j               s              5  0
000148000000     D jo              s              5  0
000149000000
000150000000      *----------------------------------------------------------------
000151000000      ** program status area
000152000000      *----------------------------------------------------------------
000153000000     D §parms         sds
000154000000     D  parms£           *parms
000155000000     D  pgmnam           *proc
000156000000
000157000000      *****************************************************************
000158000000      ** api plist
000159000000      *****************************************************************
000160000000
000161000000     C*----------------------------------------------------------------
000162000000     C* process extended dynamic sql
000163000000     C     psqprced      plist
000164000000     C                   parm                    sqlca
000165000000     C                   parm                    sqlda
000166000000     C                   parm                    sqformat
000167000000     C                   parm                    sqlp0100
000168000000     C                   parm                    §apierror
000169000000
000170000000     C*----------------------------------------------------------------
000171000000     C* retrieve program mainlib
000172000000     C     prtvpgmd      plist
000173000000     C                   parm                    objd0100
000174000000     C                   parm                    objd§bytes
000175000000     C                   parm                    objd§form
000176000000     C                   parm                    §mainpgm
000177000000     C                   parm                    objd§type
000178000000     C                   parm                    §apierror
000179000000
000180000000     C*----------------------------------------------------------------
000181000000     C* retrieve object description
000182000000     C     prtvobjd      plist
000183000000     C                   parm                    objd0100
000184000000     C                   parm                    objd§bytes
000185000000     C                   parm                    objd§form
000186000000     C                   parm                    §pkgname
000187000000     C                   parm                    objd§type
000188000000     C                   parm                    §apierror
000189000000
000190000000     C*----------------------------------------------------------------
000191000000     C* retrieve job information
000192000000     C     prtvjobi      plist
000193000000     C                   parm                    jobi0100
000194000000     C                   parm                    jobi§bytes
000195000000     C                   parm                    jobi§form
000196000000     C                   parm                    jobi§jobn
000197000000     C                   parm                    jobi§jobi
000198000000     C                   parm                    §apierror
000199000000
000200000000     C*----------------------------------------------------------------
000201000000     C* retrieve system values
000202000000     C     pwcrsval      plist
000203000000     C                   parm                    qsva§rcv
000204000000     C                   parm                    qsva§rcv£
000205000000     C                   parm                    qsva§sys£
000206000000     C                   parm                    qsva§sysv
000207000000     C                   parm                    §apierror
000208000000
000209000000     C*----------------------------------------------------------------
000210000000     C* send program message - qmhsndpm
000211000000     C     psndmsg       plist
000212000000     C                   parm                    §msgid
000213000000     C                   parm                    §msgfile
000214000000     C                   parm                    §msgdata
000215000000     C                   parm                    §msgdata£
000216000000     C                   parm                    §msgtype
000217000000     C                   parm                    §msgmsgq
000218000000     C                   parm                    §msgstack
000219000000     C                   parm                    §msgkey
000220000000     C                   parm                    §apierror
000221000000
000222000000     C*----------------------------------------------------------------
000223000000     C* receive program message - qmhrcvpm
000224000000     C     prcvmsg       plist
000225000000     C                   parm                    §rcv0100
000226000000     C                   parm                    §msgbytes
000227000000     C                   parm                    §msgformat
000228000000     C                   parm                    §msgmsgq
000229000000     C                   parm                    §msgstack
000230000000     C                   parm                    §msgtyper
000231000000     C                   parm                    §msgkey
000232000000     C                   parm                    §msgwait
000233000000     C                   parm                    §msgaction
000234000000     C                   parm                    §apierror
000235000000
000236000000      *****************************************************************
000237000000      ** main
000238000000      *****************************************************************
000239000000
000240000000     C     *entry        plist
000241000000     C                   parm                    psqlstmt
000242000000     C                   parm                    logsqlstmt
000243000000     C                   parm                    sqlsysval
000244000000     C                   parm                    escapeerr
000245000000
000246000000      *----------------------------------------------------------------
000247000000      ** reformat the sql statement
000248000000      *----------------------------------------------------------------
000249000000
000250000000     C                   movea     psqlstmt      §sqlstmt
000251000000     C     ' '           checkr    psqlstmt      psql£
000252000000     C                   do        psql£         j
000253000000     C                   if        §sqlstmt(j) = '"'
000254000000     C                   move      ''''          §sqlstmt(j)
000255000000     C                   endif
000256000000     C                   if        §sqlstmt(j) = '['
000257000000     C                   move      '"'           §sqlstmt(j)
000258000000     C                   endif
000259000000     C                   if        §sqlstmt(j) = ']'
000260000000     C                   move      '"'           §sqlstmt(j)
000261000000     C                   endif
000262000000     C                   enddo
000263000000
000264000000      *----------------------------------------------------------------
000265000000      ** display the sql statement being run
000266000000      *----------------------------------------------------------------
000267000000
000268000000     C                   if        parms£ >= 2 and logsqlstmt = '*YES'
000269000000     C                   movea     §sqlstmt      psqlstmt
000270000000     C     'sql=>'       cat       psqlstmt:0    §msgdata
000271000000
000272000000      *----------------------------------------------------------------
000273000000      * if job interactive display to ext
000274000000     C                   call      'QUSRJOBI'    prtvjobi
000275000000
000276000000     C                   if        jobi§type = 'I'
000277000000     C                   movel(p)  '*STATUS'     §msgtype
000278000000     C                   movel(p)  '*EXT'        §msgmsgq
000279000000     C                   call      'QMHSNDPM'    psndmsg
000280000000     C                   reset                   §sndpgmmsg
000281000000     C                   endif
000282000000
000283000000      *----------------------------------------------------------------
000284000000      * send to joblog
000285000000     C                   z-add     0             §msgstack
000286000000     C                   call      'QMHSNDPM'    psndmsg
000287000000
000288000000      *----------------------------------------------------------------
000289000000      * flag the message as received
000290000000     C                   call      'QMHRCVPM'    prcvmsg
000291000000     C                   endif
000292000000
000293000000      *----------------------------------------------------------------
000294000000      ** create the *sqlpackage if not done yet
000295000000      *----------------------------------------------------------------
000296000000
000297000000     C                   move      *blanks       §rrmsgid
000298000000     C                   movel     pgmnam        §mainpgm
000299000000     C                   movel(p)  '*PGM'        objd§type
000300000000     C                   call      'QUSROBJD'    prtvpgmd
000301000000     C                   movel     objd§rlib     §mainlib
000302000000
000303000000     C                   move      *blanks       §rrmsgid
000304000000     C                   movel     pgmnam        §pkgname
000305000000     C                   movel(p)  'OBJD0100'    objd§form
000306000000     C                   movel(p)  '*SQLPKG'     objd§type
000307000000     C                   call      'QUSROBJD'    prtvobjd
000308000000
000309000000     C                   if        §rrmsgid = 'CPF9801'
000310000000
000311000000      *----------------------------------------------------------------
000312000000     C* set the sql template
000313000000     C                   if        parms£ = 3 and sqlsysval = '*YES'
000314000000     C                   exsr      sq100setup
000315000000     C                   endif
000316000000
000317000000     C                   movel     '1'           §function
000318000000     C                   call      'QSQPRCED'    psqprced
000319000000
000320000000     C                   endif
000321000000
000322000000      *----------------------------------------------------------------
000323000000      ** prepare and describe the sql statement
000324000000      *----------------------------------------------------------------
000325000000
000326000000     C                   movel     '9'           §function
000327000000     C                   z-add     psql£         §sqlstmtl
000328000000     C                   call      'QSQPRCED'    psqprced
000329000000
000330000000      *----------------------------------------------------------------
000331000000      ** select statement not allowed
000332000000     C                   if        sqld > 0
000333000000     C                   z-add     0084          sqlcode
000334000000     C                   endif
000335000000
000336000000      *----------------------------------------------------------------
000337000000      ** if errors in prepare/describe, do not continue
000338000000     C                   if        sqlcode <> 0
000339000000     C                   if        jobi§type = 'I'
000340000000     C                   exsr      ckretcode
000341000000     C                   endif
000342000000     C                   if        escapeerr = '*ALL' or
000343000000     C                             (escapeerr = '*ERR' and
000344000000     C                              sqlcode < 0)
000345000000     C                   movel(p)  '*ESCAPE'     §msgtype
000346000000     C                   movel(p)  '*'           §msgmsgq
000347000000     C                   z-add     2             §msgstack
000348000000     C                   call      'QMHSNDPM'    psndmsg
000349000000     C                   endif
000350000000     C                   move      *on           *inlr
000351000000     C                   return
000352000000     C                   endif
000353000000
000354000000      *----------------------------------------------------------------
000355000000      ** execute the sql statement
000356000000      *----------------------------------------------------------------
000357000000
000358000000     C                   movel     '3'           §function
000359000000     C                   call      'QSQPRCED'    psqprced
000360000000
000361000000      *----------------------------------------------------------------
000362000000      ** reformat the sql return code
000363000000     C                   exsr      ckretcode
000364000000
000365000000      *----------------------------------------------------------------
000366000000      ** send the completion message into joblog
000367000000     C                   z-add     1             §msgstack
000368000000     C                   call      'QMHSNDPM'    psndmsg
000369000000
000370000000      *----------------------------------------------------------------
000371000000      ** return to caller
000372000000     C                   move      *on           *inlr
000373000000     C                   return
000374000000
000375000000      *****************************************************************
000376000000      ** check sql return codes
000377000000      *****************************************************************
000378000000
000379000000     C     ckretcode     begsr
000380000000
000381000000     C                   select
000382000000
000383000000      *----------------------------------------------------------------
000384000000     C* error condition
000385000000     C     sqlcode       whenlt    0
000386000000     C                   movel(p)  '*DIAG'       §msgtype
000387000000
000388000000      *----------------------------------------------------------------
000389000000     C* ... cpf message
000390000000     C                   if        sqler1 > 0
000391000000     C                   move      sqler1        §msgid
000392000000     C                   movel     'CPF'         §msgid
000393000000     C                   else
000394000000
000395000000      *----------------------------------------------------------------
000396000000     C* ... cpd message
000397000000     C                   if        sqler2 > 0
000398000000     C                   move      sqler2        §msgid
000399000000     C                   movel     'CPD'         §msgid
000400000000     C                   else
000401000000     C                   mllzo     1             sqlcode
000402000000     C                   move      sqlcode       §msgid
000403000000     C                   movel     'SQL'         §msgid
000404000000     C                   movel     'QSQLMSG '    §msgfile
000405000000     C                   endif
000406000000     C                   endif
000407000000
000408000000      *----------------------------------------------------------------
000409000000     C* successful execution with warnings
000410000000     C     sqlcode       whengt    0
000411000000     C                   move      sqlcode       §msgid
000412000000     C                   movel     'SQL'         §msgid
000413000000     C                   movel     'QSQLMSG '    §msgfile
000414000000
000415000000      *----------------------------------------------------------------
000416000000     C* successful execution
000417000000     C                   other
000418000000     C                   move      sqlerrd       §msgid
000419000000     C                   movel     'SQL'         §msgid
000420000000     C                   movel     'QSQLMSG '    §msgfile
000421000000
000422000000     C                   endsl
000423000000
000424000000      *----------------------------------------------------------------
000425000000     C* message text
000426000000     C                   if        sqlerrml > 0
000427000000     C                   movel     sqlerrmc      §msgdata
000428000000     C                   z-add     sqlerrml      §msgdata£
000429000000     C                   endif
000430000000
000431000000      *----------------------------------------------------------------
000432000000     C* if job is interactive, send completion message for 1 second
000433000000     C                   if        jobi§type = 'I'
000434000000     C                   movel(p)  '*STATUS'     §msgtype
000435000000     C                   movel(p)  '*EXT'        §msgmsgq
000436000000     C                   z-add     0             §msgstack
000437000000     C                   call      'QMHSNDPM'    psndmsg
000438000000     C                   call      'QCMDEXC'
000439000000     C                   parm                    dlyjob
000440000000     C                   parm                    cmd£
000441000000     C                   movel(p)  '*DIAG'       §msgtype
000442000000     C                   movel(p)  '*'           §msgmsgq
000443000000     C                   endif
000444000000
000445000000     C                   endsr
000446000000
000447000000      *****************************************************************
000448000000      ** setup the SQLP0100 structure with system values
000449000000      *****************************************************************
000450000000
000451000000     C     sq100setup    begsr
000452000000
000453000000     C                   call      'QWCRSVAL'    pwcrsval
000454000000
000455000000     C                   do        4             j
000456000000     C                   eval      jo = §sysoff(j) + 1
000457000000     C                   subst     qsva§rcv:jo   sysvaltab(j)
000458000000     C                   enddo
000459000000
000460000000     C                   movel     sysvalue(1)   §datefmt
000461000000     C                   movel     sysvalue(2)   §datesep
000462000000     C                   movel     sysvalue(3)   §timesep
000463000000     C                   if        %subst(sysvalue(4):1:1) <> ' '
000464000000     C                   move      ','           §decpos
000465000000     C                   endif
000466000000
000467000000     C                   endsr
