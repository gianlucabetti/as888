000100991105      **************************************************************************
000200060720      * Conversione SCS/AFP spoold file a STMF in IFS applicando *WSCST
000300060720      * (Occhio agli spool troppo grossi => MAX 16M)
000400991105      **************************************************************************
000500990907      * Printer file for debug output
000600990907     FQPRINT    O    F  132        PRINTER USROPN OFLIND(*INOV)
000700990907      *****************************************************************
000800990907      * Program version
000900000318     Dversion          C                   '0-03 00-03-18'
001000990905      * User space error code
001100990905     D/COPY QSYSINC/QRPGLESRC,QUSEC
001200991031      * Retreive error information .............................................
001300991031     Dgeterrinfo       PR           128
001400991104      * Convert numeric to char ................................................
001500991104     Dn2c              PR            12
001600991104     D numeric                       10I 0 VALUE
001700000312      * Qual to string .........................................................
001800000312     Dq2s              PR            21
001900000312     D qual                          20    VALUE
002000000312      * Send program message ...................................................
002100000312     Dsndpm            PR
002200000312     D msg_data                     256    VALUE
002300000312     D msg_t                          1    VALUE
002400000312      * Debug print out ........................................................
002500991104     Ddp               PR
002600991104     D instr                        132    VALUE
002700990908      * Host print transform API ...............................................
002800990908     Dhpt              PR                  EXTPROC('QwpzHostPrintTransform')
002900990908     D                                 *   VALUE
003000990908     D                                 *   VALUE
003100990908     D                                 *   VALUE
003200990908     D                                 *   VALUE
003300990908     D                                 *   VALUE
003400990908     D                                 *   VALUE
003500990908     D                                 *   VALUE
003600990908     D                                 *   VALUE
003700990908     D                                 *   VALUE
003800990908     D                                 *   VALUE
003900990908     D                                 *   VALUE
004000990908     D                                 *   VALUE
004100991031      * Option specific input/output information
004200991031     D/COPY QSYSINC/QRPGLESRC,QWPZ
004300991031      * variables for Host Print Transform API
004400990908     Dhptopt           S              9B 0
004500991031     Dhptosilen        S              9B 0 INZ(%LEN(QWPPTOSI))
004600990908     Dhptsplbuflen     S              9B 0
004700991031     Dhptosolen        S              9B 0 INZ(%LEN(QWPPTOSO))
004800990908     Dhptosolena       S              9B 0
004900991104     Dxbufspc_p        S               *
005000991104     Dhptxbuflen       S              9B 0 INZ(200000)
005100990908     Dhptxbuflena      S              9B 0
005200990726      * Size of user space
005300990726     Dspc_size         S              9B 0
005400991105      * Stream file APIs .......................................................
005500991031     Dunlink           PR             9B 0 EXTPROC('unlink')
005600991031     D                                 *   VALUE
005700991031     Dopen             PR            10I 0 EXTPROC('open')
005800991031     D                                 *   VALUE
005900991031     D                               10I 0 VALUE
006000991031     D                               10U 0 VALUE OPTIONS(*NOPASS)
006100991031     D                               10U 0 VALUE OPTIONS(*NOPASS)
006200991031     D O_CREAT         S             10I 0 INZ(8)
006300991031     D O_WRONLY        S             10I 0 INZ(2)
006400991031     D O_TRUNC         S             10I 0 INZ(64)
006500991031     D O_CODEPAGE      S             10I 0 INZ(8388608)
006600991031     D S_IRWXU         S             10I 0 INZ(448)
006700991031     D S_IROTH         S             10I 0 INZ(4)
006800990724     Dwrite            PR            10I 0 EXTPROC('write')
006900990724     D                               10I 0 VALUE
007000000000     D                                 *   VALUE
007100990724     D                               10I 0 VALUE
007200990724     Dclose            PR            10I 0 EXTPROC('close')
007300990724     D                               10I 0 VALUE
007400990724      *
007500990724     Dfd               S             10I 0
007600990724     Dbytesw           S             10I 0
007700990724     Dstmf             S            257
007800990907      * 'QSPOPNSP/QSPGETSP/QSPCLOSP' spool file APIs variables .................
007900990905     Dspl_hdl          S              9B 0
008000991031     Dspl_nbr_b        S              9B 0
008100991031     Dspl_bufnbr       S              9B 0
008200990907      * Get Spooled File Data ..................................................
008300990905     D/COPY QSYSINC/QRPGLESRC,QSPGETSP
008400991104     Dgen_hdr          S            128    BASED(splspc_p)
008500990906     Dbuf_inf          S             40    BASED(bufp)
008600990906     Dbuf_inf2         S             44    BASED(bufp2)
008700991104     Dsplbuf           S           5000    BASED(splbuf_p)
008800991104      * Misc ...................................................................
008900000312     Dmsg_data         S            256
009000991104     Dstart_time       S               Z
009100991104     Dend_time         S               Z
009200991104      * Debug printout string ..................................................
009300990907     Dpm               S            132
009400000000      /EJECT
009500000000      **************************************************************************
009600990724      * Main
009700000000      *****************************************************************
009800990907      * Initialize
009900990907     C                   EXSR      #INIT
010000990907      * Prepare user space
010100990907     C                   EXSR      #PPRUS
010200991104      * Retrieve spool file
010300991105     C                   EXSR      #RTVSP
010400991102      * Create stream file
010500991031     C                   EXSR      #OPEN
010600991102      * Perform HTP
010700991102      *   initialize HPT
010800991102     C                   EVAL      hptopt = 10
010900991102     C                   EXSR      #HPT
011000991102      *   process file
011100991102     C                   EVAL      hptopt = 20
011200991102     C                   EXSR      #HPT
011300990907      *   set pointer to first buffer
011400991104     C                   EVAL      bufp = splspc_p + QSPOFB
011500990907      *   loop thru buffer
011600990907     C                   DO        QSPBRTN01
011700990907      *     retrieve 'Buffer information'
011800990906     C                   EVAL      QSPSPFRB = buf_inf
011900990907      *     set pointer to 'offset to general information buffer'
012000991104     C                   EVAL      bufp2 = splspc_p + QSPOGI
012100990907      *     retrieve 'General data (information buffer)'
012200990906     C                   EVAL      QSPSPFRG = buf_inf2
012300990907      *     transform data
012400990907     C                   EVAL      hptopt = 30
012500990907     C                   EXSR      #HPT
012600060718      *     transform pages
012700060718     C                   EVAL      hptopt = 30
012800060718     C                   EXSR      #HPT
012900990907      *     increment pointer by 'length of all buffer information'
013000990906     C                   EVAL      bufp = bufp + QSPLBI
013100990906     C                   END
013200990907      *   end file
013300990905     C                   EVAL      hptopt = 40
013400990905     C                   EXSR      #HPT
013500990907      *   terminate HTP
013600990905     C                   EVAL      hptopt = 50
013700990905     C                   EXSR      #HPT
013800990907      *   Close stream file
013900990907     C                   EXSR      #CLOSE
014000990907      * End program
014100990907     C                   SETON                                        LR
014200990907     C                   RETURN
014300000000      /EJECT
014400000000      **************************************************************************
014500990724      * Subroutines
014600000000      *****************************************************************
014700990907      * Initialize
014800990907     C     #INIT         BEGSR
014900990907      *
015000991031     C     *ENTRY        PLIST
015100991031     C                   PARM                    spl_name         10        I
015200000312     C                   PARM                    stmf_path       128        I
015300991031     C                   PARM                    wscst            20        I
015400991031     C                   PARM                    spl_job          26        I
015500000312     C                   PARM                    spl_nbr           4 0      I
015600991031     C                   PARM                    stmf_replace      4        I
015700000317     C                   PARM                    debug             4        I
015800991104      * Remember start time
015900991104     C                   TIME                    start_time
016000991104      * Debug mode?
016100000317     C     debug         COMP      '*YES'                                 90
016200991031     C   90              OPEN      QPRINT
016300991031     C   90              CALLP     dp('Debug mode.  ' +
016400991031     C                                      %TRIM(%EDITC(*DATE : 'Y')))
016500991031     C   90              CALLP     dp('*** SPL2STMF version ' +
016600991031     C                                      %TRIM(version) + ' ***')
016700991031     C   90              CALLP     dp(' ')
016800991031     C   90              CALLP     dp('Input Parameters')
016900991031     C   90              CALLP     dp(' splf     : ' + spl_name)
017000000312     C   90              CALLP     dp(' stmf     : ' + stmf_path)
017100000312     C   90              CALLP     dp(' wscst    : ' + q2s(wscst))
017200991031     C   90              CALLP     dp(' spl_job  : ' + spl_job)
017300000312     C   90              CALLP     dp(' spl_nbr  : ' + n2c(spl_nbr))
017400991031     C   90              CALLP     dp(' replace  : ' + stmf_replace)
017500990907      * Set APIs not to raise an exception
017600990907      *   System API Programming Version 4 SC41-5800-00
017700990907      *   2.4.3.2 Receiving the Error Code without the Exception Data--Example
017800990907     C                   Z-ADD     16            QUSBPRV
017900990907      *
018000990907     C                   ENDSR
018100990907      *****************************************************************
018200991104      * Create user space to retrieve spool data
018300990907     C     #PPRUS        BEGSR
018400991104     C                   EVAL      splspc_name = 'QSPGETSP  QTEMP'
018500991104     C   90              CALLP     dp(' ')
018600000312     C   90              CALLP     dp('Create User Space ' + q2s(splspc_name))
018700991104      *
018800990726     C                   CALL      'QUSCRTUS'
018900991104     C                   PARM                    splspc_name      20        I
019000990726     C                   PARM      *BLANKS       spc_attr         10        I
019100990905     C                   PARM      1024          spc_size                   I
019200990726     C                   PARM      X'00'         spc_init          1        I
019300990726     C                   PARM      '*CHANGE'     spc_aut          10        I
019400991031     C                   PARM      'SPL2STMF'    spc_text         50        I
019500991104     C                   PARM      '*NO'         spc_replace      10        I
019600990726     C                   PARM                    QUSEC                      I/O
019700990726     C                   PARM      '*USER'       spc_domain       10        I
019800990726      *
019900991104     C                   IF        QUSBAVL > 0
020000991104     C                   IF        QUSEI =  'CPF9870'
020100000312     C   90              CALLP     dp(' User Space ' + %TRIMR(q2s(splspc_name))
020200000312     C                                + ' already exists.')
020300991104     C                   ELSE
020400991104     C                   EVAL      msg_data = 'API QUSCRTUS failed : ' + QUSEI
020500991104     C                   EXSR      #QUIT
020600991104     C                   END
020700991104     C                   ELSE
020800991104     C   90              CALLP     dp(' API QUSCRTUS successful.')
020900991104     C                   END
021000991104      *   Retrieve pointer to user space
021100990905     C                   CALL      'QUSPTRUS'
021200991104     C                   PARM                    splspc_name                I
021300991104     C                   PARM                    splspc_p                   O
021400990905     C                   PARM                    QUSEC                      I/O
021500990905      *
021600990905     C                   IF        QUSBAVL > 0
021700991104     C                   EVAL      msg_data = 'API QUSPTRUS failed : ' + QUSEI
021800991104     C                   EXSR      #QUIT
021900990905     C                   END
022000991104     C   90              CALLP     dp(' API QUSPTRUS successful.')
022100991104      * Create user space for translation
022200991104     C                   EVAL      xbufspc_name = 'QWPZHPT1  QTEMP'
022300991104     C   90              CALLP     dp(' ')
022400000312     C   90              CALLP     dp('Create User Space ' + q2s(xbufspc_name))
022500991104      *
022600991104     C                   CALL      'QUSCRTUS'
022700991104     C                   PARM                    xbufspc_name     20        I
022800991104     C                   PARM      *BLANKS       spc_attr                   I
022900991104     C                   PARM      200000        spc_size                   I
023000991104     C                   PARM      X'00'         spc_init                   I
023100991104     C                   PARM      '*CHANGE'     spc_aut                    I
023200991104     C                   PARM      'SPL2STMF'    spc_text                   I
023300991104     C                   PARM      '*NO'         spc_replace                I
023400991104     C                   PARM                    QUSEC                      I/O
023500991104     C                   PARM      '*USER'       spc_domain                 I
023600991104      *
023700991104     C                   IF        QUSBAVL > 0
023800991104     C                   IF        QUSEI =  'CPF9870'
023900000312     C   90              CALLP     dp(' User Space ' + %TRIMR(q2s(xbufspc_name))
024000000312     C                                + ' already exists.')
024100991104     C                   ELSE
024200991104     C                   EVAL      msg_data = 'API QUSCRTUS failed : ' + QUSEI
024300991104     C                   EXSR      #QUIT
024400991104     C                   END
024500991104     C                   ELSE
024600991104     C   90              CALLP     dp(' API QUSCRTUS successful.')
024700991104     C                   END
024800991104      *   Retrieve pointer to user space
024900991104     C                   CALL      'QUSPTRUS'
025000991104     C                   PARM                    xbufspc_name               I
025100991104     C                   PARM                    xbufspc_p                  O
025200991104     C                   PARM                    QUSEC                      I/O
025300991104      *
025400991104     C                   IF        QUSBAVL > 0
025500991104     C                   EVAL      msg_data = 'API QUSPTRUS failed : ' + QUSEI
025600991104     C                   EXSR      #QUIT
025700991104     C                   END
025800991104     C   90              CALLP     dp(' API QUSPTRUS successful.')
025900991104      *
026000991104     C                   ENDSR
026100990905      *****************************************************************
026200991104      * Retrieve spool file
026300991105     C     #RTVSP        BEGSR
026400991104      *
026500991104     C   90              CALLP     dp(' ')
026600991104     C   90              CALLP     dp('Retrieve spool file.')
026700991104      * Open spool file
026800990905     C                   CALL      'QSPOPNSP'
026900990905     C                   PARM                    spl_hdl                    O
027000990905     C                   PARM                    spl_job          26        I
027100991031     C                   PARM      *BLANKS       spl_ijobi        16        I
027200991031     C                   PARM      *BLANKS       spl_ispli        16        I
027300991031     C                   PARM                    spl_name                   I
027400991031     C                   PARM      spl_nbr       spl_nbr_b                  I
027500991031     C                   PARM      -1            spl_bufnbr                 I
027600990905     C                   PARM                    QUSEC                      I/O
027700990905      *
027800990905     C                   IF        QUSBAVL > 0
027900991104     C                   EVAL      msg_data = 'API QSPOPNSP failed : ' + QUSEI
028000991104     C                   EXSR      #QUIT
028100990905     C                   END
028200991104     C   90              CALLP     dp(' API QSPOPNSP successful. spl_hdl = ' +
028300991104     C                                                          n2c(spl_hdl))
028400990905      * Get spool data
028500990905     C                   CALL      'QSPGETSP'
028600990905     C                   PARM                    spl_hdl                    I
028700991104     C                   PARM                    splspc_name                I
028800990906     C                   PARM      'SPFR0200'    fmt_name          8        I
028900991031     C                   PARM      -1            spl_bufnbr                 I
029000990905     C                   PARM      '*WAIT     '  spl_end          10        I
029100990905     C                   PARM                    QUSEC                      I/O
029200990905      *
029300990905     C                   IF        QUSBAVL > 0
029400991104     C                   EVAL      msg_data = 'API QSPGETSP failed : ' + QUSEI
029500991104     C                   EXSR      #QUIT
029600990905     C                   END
029700991104     C   90              CALLP     dp(' API QSPGETSP successful.')
029800991031      *   retrieve 'General header'
029900991031     C                   EVAL      QSPSPFRH = gen_hdr
030000991102     C   90              CALLP     dp(' generic header :'
030100991031     C                              + ' level = ' + QSPSFILL
030200991031     C                              + ' format = ' + QSPFN
030300991031     C                              + ' complete = ' + QSPICI
030400991104     C                              + ' sizeofUS = ' + %TRIMR(n2c(QSPUSU))
030500991104     C                              + ' reqbuf = ' + %TRIMR(n2c(QSPBR00))
030600991104     C                              + ' retbuf = '+%TRIMR(n2c(QSPBRTN01)))
030700990905      * Close spool file
030800990905     C                   CALL      'QSPCLOSP'
030900990905     C                   PARM                    spl_hdl                    I
031000990905     C                   PARM                    QUSEC                      I/O
031100990905      *
031200990905     C                   IF        QUSBAVL > 0
031300991104     C                   EVAL      msg_data = 'API QSPCLOSP failed : ' + QUSEI
031400991104     C                   EXSR      #QUIT
031500990905     C                   END
031600991104     C   90              CALLP     dp(' API QSPCLOSP successful.')
031700990905      *
031800991104     C                   IF        QSPICI <> 'C'
031900991104     C                   EVAL      msg_data = 'Cannot process sppoled file ' +
032000991104     C                                        'larger than 16M.'
032100991104     C                   EXSR      #QUIT
032200991104     C                   END
032300991104      *
032400990905     C                   ENDSR
032500990905      *****************************************************************
032600990905     C     #HPT          BEGSR
032700991102      * Clear option specific I/O information
032800990905     C                   CLEAR                   QWPPTOSI
032900990905     C                   CLEAR                   QWPPTOSO
033000990907      *   API parameters
033100991104     C                   EVAL      splbuf_p = splspc_p
033200990905     C                   EVAL      hptsplbuflen = 0
033300990908     C                   EVAL      hptosolena = 0
033400990908     C                   EVAL      hptxbuflena = 0
033500990907      * Set parameters for QWPZHPTR
033600990907     C                   SELECT
033700990907      * 10 = initialize HPT
033800991031      *                  (no further parameters required)
033900990907      * 20 = process file
034000990907     C                   WHEN      hptopt = 20
034100990905      *   Option specific input information
034200990724     C                   EVAL      QWPPDN = '*NONE'
034300991031     C                   EVAL      QWPJN = %SUBST(spl_job : 1 : 10)
034400991031     C                   EVAL      QWPUN = %SUBST(spl_job : 11 : 10)
034500991031     C                   EVAL      QWPJNBR = %SUBST(spl_job : 21 : 6)
034600991104     C                   EVAL      QWPSNBR = spl_nbr
034700991031     C                   EVAL      QWPSN = spl_name
034800990724     C                   EVAL      QWPRAD = '0'
034900991031     C                   EVAL      QWPWCOBJ = %SUBST(wscst : 1 : 10)
035000991031     C                   EVAL      QWPWCOL = %SUBST(wscst : 11 : 10)
035100990724     C                   EVAL      QWPMTM = '*WSCST'
035200990907      * 30 = transform data
035300990907     C                   WHEN      hptopt = 30
035400990905      *   Option specific input information
035500990905     C                   EVAL      QWPRAD = '0'
035600990908      *     Adjust page number
035700990908     C                   EVAL      QWPNBRCP = QSPNBRPE
035800991104     C                   ADD       QSPNBRPE      total_pages       9 0
035900990908     C                   IF        QSPBNBR = 1
036000990908     C                   IF        (QSPNBRPE > 0) AND (QSPLPC = 'Y')
036100990908     C                   SUB       1             QWPNBRCP
036200990908     C                   ENDIF
036300990908     C                   ELSE
036400990908     C                   IF        QSPLPC = 'N'
036500990908     C                   ADD       1             QWPNBRCP
036600990908     C                   ENDIF
036700990908     C                   ENDIF
036800990905      *   API parameters
036900991104     C                   EVAL      splbuf_p = splspc_p + QSPOPD00
037000990907     C                   EVAL      hptsplbuflen = QSPSPD00
037100990907      * 40 = end file
037200991104      *                  (no further parameters required)
037300990907      * 50 = terminate HPT
037400991104      *                  (no further parameters required)
037500990907     C                   ENDSL
037600991031      * perform HPT
037700991102     C                   EXSR      #EXHPT
037800000312     C                   IF        hptopt = 30
037900000318      *   Increment counter
038000000318     C                   ADD       1             counter           7 0
038100000318     C                   IF        counter >= 5
038200000318     C                   CALLP     sndpm(%TRIMR(n2c(total_pages)) +
038300000318     C                                   ' pages processed. (' + %TRIMR(
038400000318     C                                   n2c((QSPBNBR / QSPBRTN01) * 100)) +
038500000318     C                                   '%)' : 'S')
038600000318     C                   Z-ADD     0             counter
038700000312     C                   ENDIF
038800000318     C                   ENDIF
038900991031      * debug information print out
039000991104     C   90              CALLP     dp('hptopt = ' + n2c(hptopt))
039100991031     C   90              SELECT
039200990907     C                   WHEN      hptopt = 10
039300991104     C                   CALLP     dp(' QUSBAVL = ' + %TRIMR(n2c(QUSBAVL)))
039400990907     C                   WHEN      hptopt = 20
039500991104     C                   CALLP     dp(' QUSBAVL = ' + %TRIMR(n2c(QUSBAVL)) +
039600991104     C                             ' splbuflen = ' + %TRIMR(n2c(hptsplbuflen)) +
039700991104     C                             ' hptosolena = ' + %TRIMR(n2c(hptosolena)) +
039800991104     C                             ' hptxbuflena = ' + n2c(hptxbuflena))
039900991104     C                   CALLP     dp('  Option specific input information /' +
040000991104     C                             ' device = ''' + %TRIM(QWPPDN) + '''' +
040100991104     C                             ' job = ' + %TRIM(QWPJN) + '/' +
040200990908     C                                  %TRIM(QWPUN) + '/' + %TRIM(QWPJNBR))
040300991102     C                   CALLP     dp('   spoolfile = ''' + %TRIM(QWPSN) + ''''
040400991104     C                             + ' spoolNo = ' + %TRIMR(n2c(QWPSNBR)) +
040500991104     C                             ' pages = ' + %TRIMR(n2c(QWPNBRCP)) +
040600991104     C                             ' WSCST = ' + '''' + %TRIM(QWPWCOL) + '/' +
040700991104     C                                           %TRIM(QWPWCOBJ) + '''' +
040800991104     C                             ' Mmodel = ''' + %TRIM(QWPMTM) + '''')
040900991104     C                   CALLP     dp('  Option specific output information /' +
041000991104     C                             ' transform file = ' + QWPTFIL +
041100991105     C                             ' pass data = ' + QWPPID)
041200991102     C                   WHEN      hptopt = 30 or hptopt = 40
041300991102     C                   IF        hptopt = 30
041400991104     C                   CALLP     dp(' buffer info :' +
041500991104     C                             ' No = ' + %TRIMR(n2c(QSPBNBR)) +
041600991104     C                             ' len = '+%TRIMR(n2c(QSPLBI)) +
041700991104     C                             ' numentry = ' + %TRIMR(n2c(QSPNBRPE)) +
041800991104     C                             ' offset = ' + %TRIMR(n2c(QSPOPD00)) +
041900991104     C                             ' size = ' + %TRIMR(n2c(QSPSPD00)) +
042000991104     C                             ' / page info : LastPageCont = ' + QSPLPC)
042100991102     C                   ENDIF
042200991104     C                   CALLP     dp(' QUSBAVL = ' + %TRIMR(n2c(QUSBAVL)) +
042300991104     C                             ' splbuflen = ' + %TRIMR(n2c(hptsplbuflen)) +
042400991104     C                             ' hptosolena = ' + %TRIMR(n2c(hptosolena)) +
042500991104     C                             ' hptxbuflena = ' + %TRIMR(n2c(hptxbuflena))+
042600991104     C                             ' pages(OSI) = ' + %TRIMR(n2c(QWPNBRCP)) +
042700991104     C                             ' done(OSO) = ' + QWPDTFIL)
042800990907     C                   WHEN      hptopt = 50
042900991104     C                   CALLP     dp(' QUSBAVL = ' + %TRIMR(n2c(QUSBAVL)) +
043000991104     C                             ' hptxbuflena = ' + %TRIMR(n2c(hptxbuflena)))
043100990907     C                   ENDSL
043200991031      *
043300991102     C                   IF        QUSBAVL > 0
043400991102      * retry hpt if CPF6DF5 (process option parameter not valid)
043500991102     C  N99              IF        (QUSEI  = 'CPF6DF5') AND (hptopt = 10)
043600000317     C   90              CALLP     dp('** hpt sequence error detected **')
043700991102      *   avoid loop
043800991102     C                   SETON                                        99
043900991102      *   terminate HTP then try again
044000000317     C   90              CALLP     dp('** reset hpt - process option 50 **')
044100991102     C                   EVAL      hptopt = 50
044200991102     C                   EXSR      #EXHPT
044300000317     C   90              CALLP     dp('** retry hpt - process option 10 **')
044400991102     C                   EVAL      hptopt = 10
044500991102     C                   CLEAR                   QWPPTOSI
044600991102     C                   CLEAR                   QWPPTOSO
044700991102     C                   EXSR      #EXHPT
044800991102     C                   ELSE
044900991104      * reset hpt and exit
045000991104     C                   EVAL      msg_data = 'API QwpzHostPrintTransform ' +
045100991104     C                               'failed : ' + QUSEI + ' hptopt = ' +
045200991104     C                               %TRIM(%EDITC(hptopt:'J'))
045300000317     C   90              CALLP     dp('** reset hpt - process option 50 **')
045400991104     C                   EVAL      hptopt = 50
045500991104     C                   EXSR      #EXHPT
045600991104     C                   EXSR      #QUIT
045700991102     C                   ENDIF
045800990905     C                   ENDIF
045900991104      * write data to stream file
046000991104     C                   IF        hptxbuflena > 0
046100991104     C                   EVAL      bytesw = write(fd : xbufspc_p : hptxbuflena)
046200991104     C                   IF        bytesw <> hptxbuflena
046300991104     C                   EVAL      msg_data = 'write() failed. ' + geterrinfo
046400991104     C                   EXSR      #QUIT
046500991104     C                   ENDIF
046600991104     C                   ENDIF
046700990907      *
046800990905     C                   ENDSR
046900991102      *****************************************************************
047000991102     C     #EXHPT        BEGSR
047100991102      * perform HPT
047200991102     C                   CALLP     hpt(%ADDR(hptopt) :
047300991104     C                                 %ADDR(QWPPTOSI) :
047400991104     C                                 %ADDR(hptosilen) :
047500991104     C                                 splbuf_p :
047600991104     C                                 %ADDR(hptsplbuflen) :
047700991104     C                                 %ADDR(QWPPTOSO) :
047800991104     C                                 %ADDR(hptosolen) :
047900991102     C                                 %ADDR(hptosolena) :
048000991104     C                                 xbufspc_p :
048100991104     C                                 %ADDR(hptxbuflen) :
048200991102     C                                 %ADDR(hptxbuflena) :
048300991102     C                                 %ADDR(QUSEC))
048400991104      *
048500991102     C                   ENDSR
048600990907      *****************************************************************
048700991031     C     #OPEN         BEGSR
048800991031      *
048900000312     C                   EVAL      stmf = %TRIM(stmf_path) + X'00'
049000991102      * stream file exists?
049100991102     C   90              CALLP     dp(' ')
049200991102     C   90              CALLP     dp('Checking output stream file.')
049300991102     C   90              CALLP     dp(' file name = ' + %STR(%ADDR(stmf)))
049400991102     C                   EVAL      fd = open(%ADDR(stmf) : 1)
049500991102      *   yes
049600991102 I   C                   IF        fd <> -1
049700991102     C   90              CALLP     dp(' stream file already exists.')
049800991102  I  C                   IF        -1 = close(fd)
049900991104     C                   EVAL      msg_data = 'close() failed. ' + geterrinfo
050000991104     C                   EXSR      #QUIT
050100991102  E  C                   ENDIF
050200991102      *     replace(*yes) specified?
050300991102  I  C                   IF        stmf_replace = '*YES'
050400991102   I C                   IF        -1 = unlink(%ADDR(stmf))
050500991104     C                   EVAL      msg_data = 'unlink() failed. ' + geterrinfo
050600991104     C                   EXSR      #QUIT
050700991102   X C                   ELSE
050800991102     C   90              CALLP     dp(' unlink() successful.')
050900000312     C                   CALLP     sndpm('Stream file ' + %TRIMR(stmf_path) +
051000000312     C                                   ' removed.' : 'D')
051100991102   E C                   ENDIF
051200991102  X  C                   ELSE
051300991104     C                   EVAL      msg_data = 'file already exists.'
051400991104     C                   EXSR      #QUIT
051500991102  E  C                   ENDIF
051600991102 X   C                   ELSE
051700991102     C   90              CALLP     dp(' stream file not found.')
051800991102 E   C                   ENDIF
051900991031      * open(create) stream file
052000991031     C                   EVAL      fd = open(%ADDR(stmf)
052100991031     C                               : O_CREAT + O_WRONLY + O_TRUNC + O_CODEPAGE
052200991031     C                               : S_IRWXU + S_IROTH
052300991031     C                               : 819)
052400990907     C                   IF        fd = -1
052500991104     C                   EVAL      msg_data = 'open() failed. ' + geterrinfo
052600991104     C                   EXSR      #QUIT
052700990907     C                   ENDIF
052800991031     C   90              CALLP     dp('Stream file opened.')
052900991102     C   90              CALLP     dp(' ')
053000990907      *
053100990907     C                   ENDSR
053200990907      *****************************************************************
053300990907     C     #CLOSE        BEGSR
053400000312      * Clear status message
053500000312     C                   CALLP     sndpm(' ' : 'X')
053600990907      * Close stream file
053700990907     C                   IF        -1 = close(fd)
053800991104     C                   EVAL      msg_data = 'close() failed. ' + geterrinfo
053900991104     C                   EXSR      #QUIT
054000990907     C                   ENDIF
054100991102     C   90              CALLP     dp(' ')
054200991031     C   90              CALLP     dp('Stream file closed.')
054300991102     C   90              CALLP     dp(' ')
054400991104      *
054500991104     C                   TIME                    end_time
054600991104     C     end_time      SUBDUR    start_time    dur:*S            9 0
054700991104     C     dur           DIV       60            dur_min           7 0
054800991104     C                   MVR                     dur_sec           2 0
054900991105     C                   EVAL      msg_data = 'execution time ' +
055000991105     C                                %TRIMR(n2c(dur_min)) + ' min ' +
055100991105     C                                %TRIMR(n2c(dur_sec)) + ' sec, ' +
055200991105     C                                'total pages = ' + n2c(total_pages)
055300991105     C   90              CALLP     dp(msg_data)
055400991102     C   90              CALLP     dp('SPL2STMF completed successfully.')
055500000312     C                   CALLP     sndpm('Stream file generated sucessfully. ' +
055600000312     C                                   msg_data : 'C')
055700990907      *
055800990907     C                   ENDSR
055900990905      *****************************************************************
056000991104     C     #QUIT         BEGSR
056100990905      * abort
056200991102     C   90              CALLP     dp(' ')
056300991102     C   90              CALLP     dp('Fatal error : ' + msg_data)
056400000312     C                   CALLP     sndpm('Command failed. reason - ' + msg_data
056500000312     C                                   : 'E')
056600990905     C                   EVAL      *INLR = '1'
056700990905     C                   RETURN
056800990905      *
056900990905     C                   ENDSR
057000990907      *****************************************************************
057100990907     OQPRINT    E                           1
057200990907     O                       pm                 132
057300990907      /EJECT
057400990905      *****************************************************************
057500990907      * debug print out
057600990907     Pdp               B
057700990907     Ddp               PI
057800990907     D instr                        132    VALUE
057900990907      *
058000990907     Dcurtime          S               Z
058100990907     DcurtimeHMS       S               T
058200990907     C                   MOVEL     instr         pm
058300990907      *
058400990907      * get current time
058500990907     C                   TIME                    curtime
058600990907     C                   MOVE      curtime       curtimeHMS
058700990907     C                   MOVE      curtimeHMS    hms               6 0
058800990907     C                   EXTRCT    curtime:*MS   ms                6 0
058900990907     C                   EVAL      pm = %EDITW(hms : '  :  :  ') + '.' +
059000990907     C                                  %SUBST(%EDITW(ms : '0      ') : 2 : 3) +
059100990907     C                                  '|' + %TRIMR(pm)
059200990907     C
059300990907     C                   EXCEPT
059400990907      *
059500990907     C                   RETURN
059600990907      *
059700990907     Pdp               E
059800991104      *****************************************************************
059900991104     Pn2c              B
060000991104     Dn2c              PI            12
060100991104     D numeric                       10I 0 VALUE
060200991104      *
060300991104     C                   RETURN    %TRIML(%EDITC(numeric : 'P'))
060400991104     Pn2c              E
060500000312      *****************************************************************
060600000312     Pq2s              B
060700000312     Dq2s              PI            21
060800000312     D qual                          20    VALUE
060900000312      *
061000000312     C                   RETURN       %TRIMR(%SUBST(qual : 11 : 10)) + '/' +
061100000312     C                                %SUBST(qual :  1 : 10)
061200000312     Pq2s              E
061300990907      *****************************************************************
061400991031     Pgeterrinfo       B
061500991031     Dgeterrinfo       PI           128
061600991031      *
061700991031     Dgeterrno         PR              *   EXTPROC('__errno')
061800991031      *
061900991031     Dstrerror         PR              *   EXTPROC('strerror')
062000991031     D errno                         10I 0 VALUE
062100991031      *
062200991031     Derrnum           S             10I 0 BASED(errnum_p)
062300991031      *
062400991031     C                   EVAL      errnum_p = geterrno
062500991031      *
062600991031     C                   RETURN    %TRIM(%EDITC(errnum : '3')) + ' : ' +
062700991031     C                             %STR(strerror(errnum))
062800991031     Pgeterrinfo       E
062900000312      *****************************************************************
063000000312     Psndpm            B
063100000312     Dsndpm            PI
063200000312     D msg_data                     256    VALUE
063300000312     D msg_t                          1    VALUE
063400000312      *
063500000312     Dmsg_file         S             20    INZ('QCPFMSG   *LIBL')
063600000312     Dmsg_len          S              9B 0
063700000312     Dstack_ctr        S              9B 0 INZ(1)
063800000312      *
063900000312     C                   EVAL      msg_len = %LEN(%TRIMR(msg_data))
064000000312     C                   SELECT
064100000312      * escape message (fatal error) 512
064200000312     C                   WHEN      msg_t = 'E'
064300000312     C                   EVAL      msg_type = '*ESCAPE'
064400000312     C                   EVAL      msg_id = 'CPF9897'
064500000312     C                   EVAL      stack_ent = '*'
064600000312     C                   EVAL      stack_ctr = 3
064700000312      * daignostic message (information, warning) 132
064800000312     C                   WHEN      msg_t = 'D'
064900000312     C                   EVAL      msg_type = '*DIAG'
065000000312     C                   EVAL      msg_id = 'CPDA0FF'
065100000312     C                   EVAL      stack_ent = '*'
065200000312     C                   EVAL      stack_ctr = 0
065300000312      * completion message (normal end) 255
065400000312     C                   WHEN      msg_t = 'C'
065500000312     C                   EVAL      msg_type = '*COMP'
065600000312     C                   EVAL      msg_id = 'CPI8859'
065700000312     C                   EVAL      stack_ent = '*'
065800000312     C                   EVAL      stack_ctr = 3
065900000312      * status message
066000000312     C                   WHEN      msg_t = 'S'
066100000312     C                   EVAL      msg_type = '*STATUS'
066200000312     C                   EVAL      msg_id = 'CPF9898'
066300000312     C                   EVAL      stack_ent = '*EXT'
066400000312     C                   EVAL      stack_ctr = 1
066500000312      * clear status message
066600000312     C                   WHEN      msg_t = 'X'
066700000312     C                   EVAL      msg_type = '*STATUS'
066800000312     C                   EVAL      msg_id = 'CPI9801'
066900000312     C                   EVAL      stack_ent = '*EXT'
067000000312     C                   EVAL      stack_ctr = 1
067100000312      * invalid msg_t
067200000312     C                   OTHER
067300000312     C                   RETURN
067400000312     C                   ENDSL
067500000312      *
067600000312     C                   CALL      'QMHSNDPM'
067700000312     C                   PARM                    msg_id            7        I
067800000312     C                   PARM                    msg_file                   I
067900000312     C                   PARM                    msg_data                   I
068000000312     C                   PARM                    msg_len                    I
068100000312     C                   PARM                    msg_type         10        I
068200000312     C                   PARM                    stack_ent        10        I
068300000312     C                   PARM                    stack_ctr                  I
068400000312     C                   PARM                    msg_key           4        O
068500000312     C                   PARM                    QUSEC                      I/O
068600000312      *
068700000312     C                   RETURN
068800000312     Psndpm            E
