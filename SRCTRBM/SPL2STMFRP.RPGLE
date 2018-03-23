000100991105      **************************************************************************
000200060720      * Conversione SCS/AFP spoold file a STMF in IFS applicando *WSCST
000300060720      * (Occhio agli spool troppo grossi => MAX 16M)
000400991105      **************************************************************************
000500990907      * Printer file for debug output
000600990907     FQPRINT    O    F  132        PRINTER USROPN OFLIND(*INOV)
000700990907      *****************************************************************
000800990907      * Program version
000900060720     D Version         C                   '0-04 04-05-23'
001000060720      * Changed to initialize API fields:
001100060720      *  - QWPJSN "Job System Name"  => "*ONLY"
001200060720      *  - QWPSCD "Splf Create Date" => "*ONLY"
001300060720      *  - QWPSCT "Splf Create Time" => *blanks
001400040523      * ........................................................................
001500990905      * User space error code
001600990905     D/COPY QSYSINC/QRPGLESRC,QUSEC
001700991031      * Retreive error information .............................................
001800991031     Dgeterrinfo       PR           128
001900991104      * Convert numeric to char ................................................
002000991104     Dn2c              PR            12
002100991104     D numeric                       10I 0 VALUE
002200000312      * Qual to string .........................................................
002300000312     Dq2s              PR            21
002400000312     D qual                          20    VALUE
002500000312      * Send program message ...................................................
002600000312     Dsndpm            PR
002700000312     D msg_data                     256    VALUE
002800000312     D msg_t                          1    VALUE
002900000312      * Debug print out ........................................................
003000991104     Ddp               PR
003100991104     D instr                        132    VALUE
003200990908      * Host print transform API ...............................................
003300990908     Dhpt              PR                  EXTPROC('QwpzHostPrintTransform')
003400990908     D                                 *   VALUE
003500990908     D                                 *   VALUE
003600990908     D                                 *   VALUE
003700990908     D                                 *   VALUE
003800990908     D                                 *   VALUE
003900990908     D                                 *   VALUE
004000990908     D                                 *   VALUE
004100990908     D                                 *   VALUE
004200990908     D                                 *   VALUE
004300990908     D                                 *   VALUE
004400990908     D                                 *   VALUE
004500990908     D                                 *   VALUE
004600991031      * Option specific input/output information
004700991031     D/COPY QSYSINC/QRPGLESRC,QWPZ
004800991031      * variables for Host Print Transform API
004900990908     Dhptopt           S              9B 0
005000991031     Dhptosilen        S              9B 0 INZ(%LEN(QWPPTOSI))
005100990908     Dhptsplbuflen     S              9B 0
005200991031     Dhptosolen        S              9B 0 INZ(%LEN(QWPPTOSO))
005300990908     Dhptosolena       S              9B 0
005400991104     Dxbufspc_p        S               *
005500991104     Dhptxbuflen       S              9B 0 INZ(200000)
005600990908     Dhptxbuflena      S              9B 0
005700990726      * Size of user space
005800990726     Dspc_size         S              9B 0
005900991105      * Stream file APIs .......................................................
006000991031     Dunlink           PR             9B 0 EXTPROC('unlink')
006100991031     D                                 *   VALUE
006200991031     Dopen             PR            10I 0 EXTPROC('open')
006300991031     D                                 *   VALUE
006400991031     D                               10I 0 VALUE
006500991031     D                               10U 0 VALUE OPTIONS(*NOPASS)
006600991031     D                               10U 0 VALUE OPTIONS(*NOPASS)
006700991031     D O_CREAT         S             10I 0 INZ(8)
006800991031     D O_WRONLY        S             10I 0 INZ(2)
006900991031     D O_TRUNC         S             10I 0 INZ(64)
007000991031     D O_CODEPAGE      S             10I 0 INZ(8388608)
007100991031     D S_IRWXU         S             10I 0 INZ(448)
007200991031     D S_IROTH         S             10I 0 INZ(4)
007300990724     Dwrite            PR            10I 0 EXTPROC('write')
007400990724     D                               10I 0 VALUE
007500000000     D                                 *   VALUE
007600990724     D                               10I 0 VALUE
007700990724     Dclose            PR            10I 0 EXTPROC('close')
007800990724     D                               10I 0 VALUE
007900990724      *
008000990724     Dfd               S             10I 0
008100990724     Dbytesw           S             10I 0
008200990724     Dstmf             S            257
008300990907      * 'QSPOPNSP/QSPGETSP/QSPCLOSP' spool file APIs variables .................
008400990905     Dspl_hdl          S              9B 0
008500991031     Dspl_nbr_b        S              9B 0
008600991031     Dspl_bufnbr       S              9B 0
008700990907      * Get Spooled File Data ..................................................
008800990905     D/COPY QSYSINC/QRPGLESRC,QSPGETSP
008900991104     Dgen_hdr          S            128    BASED(splspc_p)
009000990906     Dbuf_inf          S             40    BASED(bufp)
009100990906     Dbuf_inf2         S             44    BASED(bufp2)
009200991104     Dsplbuf           S           5000    BASED(splbuf_p)
009300991104      * Misc ...................................................................
009400000312     Dmsg_data         S            256
009500991104     Dstart_time       S               Z
009600991104     Dend_time         S               Z
009700991104      * Debug printout string ..................................................
009800990907     Dpm               S            132
009801070206      *****************************************************************
009802070206      * Definizione variabili d wrk
009803070206     D spl_nbr         S              9B 0
009804070206      *****************************************************************
009900000000      /EJECT
010000000000      **************************************************************************
010100990724      * Main
010200000000      *****************************************************************
010300990907      * Initialize
010400060720     C                   EXSR      nINIT
010500990907      * Prepare user space
010600060720     C                   EXSR      nPPRUS
010700991104      * Retrieve spool file
010800060720     C                   EXSR      nRTVSP
010900991102      * Create stream file
011000060720     C                   EXSR      nOPEN
011100991102      * Perform HTP
011200991102      *   initialize HPT
011300991102     C                   EVAL      hptopt = 10
011400060720     C                   EXSR      nHPT
011500991102      *   process file
011600991102     C                   EVAL      hptopt = 20
011700060720     C                   EXSR      nHPT
011800990907      *   set pointer to first buffer
011900991104     C                   EVAL      bufp = splspc_p + QSPOFB
012000990907      *   loop thru buffer
012100990907     C                   DO        QSPBRTN01
012200990907      *     retrieve 'Buffer information'
012300990906     C                   EVAL      QSPSPFRB = buf_inf
012400990907      *     set pointer to 'offset to general information buffer'
012500991104     C                   EVAL      bufp2 = splspc_p + QSPOGI
012600990907      *     retrieve 'General data (information buffer)'
012700990906     C                   EVAL      QSPSPFRG = buf_inf2
012800990907      *     transform data
012900990907     C                   EVAL      hptopt = 30
013000060720     C                   EXSR      nHPT
013100990907      *     increment pointer by 'length of all buffer information'
013200990906     C                   EVAL      bufp = bufp + QSPLBI
013300990906     C                   END
013400990907      *   end file
013500990905     C                   EVAL      hptopt = 40
013600060720     C                   EXSR      nHPT
013700990907      *   terminate HTP
013800990905     C                   EVAL      hptopt = 50
013900060720     C                   EXSR      nHPT
014000990907      *   Close stream file
014100060720     C                   EXSR      nCLOSE
014200990907      * End program
014300990907     C                   SETON                                        LR
014400990907     C                   RETURN
014500000000      /EJECT
014600000000      **************************************************************************
014700990724      * Subroutines
014800000000      *****************************************************************
014900990907      * Initialize
015000060720     C     nINIT         BEGSR
015100990907      *
015200991031     C     *ENTRY        PLIST
015300991031     C                   PARM                    spl_name         10        I
015400000312     C                   PARM                    stmf_path       128        I
015500991031     C                   PARM                    wscst            20        I
015600991031     C                   PARM                    spl_job          26        I
015700070206     C                   PARM                    spl_nbr                    I
015800991031     C                   PARM                    stmf_replace      4        I
015900000317     C                   PARM                    debug             4        I
016000991104      * Remember start time
016100991104     C                   TIME                    start_time
016200991104      * Debug mode?
016300000317     C     debug         COMP      '*YES'                                 90
016400991031     C   90              OPEN      QPRINT
016500991031     C   90              CALLP     dp('Debug mode.  ' +
016600991031     C                                      %TRIM(%EDITC(*DATE : 'Y')))
016700991031     C   90              CALLP     dp('*** SPL2STMF version ' +
016800991031     C                                      %TRIM(version) + ' ***')
016900991031     C   90              CALLP     dp(' ')
017000991031     C   90              CALLP     dp('Input Parameters')
017100991031     C   90              CALLP     dp(' splf     : ' + spl_name)
017200000312     C   90              CALLP     dp(' stmf     : ' + stmf_path)
017300000312     C   90              CALLP     dp(' wscst    : ' + q2s(wscst))
017400991031     C   90              CALLP     dp(' spl_job  : ' + spl_job)
017500000312     C   90              CALLP     dp(' spl_nbr  : ' + n2c(spl_nbr))
017600991031     C   90              CALLP     dp(' replace  : ' + stmf_replace)
017700990907      * Set APIs not to raise an exception
017800990907      *   System API Programming Version 4 SC41-5800-00
017900990907      *   2.4.3.2 Receiving the Error Code without the Exception Data--Example
018000990907     C                   Z-ADD     16            QUSBPRV
018100990907      *
018200990907     C                   ENDSR
018300990907      *****************************************************************
018400991104      * Create user space to retrieve spool data
018500060720     C     nPPRUS        BEGSR
018600991104     C                   EVAL      splspc_name = 'QSPGETSP  QTEMP'
018700991104     C   90              CALLP     dp(' ')
018800000312     C   90              CALLP     dp('Create User Space ' + q2s(splspc_name))
018900991104      *
019000990726     C                   CALL      'QUSCRTUS'
019100991104     C                   PARM                    splspc_name      20        I
019200990726     C                   PARM      *BLANKS       spc_attr         10        I
019300990905     C                   PARM      1024          spc_size                   I
019400990726     C                   PARM      X'00'         spc_init          1        I
019500990726     C                   PARM      '*CHANGE'     spc_aut          10        I
019600991031     C                   PARM      'SPL2STMF'    spc_text         50        I
019700991104     C                   PARM      '*NO'         spc_replace      10        I
019800990726     C                   PARM                    QUSEC                      I/O
019900990726     C                   PARM      '*USER'       spc_domain       10        I
020000990726      *
020100991104     C                   IF        QUSBAVL > 0
020200991104     C                   IF        QUSEI =  'CPF9870'
020300000312     C   90              CALLP     dp(' User Space ' + %TRIMR(q2s(splspc_name))
020400000312     C                                + ' already exists.')
020500991104     C                   ELSE
020600991104     C                   EVAL      msg_data = 'API QUSCRTUS failed : ' + QUSEI
020700060720     C                   EXSR      nQUIT
020800991104     C                   END
020900991104     C                   ELSE
021000991104     C   90              CALLP     dp(' API QUSCRTUS successful.')
021100991104     C                   END
021200991104      *   Retrieve pointer to user space
021300990905     C                   CALL      'QUSPTRUS'
021400991104     C                   PARM                    splspc_name                I
021500991104     C                   PARM                    splspc_p                   O
021600990905     C                   PARM                    QUSEC                      I/O
021700990905      *
021800990905     C                   IF        QUSBAVL > 0
021900991104     C                   EVAL      msg_data = 'API QUSPTRUS failed : ' + QUSEI
022000060720     C                   EXSR      nQUIT
022100990905     C                   END
022200991104     C   90              CALLP     dp(' API QUSPTRUS successful.')
022300991104      * Create user space for translation
022400991104     C                   EVAL      xbufspc_name = 'QWPZHPT1  QTEMP'
022500991104     C   90              CALLP     dp(' ')
022600000312     C   90              CALLP     dp('Create User Space ' + q2s(xbufspc_name))
022700991104      *
022800991104     C                   CALL      'QUSCRTUS'
022900991104     C                   PARM                    xbufspc_name     20        I
023000991104     C                   PARM      *BLANKS       spc_attr                   I
023100991104     C                   PARM      200000        spc_size                   I
023200991104     C                   PARM      X'00'         spc_init                   I
023300991104     C                   PARM      '*CHANGE'     spc_aut                    I
023400991104     C                   PARM      'SPL2STMF'    spc_text                   I
023500991104     C                   PARM      '*NO'         spc_replace                I
023600991104     C                   PARM                    QUSEC                      I/O
023700991104     C                   PARM      '*USER'       spc_domain                 I
023800991104      *
023900991104     C                   IF        QUSBAVL > 0
024000991104     C                   IF        QUSEI =  'CPF9870'
024100000312     C   90              CALLP     dp(' User Space ' + %TRIMR(q2s(xbufspc_name))
024200000312     C                                + ' already exists.')
024300991104     C                   ELSE
024400991104     C                   EVAL      msg_data = 'API QUSCRTUS failed : ' + QUSEI
024500060720     C                   EXSR      nQUIT
024600991104     C                   END
024700991104     C                   ELSE
024800991104     C   90              CALLP     dp(' API QUSCRTUS successful.')
024900991104     C                   END
025000991104      *   Retrieve pointer to user space
025100991104     C                   CALL      'QUSPTRUS'
025200991104     C                   PARM                    xbufspc_name               I
025300991104     C                   PARM                    xbufspc_p                  O
025400991104     C                   PARM                    QUSEC                      I/O
025500991104      *
025600991104     C                   IF        QUSBAVL > 0
025700991104     C                   EVAL      msg_data = 'API QUSPTRUS failed : ' + QUSEI
025800060720     C                   EXSR      nQUIT
025900991104     C                   END
026000991104     C   90              CALLP     dp(' API QUSPTRUS successful.')
026100991104      *
026200991104     C                   ENDSR
026300990905      *****************************************************************
026400991104      * Retrieve spool file
026500060720     C     nRTVSP        BEGSR
026600991104      *
026700991104     C   90              CALLP     dp(' ')
026800991104     C   90              CALLP     dp('Retrieve spool file.')
026900991104      * Open spool file
027000990905     C                   CALL      'QSPOPNSP'
027100990905     C                   PARM                    spl_hdl                    O
027200990905     C                   PARM                    spl_job          26        I
027300991031     C                   PARM      *BLANKS       spl_ijobi        16        I
027400991031     C                   PARM      *BLANKS       spl_ispli        16        I
027500991031     C                   PARM                    spl_name                   I
027600991031     C                   PARM      spl_nbr       spl_nbr_b                  I
027700991031     C                   PARM      -1            spl_bufnbr                 I
027800990905     C                   PARM                    QUSEC                      I/O
027900990905      *
028000990905     C                   IF        QUSBAVL > 0
028100991104     C                   EVAL      msg_data = 'API QSPOPNSP failed : ' + QUSEI
028200060720     C                   EXSR      nQUIT
028300990905     C                   END
028400991104     C   90              CALLP     dp(' API QSPOPNSP successful. spl_hdl = ' +
028500991104     C                                                          n2c(spl_hdl))
028600990905      * Get spool data
028700990905     C                   CALL      'QSPGETSP'
028800990905     C                   PARM                    spl_hdl                    I
028900991104     C                   PARM                    splspc_name                I
029000990906     C                   PARM      'SPFR0200'    fmt_name          8        I
029100991031     C                   PARM      -1            spl_bufnbr                 I
029200990905     C                   PARM      '*WAIT     '  spl_end          10        I
029300990905     C                   PARM                    QUSEC                      I/O
029400990905      *
029500990905     C                   IF        QUSBAVL > 0
029600991104     C                   EVAL      msg_data = 'API QSPGETSP failed : ' + QUSEI
029700060720     C                   EXSR      nQUIT
029800990905     C                   END
029900991104     C   90              CALLP     dp(' API QSPGETSP successful.')
030000991031      *   retrieve 'General header'
030100991031     C                   EVAL      QSPSPFRH = gen_hdr
030200991102     C   90              CALLP     dp(' generic header :'
030300991031     C                              + ' level = ' + QSPSFILL
030400991031     C                              + ' format = ' + QSPFN
030500991031     C                              + ' complete = ' + QSPICI
030600991104     C                              + ' sizeofUS = ' + %TRIMR(n2c(QSPUSU))
030700991104     C                              + ' reqbuf = ' + %TRIMR(n2c(QSPBR00))
030800991104     C                              + ' retbuf = '+%TRIMR(n2c(QSPBRTN01)))
030900990905      * Close spool file
031000990905     C                   CALL      'QSPCLOSP'
031100990905     C                   PARM                    spl_hdl                    I
031200990905     C                   PARM                    QUSEC                      I/O
031300990905      *
031400990905     C                   IF        QUSBAVL > 0
031500991104     C                   EVAL      msg_data = 'API QSPCLOSP failed : ' + QUSEI
031600060720     C                   EXSR      nQUIT
031700990905     C                   END
031800991104     C   90              CALLP     dp(' API QSPCLOSP successful.')
031900990905      *
032000991104     C                   IF        QSPICI <> 'C'
032100991104     C                   EVAL      msg_data = 'Cannot process sppoled file ' +
032200991104     C                                        'larger than 16M.'
032300060720     C                   EXSR      nQUIT
032400991104     C                   END
032500991104      *
032600990905     C                   ENDSR
032700990905      *****************************************************************
032800060720     C     nHPT          BEGSR
032900991102      * Clear option specific I/O information
033000990905     C                   CLEAR                   QWPPTOSI
033100990905     C                   CLEAR                   QWPPTOSO
033200990907      *   API parameters
033300991104     C                   EVAL      splbuf_p = splspc_p
033400990905     C                   EVAL      hptsplbuflen = 0
033500990908     C                   EVAL      hptosolena = 0
033600990908     C                   EVAL      hptxbuflena = 0
033700990907      * Set parameters for QWPZHPTR
033800990907     C                   SELECT
033900990907      * 10 = initialize HPT
034000991031      *                  (no further parameters required)
034100990907      * 20 = process file
034200990907     C                   WHEN      hptopt = 20
034300990905      *   Option specific input information
034400990724     C                   EVAL      QWPPDN = '*NONE'
034500991031     C                   EVAL      QWPJN = %SUBST(spl_job : 1 : 10)
034600991031     C                   EVAL      QWPUN = %SUBST(spl_job : 11 : 10)
034700991031     C                   EVAL      QWPJNBR = %SUBST(spl_job : 21 : 6)
034800991104     C                   EVAL      QWPSNBR = spl_nbr
034900991031     C                   EVAL      QWPSN = spl_name
035000990724     C                   EVAL      QWPRAD = '0'
035100991031     C                   EVAL      QWPWCOBJ = %SUBST(wscst : 1 : 10)
035200991031     C                   EVAL      QWPWCOL = %SUBST(wscst : 11 : 10)
035300990724     C                   EVAL      QWPMTM = '*WSCST'
035400040523      * Added for V5R2 API change 2004-05-23 -- start
035500040523     C                   EVAL      QWPJSN = '*ONLY'
035600040523     C                   EVAL      QWPSCD = '*ONLY'
035700040523     C                   EVAL      QWPSCT = '      '
035800040523      * Added for V5R2 API change 2004-05-23 -- end
035900990907      * 30 = transform data
036000990907     C                   WHEN      hptopt = 30
036100990905      *   Option specific input information
036200990905     C                   EVAL      QWPRAD = '0'
036300990908      *     Adjust page number
036400990908     C                   EVAL      QWPNBRCP = QSPNBRPE
036500991104     C                   ADD       QSPNBRPE      total_pages       9 0
036600990908     C                   IF        QSPBNBR = 1
036700990908     C                   IF        (QSPNBRPE > 0) AND (QSPLPC = 'Y')
036800990908     C                   SUB       1             QWPNBRCP
036900990908     C                   ENDIF
037000990908     C                   ELSE
037100990908     C                   IF        QSPLPC = 'N'
037200990908     C                   ADD       1             QWPNBRCP
037300990908     C                   ENDIF
037400990908     C                   ENDIF
037500990905      *   API parameters
037600991104     C                   EVAL      splbuf_p = splspc_p + QSPOPD00
037700990907     C                   EVAL      hptsplbuflen = QSPSPD00
037800990907      * 40 = end file
037900991104      *                  (no further parameters required)
038000990907      * 50 = terminate HPT
038100991104      *                  (no further parameters required)
038200990907     C                   ENDSL
038300991031      * perform HPT
038400060720     C                   EXSR      nEXHPT
038500000312     C                   IF        hptopt = 30
038600000318      *   Increment counter
038700000318     C                   ADD       1             counter           7 0
038800000318     C                   IF        counter >= 5
038900000318     C                   CALLP     sndpm(%TRIMR(n2c(total_pages)) +
039000000318     C                                   ' pages processed. (' + %TRIMR(
039100000318     C                                   n2c((QSPBNBR / QSPBRTN01) * 100)) +
039200000318     C                                   '%)' : 'S')
039300000318     C                   Z-ADD     0             counter
039400000312     C                   ENDIF
039500000318     C                   ENDIF
039600991031      * debug information print out
039700991104     C   90              CALLP     dp('hptopt = ' + n2c(hptopt))
039800991031     C   90              SELECT
039900990907     C                   WHEN      hptopt = 10
040000991104     C                   CALLP     dp(' QUSBAVL = ' + %TRIMR(n2c(QUSBAVL)))
040100990907     C                   WHEN      hptopt = 20
040200991104     C                   CALLP     dp(' QUSBAVL = ' + %TRIMR(n2c(QUSBAVL)) +
040300991104     C                             ' splbuflen = ' + %TRIMR(n2c(hptsplbuflen)) +
040400991104     C                             ' hptosolena = ' + %TRIMR(n2c(hptosolena)) +
040500991104     C                             ' hptxbuflena = ' + n2c(hptxbuflena))
040600991104     C                   CALLP     dp('  Option specific input information /' +
040700991104     C                             ' device = ''' + %TRIM(QWPPDN) + '''' +
040800991104     C                             ' job = ' + %TRIM(QWPJN) + '/' +
040900990908     C                                  %TRIM(QWPUN) + '/' + %TRIM(QWPJNBR))
041000991102     C                   CALLP     dp('   spoolfile = ''' + %TRIM(QWPSN) + ''''
041100991104     C                             + ' spoolNo = ' + %TRIMR(n2c(QWPSNBR)) +
041200991104     C                             ' pages = ' + %TRIMR(n2c(QWPNBRCP)) +
041300991104     C                             ' WSCST = ' + '''' + %TRIM(QWPWCOL) + '/' +
041400991104     C                                           %TRIM(QWPWCOBJ) + '''' +
041500991104     C                             ' Mmodel = ''' + %TRIM(QWPMTM) + '''')
041600991104     C                   CALLP     dp('  Option specific output information /' +
041700991104     C                             ' transform file = ' + QWPTFIL +
041800991105     C                             ' pass data = ' + QWPPID)
041900991102     C                   WHEN      hptopt = 30 or hptopt = 40
042000991102     C                   IF        hptopt = 30
042100991104     C                   CALLP     dp(' buffer info :' +
042200991104     C                             ' No = ' + %TRIMR(n2c(QSPBNBR)) +
042300991104     C                             ' len = '+%TRIMR(n2c(QSPLBI)) +
042400991104     C                             ' numentry = ' + %TRIMR(n2c(QSPNBRPE)) +
042500991104     C                             ' offset = ' + %TRIMR(n2c(QSPOPD00)) +
042600991104     C                             ' size = ' + %TRIMR(n2c(QSPSPD00)) +
042700991104     C                             ' / page info : LastPageCont = ' + QSPLPC)
042800991102     C                   ENDIF
042900991104     C                   CALLP     dp(' QUSBAVL = ' + %TRIMR(n2c(QUSBAVL)) +
043000991104     C                             ' splbuflen = ' + %TRIMR(n2c(hptsplbuflen)) +
043100991104     C                             ' hptosolena = ' + %TRIMR(n2c(hptosolena)) +
043200991104     C                             ' hptxbuflena = ' + %TRIMR(n2c(hptxbuflena))+
043300991104     C                             ' pages(OSI) = ' + %TRIMR(n2c(QWPNBRCP)) +
043400991104     C                             ' done(OSO) = ' + QWPDTFIL)
043500990907     C                   WHEN      hptopt = 50
043600991104     C                   CALLP     dp(' QUSBAVL = ' + %TRIMR(n2c(QUSBAVL)) +
043700991104     C                             ' hptxbuflena = ' + %TRIMR(n2c(hptxbuflena)))
043800990907     C                   ENDSL
043900991031      *
044000991102     C                   IF        QUSBAVL > 0
044100991102      * retry hpt if CPF6DF5 (process option parameter not valid)
044200991102     C  N99              IF        (QUSEI  = 'CPF6DF5') AND (hptopt = 10)
044300000317     C   90              CALLP     dp('** hpt sequence error detected **')
044400991102      *   avoid loop
044500991102     C                   SETON                                        99
044600991102      *   terminate HTP then try again
044700000317     C   90              CALLP     dp('** reset hpt - process option 50 **')
044800991102     C                   EVAL      hptopt = 50
044900060720     C                   EXSR      nEXHPT
045000000317     C   90              CALLP     dp('** retry hpt - process option 10 **')
045100991102     C                   EVAL      hptopt = 10
045200991102     C                   CLEAR                   QWPPTOSI
045300991102     C                   CLEAR                   QWPPTOSO
045400060720     C                   EXSR      nEXHPT
045500991102     C                   ELSE
045600991104      * reset hpt and exit
045700991104     C                   EVAL      msg_data = 'API QwpzHostPrintTransform ' +
045800991104     C                               'failed : ' + QUSEI + ' hptopt = ' +
045900991104     C                               %TRIM(%EDITC(hptopt:'J'))
046000000317     C   90              CALLP     dp('** reset hpt - process option 50 **')
046100991104     C                   EVAL      hptopt = 50
046200060720     C                   EXSR      nEXHPT
046300060720     C                   EXSR      nQUIT
046400991102     C                   ENDIF
046500990905     C                   ENDIF
046600991104      * write data to stream file
046700991104     C                   IF        hptxbuflena > 0
046800991104     C                   EVAL      bytesw = write(fd : xbufspc_p : hptxbuflena)
046900991104     C                   IF        bytesw <> hptxbuflena
047000991104     C                   EVAL      msg_data = 'write() failed. ' + geterrinfo
047100060720     C                   EXSR      nQUIT
047200991104     C                   ENDIF
047300991104     C                   ENDIF
047400990907      *
047500990905     C                   ENDSR
047600991102      *****************************************************************
047700060720     C     nEXHPT        BEGSR
047800991102      * perform HPT
047900991102     C                   CALLP     hpt(%ADDR(hptopt) :
048000991104     C                                 %ADDR(QWPPTOSI) :
048100991104     C                                 %ADDR(hptosilen) :
048200991104     C                                 splbuf_p :
048300991104     C                                 %ADDR(hptsplbuflen) :
048400991104     C                                 %ADDR(QWPPTOSO) :
048500991104     C                                 %ADDR(hptosolen) :
048600991102     C                                 %ADDR(hptosolena) :
048700991104     C                                 xbufspc_p :
048800991104     C                                 %ADDR(hptxbuflen) :
048900991102     C                                 %ADDR(hptxbuflena) :
049000991102     C                                 %ADDR(QUSEC))
049100991104      *
049200991102     C                   ENDSR
049300990907      *****************************************************************
049400060720     C     nOPEN         BEGSR
049500991031      *
049600000312     C                   EVAL      stmf = %TRIM(stmf_path) + X'00'
049700991102      * stream file exists?
049800991102     C   90              CALLP     dp(' ')
049900991102     C   90              CALLP     dp('Checking output stream file.')
050000991102     C   90              CALLP     dp(' file name = ' + %STR(%ADDR(stmf)))
050100991102     C                   EVAL      fd = open(%ADDR(stmf) : 1)
050200991102      *   yes
050300991102 I   C                   IF        fd <> -1
050400991102     C   90              CALLP     dp(' stream file already exists.')
050500991102  I  C                   IF        -1 = close(fd)
050600991104     C                   EVAL      msg_data = 'close() failed. ' + geterrinfo
050700060720     C                   EXSR      nQUIT
050800991102  E  C                   ENDIF
050900991102      *     replace(*yes) specified?
051000991102  I  C                   IF        stmf_replace = '*YES'
051100991102   I C                   IF        -1 = unlink(%ADDR(stmf))
051200991104     C                   EVAL      msg_data = 'unlink() failed. ' + geterrinfo
051300060720     C                   EXSR      nQUIT
051400991102   X C                   ELSE
051500991102     C   90              CALLP     dp(' unlink() successful.')
051600000312     C                   CALLP     sndpm('Stream file ' + %TRIMR(stmf_path) +
051700000312     C                                   ' removed.' : 'D')
051800991102   E C                   ENDIF
051900991102  X  C                   ELSE
052000991104     C                   EVAL      msg_data = 'file already exists.'
052100060720     C                   EXSR      nQUIT
052200991102  E  C                   ENDIF
052300991102 X   C                   ELSE
052400991102     C   90              CALLP     dp(' stream file not found.')
052500991102 E   C                   ENDIF
052600991031      * open(create) stream file
052700991031     C                   EVAL      fd = open(%ADDR(stmf)
052800991031     C                               : O_CREAT + O_WRONLY + O_TRUNC + O_CODEPAGE
052900991031     C                               : S_IRWXU + S_IROTH
053000991031     C                               : 819)
053100990907     C                   IF        fd = -1
053200991104     C                   EVAL      msg_data = 'open() failed. ' + geterrinfo
053300060720     C                   EXSR      nQUIT
053400990907     C                   ENDIF
053500991031     C   90              CALLP     dp('Stream file opened.')
053600991102     C   90              CALLP     dp(' ')
053700990907      *
053800990907     C                   ENDSR
053900990907      *****************************************************************
054000060720     C     nCLOSE        BEGSR
054100000312      * Clear status message
054200000312     C                   CALLP     sndpm(' ' : 'X')
054300990907      * Close stream file
054400990907     C                   IF        -1 = close(fd)
054500991104     C                   EVAL      msg_data = 'close() failed. ' + geterrinfo
054600060720     C                   EXSR      nQUIT
054700990907     C                   ENDIF
054800991102     C   90              CALLP     dp(' ')
054900991031     C   90              CALLP     dp('Stream file closed.')
055000991102     C   90              CALLP     dp(' ')
055100991104      *
055200991104     C                   TIME                    end_time
055300991104     C     end_time      SUBDUR    start_time    dur:*S            9 0
055400991104     C     dur           DIV       60            dur_min           7 0
055500991104     C                   MVR                     dur_sec           2 0
055600991105     C                   EVAL      msg_data = 'execution time ' +
055700991105     C                                %TRIMR(n2c(dur_min)) + ' min ' +
055800991105     C                                %TRIMR(n2c(dur_sec)) + ' sec, ' +
055900991105     C                                'total pages = ' + n2c(total_pages)
056000991105     C   90              CALLP     dp(msg_data)
056100991102     C   90              CALLP     dp('SPL2STMF completed successfully.')
056200000328     C                   CALLP     sndpm('Stream file generated. ' +
056300000312     C                                   msg_data : 'C')
056400990907      *
056500990907     C                   ENDSR
056600990905      *****************************************************************
056700060720     C     nQUIT         BEGSR
056800990905      * abort
056900991102     C   90              CALLP     dp(' ')
057000991102     C   90              CALLP     dp('Fatal error : ' + msg_data)
057100000312     C                   CALLP     sndpm('Command failed. reason - ' + msg_data
057200000312     C                                   : 'E')
057300990905     C                   EVAL      *INLR = '1'
057400990905     C                   RETURN
057500990905      *
057600990905     C                   ENDSR
057700990907      *****************************************************************
057800990907     OQPRINT    E                           1
057900990907     O                       pm                 132
058000990907      /EJECT
058100990905      *****************************************************************
058200990907      * debug print out
058300990907     Pdp               B
058400990907     Ddp               PI
058500990907     D instr                        132    VALUE
058600990907      *
058700990907     Dcurtime          S               Z
058800990907     DcurtimeHMS       S               T
058900990907     C                   MOVEL     instr         pm
059000990907      *
059100990907      * get current time
059200990907     C                   TIME                    curtime
059300990907     C                   MOVE      curtime       curtimeHMS
059400990907     C                   MOVE      curtimeHMS    hms               6 0
059500990907     C                   EXTRCT    curtime:*MS   ms                6 0
059600990907     C                   EVAL      pm = %EDITW(hms : '  :  :  ') + '.' +
059700990907     C                                  %SUBST(%EDITW(ms : '0      ') : 2 : 3) +
059800990907     C                                  '|' + %TRIMR(pm)
059900990907     C
060000990907     C                   EXCEPT
060100990907      *
060200990907     C                   RETURN
060300990907      *
060400990907     Pdp               E
060500991104      *****************************************************************
060600991104     Pn2c              B
060700991104     Dn2c              PI            12
060800991104     D numeric                       10I 0 VALUE
060900991104      *
061000991104     C                   RETURN    %TRIML(%EDITC(numeric : 'P'))
061100991104     Pn2c              E
061200000312      *****************************************************************
061300000312     Pq2s              B
061400000312     Dq2s              PI            21
061500000312     D qual                          20    VALUE
061600000312      *
061700000312     C                   RETURN       %TRIMR(%SUBST(qual : 11 : 10)) + '/' +
061800000312     C                                %SUBST(qual :  1 : 10)
061900000312     Pq2s              E
062000990907      *****************************************************************
062100991031     Pgeterrinfo       B
062200991031     Dgeterrinfo       PI           128
062300991031      *
062400991031     Dgeterrno         PR              *   EXTPROC('__errno')
062500991031      *
062600991031     Dstrerror         PR              *   EXTPROC('strerror')
062700991031     D errno                         10I 0 VALUE
062800991031      *
062900991031     Derrnum           S             10I 0 BASED(errnum_p)
063000991031      *
063100991031     C                   EVAL      errnum_p = geterrno
063200991031      *
063300991031     C                   RETURN    %TRIM(%EDITC(errnum : '3')) + ' : ' +
063400991031     C                             %STR(strerror(errnum))
063500991031     Pgeterrinfo       E
063600000312      *****************************************************************
063700000312     Psndpm            B
063800000312     Dsndpm            PI
063900000312     D msg_data                     256    VALUE
064000000312     D msg_t                          1    VALUE
064100000312      *
064200000312     Dmsg_file         S             20    INZ('QCPFMSG   *LIBL')
064300000312     Dmsg_len          S              9B 0
064400000312     Dstack_ctr        S              9B 0 INZ(1)
064500000312      *
064600000312     C                   EVAL      msg_len = %LEN(%TRIMR(msg_data))
064700000312     C                   SELECT
064800000312      * escape message (fatal error) 512
064900000312     C                   WHEN      msg_t = 'E'
065000000312     C                   EVAL      msg_type = '*ESCAPE'
065100000312     C                   EVAL      msg_id = 'CPF9897'
065200000312     C                   EVAL      stack_ent = '*'
065300000312     C                   EVAL      stack_ctr = 3
065400060720      * diagnostic message (information, warning) 132
065500000312     C                   WHEN      msg_t = 'D'
065600000312     C                   EVAL      msg_type = '*DIAG'
065700000312     C                   EVAL      msg_id = 'CPDA0FF'
065800000312     C                   EVAL      stack_ent = '*'
065900000312     C                   EVAL      stack_ctr = 0
066000000312      * completion message (normal end) 255
066100000312     C                   WHEN      msg_t = 'C'
066200000312     C                   EVAL      msg_type = '*COMP'
066300000312     C                   EVAL      msg_id = 'CPI8859'
066400000312     C                   EVAL      stack_ent = '*'
066500000312     C                   EVAL      stack_ctr = 3
066600000312      * status message
066700000312     C                   WHEN      msg_t = 'S'
066800000312     C                   EVAL      msg_type = '*STATUS'
066900000312     C                   EVAL      msg_id = 'CPF9898'
067000000312     C                   EVAL      stack_ent = '*EXT'
067100000312     C                   EVAL      stack_ctr = 1
067200000312      * clear status message
067300000312     C                   WHEN      msg_t = 'X'
067400000312     C                   EVAL      msg_type = '*STATUS'
067500000312     C                   EVAL      msg_id = 'CPI9801'
067600000312     C                   EVAL      stack_ent = '*EXT'
067700000312     C                   EVAL      stack_ctr = 1
067800000312      * invalid msg_t
067900000312     C                   OTHER
068000000312     C                   RETURN
068100000312     C                   ENDSL
068200000312      *
068300000312     C                   CALL      'QMHSNDPM'
068400000312     C                   PARM                    msg_id            7        I
068500000312     C                   PARM                    msg_file                   I
068600000312     C                   PARM                    msg_data                   I
068700000312     C                   PARM                    msg_len                    I
068800000312     C                   PARM                    msg_type         10        I
068900000312     C                   PARM                    stack_ent        10        I
069000000312     C                   PARM                    stack_ctr                  I
069100000312     C                   PARM                    msg_key           4        O
069200000312     C                   PARM                    QUSEC                      I/O
069300000312      *
069400000312     C                   RETURN
069500000312     Psndpm            E
