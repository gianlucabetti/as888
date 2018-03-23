000100000000     F* TAAJRNAR - Convert journal attributes - called by TAAJRNAC
000200000000     FTEMP132   IF   F  132        DISK
000300000000     FJRNFILP   O    E             DISK    USROPN
000400000000     FJRNACPP   O    E             DISK    USROPN
000500000000     FJRNRCVP   O    E             DISK
000600000000     D DTA             S              1    DIM(132)
000700000000     D DATE            DS
000800000000     D  MM                     1      2
000900000000     D  DD                     3      4
001000000000     D  YY                     5      6
001100000000     ITEMP132   AA  01
001200000000     I                                  1  132  DTA
001300000000     C     *ENTRY        PLIST
001400000000     C                   PARM                    SYSNAM            8
001500000000     C                   PARM                    ONLYRC            4
001600000000     C                   PARM                    RTNCDE            8
001700000000     C* Open up JRNRCVP and JRNFILP if ONLYRC is not *YES
001800000000     C     '*YES'        IFNE      ONLYRC
001900000000     C                   OPEN      JRNFILP
002000000000     C                   OPEN      JRNACPP
002100000000     C                   END
002200000000     C* Initialize
002300000000     C                   TIME                    FULTIM           12 0
002400000000     C                   MOVEL     FULTIM        TIME              6 0
002500000000     C                   MOVE      FULTIM        CVTDAT            6
002600000000     C                   MOVE      TIME          CVTTIM            6
002700000000     C* Get journal and and journal library name
002800000000     C     HDGTG1        TAG
002900000000     C                   READ      TEMP132                                H1
003000000000     C   H1              GOTO      BADEND
003100000000     C                   MOVEA     DTA(2)        TST8              8
003200960113     C                   MOVEA     DTA(69)       TST7              7
003300921113     C     TST8          IFEQ      'Giornale'
003400021116     C     TST7          ANDEQ     'Lib.  .'
003500000000     C                   GOTO      MOVJRN
003600000000     C                   END
003700921113     C     TST8          IFEQ      'GIORNAL '
003800021116     C     TST7          ANDEQ     'LIB.  .'
003900000000     C                   GOTO      MOVJRN
004000000000     C                   END
004100021116     C                   MOVEA     DTA(40)       TST8
004200960304     C     TST8          CABEQ     '* * * * '    ENDPGM
004300000000     C                   GOTO      HDGTG1
004400000000     C     MOVJRN        TAG
004500000000     C* Move journal and library
004600960113     C                   MOVEA     DTA(40)       JRN              10
004700960113     C                   MOVEA     DTA(107)      JRNLIB           10
004800000000     C* Processing of files being journaled
004900000000     C     FILTG1        TAG
005000000000     C                   READ      TEMP132                                H2
005100000000     C   H2              GOTO      BADEND
005200021116     C                   MOVEA     DTA(40)       TST8
005300960115     C     TST8          CABEQ     '* * * * '    ENDPGM
005400000000     C* If only RCVR output is needed, bypass until receiver title
005500000000     C     ONLYRC        IFEQ      '*YES'
005600000000     C                   MOVEA     DTA(2)        TST6
005700000000     C                   MOVEA     DTA(16)       TST8
005800921113     C*          TST6      IFEQ 'Total '                   If Total
005900921113     C*          TST8      ANDEQ'receiver'                 If receiver
006000000000     C                   GOTO      RCVTG1
006100921113     C*                    END                             If Total
006200000000     C     TST6          IFEQ      'TOTAL '
006300000000     C     TST8          ANDEQ     'RECEIVER'
006400000000     C                   GOTO      RCVTG1
006500000000     C                   END
006600000000     C                   GOTO      FILTG1
006700000000     C                   END
006800000000     C* If title is access paths, goto access path processing
006900000000     C                   MOVEA     DTA(2)        TST6              6
007000000000     C                   MOVEA     DTA(37)       TST5              5
007100000000     C     TST6          IFEQ      'Number'
007200000000     C     TST5          ANDEQ     'paths'
007300000000     C                   GOTO      ACPTG1
007400000000     C                   END
007500000000     C     TST6          IFEQ      'NUMBER'
007600000000     C     TST5          ANDEQ     'PATHS'
007700000000     C                   GOTO      ACPTG1
007800000000     C                   END
007900000000     C*    Check for 'Access' in title also
008000000000     C                   MOVEA     DTA(50)       TST6
008100000000     C     TST6          CABEQ     'Access'      ACPTG1
008200000000     C     TST6          CABEQ     'ACCESS'      ACPTG1
008300000000     C* Check for col head for files being journaled
008400000000     C                   MOVEA     DTA(2)        TST4              4
008500000000     C                   MOVEA     DTA(13)       TST7              7
008600000000     C     TST4          IFEQ      'File'
008700000000     C     TST7          ANDEQ     'Library'
008800000000     C                   GOTO      FILTG2
008900000000     C                   END
009000000000     C     TST4          IFEQ      'FILE'
009100000000     C     TST7          ANDEQ     'LIBRARY'
009200000000     C                   GOTO      FILTG2
009300000000     C                   END
009400000000     C                   GOTO      FILTG1
009500000000     C* Check for files being journaled
009600000000     C     FILTG2        TAG
009700000000     C                   READ      TEMP132                                H3
009800000000     C   H3              GOTO      BADEND
009900021116     C                   MOVEA     DTA(40)       TST8
010000960115     C     TST8          CABEQ     '* * * * '    ENDPGM
010100000000     C* If no files being journaled, check for access paths
010200000000     C                   MOVEA     DTA(4)        TST7
010300000000     C                   MOVEA     DTA(18)       TST8              8
010400000000     C     TST7          IFEQ      '(No jou'
010500000000     C     TST8          ANDEQ     'files)  '
010600000000     C                   MOVE      'NONJRNLD'    RTNCDE
010700000000     C                   GOTO      ACPTG1
010800000000     C                   END
010900000000     C     TST7          IFEQ      '(NO JOU'
011000000000     C     TST8          ANDEQ     'FILES)  '
011100000000     C                   MOVE      'NONJRNLD'    RTNCDE
011200000000     C                   GOTO      ACPTG1
011300000000     C                   END
011400980827     C* If pos 2-5 is 5769, it is overflow
011500960113     C                   MOVEA     DTA(1)        TST4              4
011600021116     C     TST4          CABEQ     '5722'        FILTG1
011700960115     C                   MOVE      *BLANK        TST4
011800960115     C                   MOVEA     DTA(121)      TST4
011900960115     C     TST4          CABEQ     'Pag.'        FILTG1
012000000000     C* If pos 12 is non-blank, it is end of files
012100000000     C                   MOVEA     DTA(12)       TST1              1
012200000000     C     TST1          CABNE     *BLANK        FILTG1
012300000000     C* Extract file names in subroutine
012400000000     C                   EXSR      MOVFIL
012500000000     C                   GOTO      FILTG2
012600000000     C* Processing in access paths
012700000000     C     ACPTG1        TAG
012800000000     C                   READ      TEMP132                                H4
012900000000     C   H4              GOTO      BADEND
013000000000     C* If title is receivers, goto receiver processing
013100000000     C                   MOVEA     DTA(2)        TST6
013200000000     C                   MOVEA     DTA(16)       TST8
013300000000     C     TST6          IFEQ      'Total '
013400000000     C     TST8          ANDEQ     'receiver'
013500000000     C                   GOTO      RCVTG1
013600000000     C                   END
013700000000     C     TST6          IFEQ      'TOTAL '
013800000000     C     TST8          ANDEQ     'RECEIVER'
013900000000     C                   GOTO      RCVTG1
014000000000     C                   END
014100000000     C* Check for col head of access paths being journaled
014200000000     C                   MOVEA     DTA(2)        TST4              4
014300000000     C                   MOVEA     DTA(13)       TST7              7
014400000000     C     TST4          IFEQ      'File'
014500000000     C     TST7          ANDEQ     'Library'
014600000000     C                   GOTO      ACPTG2
014700000000     C                   END
014800000000     C     TST4          IFEQ      'FILE'
014900000000     C     TST7          ANDEQ     'LIBRARY'
015000000000     C                   GOTO      ACPTG2
015100000000     C                   END
015200000000     C                   GOTO      ACPTG1
015300000000     C* Check for access paths being journaled
015400000000     C     ACPTG2        TAG
015500000000     C                   READ      TEMP132                                H5
015600000000     C   H5              GOTO      BADEND
015700000000     C* If no access paths being journaled, goto receiver processing
015800000000     C                   MOVEA     DTA(4)        TST7
015900000000     C                   MOVEA     DTA(25)       TST8
016000000000     C     TST7          IFEQ      '(No jou'
016100000000     C     TST8          ANDEQ     'paths)  '
016200000000     C                   GOTO      RCVTG1
016300000000     C                   END
016400000000     C     TST7          IFEQ      '(NO JOU'
016500000000     C     TST8          ANDEQ     'PATHS)  '
016600000000     C                   GOTO      RCVTG1
016700000000     C                   END
016800000000     C* If pos 12 is non-blank, it is end of files or overflow
016900000000     C                   MOVEA     DTA(12)       TST1
017000000000     C     TST1          CABNE     *BLANK        ACPTG1
017100000000     C                   SETON                                        51
017200000000     C* Extract file names in subroutine
017300000000     C                   EXSR      MOVFIL
017400000000     C                   GOTO      ACPTG2
017500000000     C* Check for column heading for receivers
017600000000     C     RCVTG1        TAG
017700000000     C                   READ      TEMP132                                H6
017800000000     C   H6              GOTO      BADEND
017900021116     C                   MOVEA     DTA(40)       TST8
018000960115     C     TST8          CABEQ     '* * * * '    ENDPGM
018100960113     C                   MOVEA     DTA(2)        TST6
018200960113     C                   MOVEA     DTA(13)       TST8
018300921113     C     TST6          IFEQ      'Numero'
018400921113     C     TST8          ANDEQ     'Ricevito'
018500000000     C                   GOTO      RCVTG2
018600000000     C                   END
018700921113     C     TST6          IFEQ      'NUMERO'
018800921113     C     TST8          ANDEQ     'RICEVITO'
018900000000     C                   GOTO      RCVTG2
019000000000     C                   END
019100000000     C                   GOTO      RCVTG1
019200000000     C* Check for receivers
019300000000     C     RCVTG2        TAG
019400000000     C                   READ      TEMP132                                H7
019500000000     C   H7              GOTO      BADEND
019600021116     C                   MOVEA     DTA(40)       TST8
019700000000     C     TST8          CABEQ     '* * * * '    ENDPGM
019800960113     C                   MOVEA     DTA(1)        TST4              4
019900021116     C     TST4          CABEQ     '5722'        RCVTG1
020000960115     C                   MOVE      *BLANK        TST4
020100960115     C                   MOVEA     DTA(121)      TST4
020200960115     C     TST4          CABEQ     'Pag.'        RCVTG1
020300980828     C                   MOVEA     DTA(2)        JRRCVN
020400980828     C     JRRCVN        IFLE      *ZERO
020500980828     C                   GOTO      RCVTG1
020600980828     C                   ENDIF
020700000000     C* Must be a journal receiver, move the fields
020800960111     C                   MOVEA     DTA(2)        JRRCVN
020900960111     C                   MOVEA     DTA(13)       JRRCV
021000960111     C                   MOVEA     DTA(28)       JRRCVL
021100021116     C                   MOVEA     DTA(43)       DD
021200021116     C                   MOVEA     DTA(46)       MM
021300960111     C                   MOVEA     DTA(49)       YY
021400000000     C                   MOVE      DATE          JRATTD
021500021116     C                   MOVEA     DTA(56)       DD
021600021116     C                   MOVEA     DTA(59)       MM
021700960111     C                   MOVEA     DTA(62)       YY
021800000000     C                   MOVE      DATE          JRSAVD
021900960111     C                   MOVEA     DTA(69)       JRSTS
022000960111     C                   MOVEA     DTA(92)       TST10            10
022100000000     C                   MOVE      TST10         JRSIZE
022200000000     C                   MOVE      SYSNAM        JRSYS
022300000000     C                   MOVE      JRN           JRJRN
022400000000     C                   MOVE      JRNLIB        JRJRNL
022500000000     C                   MOVE      CVTDAT        JRCVTD
022600000000     C                   MOVE      CVTTIM        JRCVTT
022700000000     C                   WRITE     JRNRCVR
022800000000     C                   GOTO      RCVTG2
022900000000     C* End of program routine
023000000000     C     ENDPGM        TAG
023100000000     C     RTNCDE        IFEQ      *BLANK
023200000000     C                   MOVE      'GOOD    '    RTNCDE
023300000000     C                   END
023400000000     C                   SETON                                        LR
023500000000     C     BADEND        TAG
023600000000     C                   RETURN
023700000000     C* Process file information (5 records per print line)
023800000000     C     MOVFIL        BEGSR
023900000000     C                   MOVEA     DTA(2)        TST10
024000000000     C     TST10         IFNE      *BLANK
024100000000     C                   MOVE      TST10         FILE             10
024200000000     C                   MOVEA     DTA(13)       FILELB           10
024300000000     C                   EXSR      OUTPUT
024400000000     C                   END
024500000000     C                   MOVEA     DTA(27)       TST10
024600000000     C     TST10         IFNE      *BLANK
024700000000     C                   MOVE      TST10         FILE
024800000000     C                   MOVEA     DTA(38)       FILELB
024900000000     C                   EXSR      OUTPUT
025000000000     C                   END
025100000000     C                   MOVEA     DTA(52)       TST10
025200000000     C     TST10         IFNE      *BLANK
025300000000     C                   MOVE      TST10         FILE
025400000000     C                   MOVEA     DTA(63)       FILELB
025500000000     C                   EXSR      OUTPUT
025600000000     C                   END
025700000000     C                   MOVEA     DTA(77)       TST10
025800000000     C     TST10         IFNE      *BLANK
025900000000     C                   MOVE      TST10         FILE
026000000000     C                   MOVEA     DTA(88)       FILELB
026100000000     C                   EXSR      OUTPUT
026200000000     C                   END
026300000000     C                   MOVEA     DTA(102)      TST10
026400000000     C     TST10         IFNE      *BLANK
026500000000     C                   MOVE      TST10         FILE
026600000000     C                   MOVEA     DTA(113)      FILELB
026700000000     C                   EXSR      OUTPUT
026800000000     C                   END
026900000000     C                   ENDSR
027000000000     C     OUTPUT        BEGSR
027100000000     C  N51              DO
027200000000     C* Call TAAJRNC4 to access information from DSPFD using RTVPFA
027300000000     C                   MOVEL     'TAAJRNAC'    PGMNAM           10
027400000000     C                   MOVE      '4 '          PGMNAM
027500000000     C                   CALL      PGMNAM
027600000000     C                   PARM                    FILE
027700000000     C                   PARM                    FILELB
027800000000     C                   PARM                    JFIMG
027900000000     C                   PARM                    JFOMTE
028000000000     C                   PARM                    JRNDAT            6
028100000000     C                   PARM                    JFJRNT
028200000000     C                   MOVEL     JRNDAT        JFJRND
028300000000     C* Add a record per file
028400000000     C                   MOVE      SYSNAM        JFSYS
028500000000     C                   MOVE      JRN           JFJRN
028600000000     C                   MOVE      JRNLIB        JFJRNL
028700000000     C                   MOVE      CVTDAT        JFCVTD
028800000000     C                   MOVE      CVTTIM        JFCVTT
028900000000     C                   MOVE      FILE          JFFIL
029000000000     C                   MOVE      FILELB        JFFILL
029100000000     C                   WRITE     JRNFILR
029200000000     C                   END
029300000000     C   51              DO
029400000000     C                   MOVE      SYSNAM        JASYS
029500000000     C                   MOVE      JRN           JAJRN
029600000000     C                   MOVE      JRNLIB        JAJRNL
029700000000     C                   MOVE      CVTDAT        JACVTD
029800000000     C                   MOVE      CVTTIM        JACVTT
029900000000     C                   MOVE      FILE          JAFIL
030000000000     C                   MOVE      FILELB        JAFILL
030100000000     C                   WRITE     JRNACPR
030200000000     C                   END
030300000000     C                   ENDSR
