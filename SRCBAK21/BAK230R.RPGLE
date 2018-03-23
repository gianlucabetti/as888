000001021111     Hoption(*SRCSTMT : *NODEBUGIO)
000100021111     FBAK230D   CF   E             WORKSTN
000200021111     F                                     SFILE(BAK230S1:S1NRR)
000300021111     FBAK230F   O    E             DISK    USROPN
000400021111     FBAK230P   O    E             PRINTER OFLIND(*IN55)
000500021111     F*                                    USROPN
000600020909      *---------------------------------------------------------------*
000700020909     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
000800020910     D CM1             S             80    DIM(1) CTDATA PERRCD(1)
000900020909      *---------------------------------------------------------------*
001000020910     D                SDS
001100020910      * Nome del programma
001200020910     D  W0XPGM                 1     10
001300020910      * Nome dell'utente effettivo
001400020910     D  USNAM                254    263
001500020909     D                 DS
001600020909     D  USNAME                 1     10
001700020909     D  USLIBR                11     20
001800020909     D  USRSPC                 1     20
001900020909     D                 DS
002000020909     D  NMOUTQ                 1     10
002100020909     D  LBOUTQ                11     20
002200020909     D  OUTLIB                 1     20
002300020909     D                 DS
002400020909     D  USFMT                  1     10
002500020909     D  USLEN                 11     14B 0
002600020909     D  USINZ                 15     15
002700020909     D  USAUT                 16     25
002800020909     D  USTXT                 26     75
002900020909     D                 DS
003000020909     D  LENDTA                 1      4B 0
003100020909     D  STRPOS                 5      8B 0
003200020909     D  SPALEN                 9     12B 0
003300020909     D  SPASNR                13     16B 0
003400020909     D RCVPRI          DS
003500020909     D  PRISET                 1      4B 0
003600020909     D  PRINUM                 9     12B 0
003700020909     D  PRISIZ                13     16B 0
003800020909     D RCVDAT          DS
003900020909     D  DATUSR                 1     10
004000020909     D  DATOUT                11     30
004100020909     D  DATOUN                11     20
004200020909     D  DATOUL                21     30
004300020909     D  DATFRM                31     40
004400020909     D  DATUSE                41     50
004500020909     D  DATJID                51     66
004600020909     D  DATSPL                67     82
004700020909     D RCVVAR          DS
004800020909     D  VARTRT                 1      4B 0
004900020909     D  VARVAL                 5      8B 0
005000020909     D  VARJOB                 9     24
005100020909     D  VARSPL                25     40
005200020909     D  VARJNM                41     50
005300020909     D  VARUSN                51     60
005400020909     D  VARJNR                61     66
005500020909     D  VARFIL                67     76
005600020909     D  VARFNR                77     80B 0
005700020909     D  VARTYP                81     90
005800020909     D  VARDTA                91    100
005900020909     D  VARSTS               101    110
006000020909     D  VARAVL               111    120
006100020909     D  VARHDL               121    130
006200020909     D  VARSAV               131    140
006300020909     D  VARTPG               141    144B 0
006400020909     D  VARWRT               145    148B 0
006500020909     D  VARPAG               149    152B 0
006600020909     D  VAREND               153    156B 0
006700020909     D  VARLAS               157    160B 0
006800020909     D  VARRES               161    164B 0
006900020909     D  VARCPY               165    168B 0
007000020909     D  VARLFT               169    172B 0
007100020909     D  VARLPI               173    176B 0
007200020909     D  VARCPI               177    180B 0
007300020909     D  VARPRI               181    182
007400020909     D  VARQNM               183    192
007500020909     D  OUTQ3                183    185
007600021014     D  OUTQNM               183    192
007700020909     D  VARQLB               193    202
007800021014     D  VARFOP               204    209
007900020909     D  VARTIM               210    215
008000020909     D  VARFNA               216    225
008100020909     D  VARFLB               226    235
008200020909     D  VAROPF               236    245
008300020909     D  VAROPL               246    255
008400020909     D  VARCOD               256    270
008500020909     D  VARTXT               271    300
008600020909     D  VARRCD               301    304B 0
008700020909     D  VARMAX               305    308B 0
008800020909     D  VARCLS               309    318
008900020909     D  VARPRT               319    328
009000020909     D  VARNAM               329    340
009100020909     D  VARFLD               341    404
009200020909     D  VARS36               405    412
009300020909     D  VARFID               413    422
009400020909     D  VARLUN               423    423
009500020909     D  VARCHR               424    424
009600020909     D  VARLEN               425    428B 0
009700020909     D  VARWID               429    432B 0
009800020909     D  VARSEP               433    436B 0
009900020909     D  VARLIN               437    440B 0
010000020909     D  VARSDA               441    450
010100020909     D  VARSEC               451    460
010200020909     D  VARSSO               461    470
010300020909     D  VARSCR               471    480
010400020909     D  VARSCI               481    484B 0
010500020909     D  VARPHI               485    494
010600020909     D  VARPGG               495    504
010700020909     D  VARFFA               505    514
010800020909     D  VARFLI               515    524
010900020909     D  VARDRW               525    528B 0
011000020909     D  VARFON               529    538
011100020909     D  VARSSP               539    544
011200020909     D  VARROT               545    548B 0
011300020909     D  VARJUS               549    552B 0
011400020909     D  VARBOT               553    562
011500020909     D  VARRCC               563    572
011600020909     D  VARCCH               573    582
011700020909     D  VARALG               583    592
011800020909     D  VARQUA               593    602
011900020909     D  VARFED               603    612
012000020909     D  VARVOL               613    683
012100020909     D  VARBID               684    700
012200020909     D  VAREXC               701    710
012300020909     D  VARCCD               711    720
012400020909     D  VARTRC               721    724B 0
012500020909      *
012600020909     D                 DS
012700020909     D  DATGMA                 1      6
012800020909     D  GMAGG                  1      2
012900020909     D  GMAMM                  3      4
013000020909     D  GMAAA                  5      6
013100020909     D                 DS
013200020909     D  DATAMG                 1      6
013300020909     D  AMGAA                  1      2
013400020909     D  AMGMM                  3      4
013500020909     D  AMGGG                  5      6
013600021014     D USRPAR          DS
013700021014     D  USRNUM                 1      2B 0
013800021014     D  USRSPL                 3     12
013900021017     D  USR                    3   3002
014000021017     D                                     DIM(300)
014100021104     D $c5s0           S              5s 0
014200020909      *---------------------------------------------------------------*
014300970929     C     *ENTRY        PLIST
014400021014     C                   PARM                    USRPAR
014500021014     C                   PARM                    FILLIB           20
014600021014     C                   PARM                    OMTSYS            4
014700021014     C                   PARM                    OUTPUT            8
014800020910      *
014900020910     C     USRSPL        IFEQ      '*CURRENT'
015000020910     C                   MOVEL     USNAM         USRSPL
015100020910     C                   SETON                                        88
015200020910     C                   ENDIF
015300021014      *
015400020909     C                   EXSR      DLTOUT
015500020909     C                   MOVE      FILLIB        LIBOTQ           10
015600020909     C                   MOVEL     FILLIB        INOUTQ           10
015601021111     C     INOUTQ        IFEQ      '*ALL'
015602021111     C                   MOVE      *BLANKS       LIBOTQ
015603021111     C                   ENDIF
015604021111     C     INOUTQ        IFNE      '*ALL'
015605021111     C     LIBOTQ        ANDEQ     ' '
015606021111     C                   MOVEL     '*LIBL'       LIBOTQ
015607021111     C                   ENDIF
015608021111     C*
015700971020     C     OMTSYS        IFEQ      '*YES'
015800971020     C                   SETON                                        88
015900971020     C                   ENDIF
016000971007     C     OUTPUT        IFEQ      '*OUTFILE'
016100020910     C                   EXSR      CLRPFM
016200021111     C                   OPEN      BAK230F
016300971007     C                   ENDIF
016400971007     C     OUTPUT        IFEQ      '*PRINT'
016500021111     C                   OPEN      BAK230P
016600971010     C                   SETON                                        55
016700971007     C                   ENDIF
016800910722     C*--------------------------------------------
016900970929     C* IMPOSTA I DEFAULT
017000910722     C*--------------------------------------------
017100971016     C     INIZIO        TAG
017200910702     C                   MOVEL     '*ALL'        USER
017300970929     C                   MOVEL     '*ALL'        OUTQ
017400910702     C                   MOVEL     '*ALL'        FORM
017500910702     C                   MOVEL     '*ALL'        DATA
017600910702     C                   SETOFF                                         9091
017700021111     C                   WRITE     BAK230C1
017800910702     C                   SETON                                          9091
017900910702     C                   Z-ADD     0             S1NRR             5 0
018000020909     C*
018100020909     C                   MOVEL     'USSPOOL'     USNAME
018200020909     C                   MOVEL     'QTEMP'       USLIBR
018300020909     C*
018400020909     C* CREA USER-SPACE (USSPOOL IN QTEMP)
018500020909     C                   Z-ADD     00100         USLEN
018600020909     C                   MOVEL     '*ALL'        USAUT
018700020909     C                   MOVEL     'SPOOL'       USTXT
018800020909     C                   CALL      'QUSCRTUS'
018900020909     C                   PARM                    USRSPC
019000020909     C                   PARM                    USFMT
019100020909     C                   PARM                    USLEN
019200020909     C                   PARM                    USINZ
019300020909     C                   PARM                    USAUT
019400020909     C                   PARM                    USTXT
019500020909      *
019600021014     C     1             DO        USRNUM        X                 5 0
019700020909     C* ESEGUE LISTA SPOOL-FILES IN USER-SPACE
019800020909     C                   MOVEL     'SPLF0100'    RECFOR            8
019900021014     C                   MOVEL     USR(X)        UTENTE           10
020000020909     C                   MOVEL     INOUTQ        NMOUTQ
020100020909     C                   MOVEL     LIBOTQ        LBOUTQ
020200020909     C                   MOVEL     '*ALL    '    FRMTYP           10
020300020909     C                   MOVEL     '*ALL    '    USRDAT           10
020400020909     C                   CALL      'QUSLSPL'
020500020909     C                   PARM                    USRSPC
020600020909     C                   PARM                    RECFOR
020700020909     C                   PARM                    UTENTE
020800020909     C                   PARM                    OUTLIB
020900020909     C                   PARM                    FRMTYP
021000020909     C                   PARM                    USRDAT
021100020909     C*-------------------------------------------------------
021200020909     C                   Z-ADD     16            LENDTA
021300020909     C                   Z-ADD     125           STRPOS
021400020909     C* REPERIMENTO SPAZIO UTENTE INIZIALE
021500020909     C                   CALL      'QUSRTVUS'
021600020909     C                   PARM                    USRSPC
021700020909     C                   PARM                    STRPOS
021800020909     C                   PARM                    LENDTA
021900020909     C                   PARM                    RCVPRI
022000020909     C*
022100020909     C                   Z-ADD     PRISET        STRPOS
022200020909     C                   ADD       1             STRPOS
022300020909     C                   Z-ADD     PRISIZ        LENDTA
022400020909     C                   Z-ADD     1             COUNT            15 0
022500020910     C*
022600020910     C                   EXSR      CICLO
022700020909     C*
022800021014     C                   END
022900971008      *****************************************************************
023000020910     C                   SELECT
023100020910     C     OUTPUT        WHENEQ    '*'
023200020910     C                   EXSR      GESVID
023300020910     C     OUTPUT        WHENEQ    '*OUTFILE'
023400021111     C                   CLOSE     BAK230F
023500020910     C     OUTPUT        WHENEQ    '*PRINT'
023600971010     C                   WRITE     PIEDE
023700021111     C                   CLOSE     BAK230P
023800020910     C                   ENDSL
023900020910      *
024000910702     C     FINE          TAG
024100020909     C                   EXSR      DLTOUT
024200910702     C                   SETON                                          LR
024300020910      *---------------------------------------------------------------*
024400020910      *  ELIMINA LO SPAZIO UTENTE DALLA QTEMP
024500020910      *---------------------------------------------------------------*
024600020909     C     DLTOUT        BEGSR
024700020910      *
024800020909     C                   CALL      'QCMDEXC'                            93
024900020909     C                   PARM                    CMD
025000020909     C                   PARM      80            LEN              15 5
025100020910      *
025200020909     C                   ENDSR
025300020910      *---------------------------------------------------------------*
025400020910      *  SVUOTA OUTFILE
025500020910      *---------------------------------------------------------------*
025600020910     C     CLRPFM        BEGSR
025700020910      *
025800020910     C                   CALL      'QCMDEXC'
025900020910     C                   PARM                    CM1
026000020910     C                   PARM      80            LEN              15 5
026100020910      *
026200020910     C                   ENDSR
026300020910      *---------------------------------------------------------------*
026400020910      *  OUTPUT A VIDEO
026500020910      *---------------------------------------------------------------*
026600020910     C     VIDEO         BEGSR
026700020910      *
026800020910     C                   ADD       1             S1NRR
026900020910     C                   MOVEL     VARSTS        S1STAT
027000020910     C                   Z-ADD     VARTPG        S1TOTP
027100020910     C                   MOVE      VARFNR        FILN
027200020910     C                   MOVE      VARJNR        JOBNUM
027300021017     C                   MOVE      VARFOP        DATAMG
027400021017     C                   MOVE      AMGGG         GMAGG
027500021017     C                   MOVE      AMGMM         GMAMM
027600021017     C                   MOVE      AMGAA         GMAAA
027700021017     C                   MOVE      DATGMA        DATAVV
027800021017     C                   MOVE      VARTIM        VARTVV
027900021104     C                   if        not *in77
028000021104     C                   eval      $rows = %editc(datavv:'Y')
028100021104     C                                     + ' ' +
028200021104     C                                     %editw(vartvv:'  :  :  ')
028300021104     C                   else
028400021104     C                   z-add     varcpy        $c5s0
028500021104     C                   eval      $rows = vartyp
028600021104     C                                     + ' ' +
028700021104     C                                     %editc($c5s0:'Z')
028800021104     C                   endif
028900021111     C                   WRITE     BAK230S1
029000020910      *
029100020910     C                   ENDSR
029200020910      *---------------------------------------------------------------*
029300020910      *  OUTPUT SU FILE
029400020910      *---------------------------------------------------------------*
029500020910     C     FILE          BEGSR
029600020910      *
029700020910     C                   MOVEL     VARSTS        S1STAT
029800020910     C                   Z-ADD     VARTPG        S1TOTP
029900020910     C                   WRITE     BIG
030000020910      *
030100020910     C                   ENDSR
030200020910      *---------------------------------------------------------------*
030300020910      *  OUTPUT SU STAMPANTE
030400020910      *---------------------------------------------------------------*
030500020910     C     PRINT         BEGSR
030600020910      *
030700020910     C                   MOVEL     VARSTS        S1STAT
030800020910     C                   Z-ADD     VARTPG        S1TOTP
030900021014     C                   MOVE      VARFOP        DATAMG
031000021014     C                   MOVE      AMGGG         GMAGG
031100021014     C                   MOVE      AMGMM         GMAMM
031200021014     C                   MOVE      AMGAA         GMAAA
031300021014     C                   MOVE      DATGMA        DATAPR
031400021014     C                   MOVE      VARTIM        TIMEPR
031500020910     C   55              WRITE     TESTA1
031600020910     C                   SETOFF                                       55
031700020910     C                   WRITE     SPL
031800020910      *
031900020910     C                   ENDSR
032000020910      *---------------------------------------------------------------*
032100020910      *  LETTURA USER-SPACE
032200020910      *---------------------------------------------------------------*
032300020910     C     CICLO         BEGSR
032400020910      *
032500020910     C     COUNT         DOWLE     PRINUM
032600020910      * REPERIMENTO SPAZIO UTENTE
032700020910     C                   CALL      'QUSRTVUS'
032800020910     C                   PARM                    USRSPC
032900020910     C                   PARM                    STRPOS
033000020910     C                   PARM                    LENDTA
033100020910     C                   PARM                    RCVDAT
033200020910      * REPERIMENTO  SPOOL-FILES-ATTRUBUTES
033300020910     C                   Z-ADD     724           SPALEN
033400020910     C                   MOVEL     'SPLA0100'    SPAIDR            8
033500020910     C                   MOVEL     '*INT'        SPAQJN           26
033600020910     C                   MOVEL     DATJID        SPAIJN           16
033700020910     C                   MOVEL     DATSPL        SPASPL           16
033800020910     C                   MOVEL     '*INT'        SPASNM           10
033900020910     C                   Z-ADD     0             SPASNR
034000020910     C                   CALL      'QUSRSPLA'
034100020910     C                   PARM                    RCVVAR
034200020910     C                   PARM                    SPALEN
034300020910     C                   PARM                    SPAIDR
034400020910     C                   PARM                    SPAQJN
034500020910     C                   PARM                    SPAIJN
034600020910     C                   PARM                    SPASPL
034700020910     C                   PARM                    SPASNM
034800020910     C                   PARM                    SPASNR
034900020910      *
035000020910     C                   ADD       1             COUNT
035100020910     C                   ADD       PRISIZ        STRPOS
035200020910      *  TESTA SE IL RECORD E' ELABORABILE:
035300020910     C                   SETOFF                                       21
035400020910      *
035500020910     C   88OUTQ3         IFEQ      'QEZ'
035600020910     C                   SETON                                        21
035700020910     C                   ENDIF
035800020910      *
035900020910     C     *IN21         IFEQ      *OFF
036000020910     C                   SELECT
036100020910     C     OUTPUT        WHENEQ    '*'
036200020910     C                   EXSR      VIDEO
036300020910     C     OUTPUT        WHENEQ    '*OUTFILE'
036400020910     C                   EXSR      FILE
036500020910     C     OUTPUT        WHENEQ    '*PRINT'
036600020910     C                   EXSR      PRINT
036700020910     C                   ENDSL
036800020910     C                   ENDIF
036900020910     C                   ENDDO
037000020910      *
037100020910     C                   ENDSR
037200020910      *---------------------------------------------------------------*
037300020910      *  GESTIONE DEL VIDEO
037400020910      *---------------------------------------------------------------*
037500020910     C     GESVID        BEGSR
037600020910      *
037700020910     C     S1NRR         IFEQ      0
037800020910     C                   ADD       1             S1NRR
037900020910     C                   Z-ADD     *ZEROS        S1TOTP
038000020910     C                   MOVEL(P)  '*'           VARFIL
038100020910     C                   MOVEL(P)  '*'           VARUSN
038200020910     C                   MOVEL(P)  '*'           OUTQNM
038300020910     C                   MOVEL(P)  '*'           VARDTA
038400020910     C                   MOVEL(P)  '*'           S1STAT
038500021111     C                   WRITE     BAK230S1
038600020910     C                   END
038700020910      *
038800020910     C                   DO        *HIVAL
038900021111     C                   WRITE     BAK230P1
039000020910     C     C1RCD         IFEQ      0
039100020910     C                   Z-ADD     1             C1RCD
039200020910     C                   ENDIF
039300021111     C                   EXFMT     BAK230C1
039400020910     C     *IN03         IFEQ      *ON
039500020910     C     *IN12         OREQ      *ON
039600020910     C                   LEAVE
039700020910     C                   ENDIF
039800021104      * swap modalità visualizzazione
039900021104      * n77 = dataora 77=Modulo/Copie
040000021104     C                   if        *in11
040100021104     C                   eval      *in77=not *in77
040200021104     C                   EXSR      DLTOUT
040300021104     C                   Z-ADD     1             C1RCD
040400021104     C                   GOTO      INIZIO
040500021104     C                   endif
040600021104      *
040700021104     C     *IN05         IFEQ      *ON
040800020910     C                   EXSR      DLTOUT
040900021017     C                   Z-ADD     1             C1RCD
041000020910     C                   GOTO      INIZIO
041100020910     C                   ENDIF
041101021111      *
041102021111     C     ancora        tag
041200021111     C                   READC     BAK230S1                               95
041300020910     C     *IN95         IFEQ      *OFF
041400020910     C                   Z-ADD     S1NRR         C1RCD
041500021111     C                   CALL      'BAK231C'
041600020910     C                   PARM                    S1SCE
041700020910     C                   PARM                    VARFIL
041800020910     C                   PARM                    VARJNM
041900020910     C                   PARM                    VARUSN
042000020910     C                   PARM                    JOBNUM
042100020910     C                   PARM                    FILN
042200020910     C                   PARM                    S1STAT
042300021111     C                   MOVE      S1SCE         s1old             1
042301021111     C                   MOVE      *BLANK        S1SCE
042400021111     C                   UPDATE    BAK230S1
042506021111      *
042507021111     c     *in06         ifeq      *ON
042508021111     c     S1old         andeq     '4'
042509021111     C                   goto      ancora
042510021111     C                   end
042511021111      *
042512021111     C                   ENDIF
042600020910     C                   ENDDO
042700020910      *
042800020910     C                   ENDSR
042900020909      *---------------------------------------------------------------*
043000020910**
043100020909DLTUSRSPC  USRSPC(QTEMP/USSPOOL)
043200020910**
043300021111CLRPFM BAK230F
