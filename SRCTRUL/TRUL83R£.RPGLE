000100040202     FTRUL83D   CF   E             WORKSTN
000200040202     F                                     SFILE(UL83S1:S1NRR)
000300020909      *---------------------------------------------------------------*
000400020909     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
000500020909      *---------------------------------------------------------------*
000600020910     D                SDS
000700020910      * Nome del programma
000800020910     D  W0XPGM                 1     10
000900020910      * Nome dell'utente effettivo
001000020910     D  USNAM                254    263
001100020909     D                 DS
001200020909     D  USNAME                 1     10
001300020909     D  USLIBR                11     20
001400020909     D  USRSPC                 1     20
001500020909     D                 DS
001600020909     D  NMOUTQ                 1     10
001700020909     D  LBOUTQ                11     20
001800020909     D  OUTLIB                 1     20
001900020909     D                 DS
002000020909     D  USFMT                  1     10
002100020909     D  USLEN                 11     14B 0
002200020909     D  USINZ                 15     15
002300020909     D  USAUT                 16     25
002400020909     D  USTXT                 26     75
002500020909     D                 DS
002600020909     D  LENDTA                 1      4B 0
002700020909     D  STRPOS                 5      8B 0
002800020909     D  SPALEN                 9     12B 0
002900020909     D  SPASNR                13     16B 0
003000020909     D RCVPRI          DS
003100020909     D  PRISET                 1      4B 0
003200020909     D  PRINUM                 9     12B 0
003300020909     D  PRISIZ                13     16B 0
003400020909     D RCVDAT          DS
003500020909     D  DATUSR                 1     10
003600020909     D  DATOUT                11     30
003700020909     D  DATOUN                11     20
003800020909     D  DATOUL                21     30
003900020909     D  DATFRM                31     40
004000020909     D  DATUSE                41     50
004100020909     D  DATJID                51     66
004200020909     D  DATSPL                67     82
004300020909     D RCVVAR          DS
004400020909     D  VARTRT                 1      4B 0
004500020909     D  VARVAL                 5      8B 0
004600020909     D  VARJOB                 9     24
004700020909     D  VARSPL                25     40
004800020909     D  VARJNM                41     50
004900020909     D  VARUSN                51     60
005000020909     D  VARJNR                61     66
005100020909     D  VARFIL                67     76
005200020909     D  VARFNR                77     80B 0
005300020909     D  VARTYP                81     90
005400020909     D  VARDTA                91    100
005500020909     D  VARSTS               101    110
005600020909     D  VARAVL               111    120
005700020909     D  VARHDL               121    130
005800020909     D  VARSAV               131    140
005900020909     D  VARTPG               141    144B 0
006000020909     D  VARWRT               145    148B 0
006100020909     D  VARPAG               149    152B 0
006200020909     D  VAREND               153    156B 0
006300020909     D  VARLAS               157    160B 0
006400020909     D  VARRES               161    164B 0
006500020909     D  VARCPY               165    168B 0
006600020909     D  VARLFT               169    172B 0
006700020909     D  VARLPI               173    176B 0
006800020909     D  VARCPI               177    180B 0
006900020909     D  VARPRI               181    182
007000020909     D  VARQNM               183    192
007100020909     D  OUTQ3                183    185
007200021014     D  OUTQNM               183    192
007300020909     D  VARQLB               193    202
007400021014     D  VARFOP               204    209
007500020909     D  VARTIM               210    215
007600020909     D  VARFNA               216    225
007700020909     D  VARFLB               226    235
007800020909     D  VAROPF               236    245
007900020909     D  VAROPL               246    255
008000020909     D  VARCOD               256    270
008100020909     D  VARTXT               271    300
008200020909     D  VARRCD               301    304B 0
008300020909     D  VARMAX               305    308B 0
008400020909     D  VARCLS               309    318
008500020909     D  VARPRT               319    328
008600020909     D  VARNAM               329    340
008700020909     D  VARFLD               341    404
008800020909     D  VARS36               405    412
008900020909     D  VARFID               413    422
009000020909     D  VARLUN               423    423
009100020909     D  VARCHR               424    424
009200020909     D  VARLEN               425    428B 0
009300020909     D  VARWID               429    432B 0
009400020909     D  VARSEP               433    436B 0
009500020909     D  VARLIN               437    440B 0
009600020909     D  VARSDA               441    450
009700020909     D  VARSEC               451    460
009800020909     D  VARSSO               461    470
009900020909     D  VARSCR               471    480
010000020909     D  VARSCI               481    484B 0
010100020909     D  VARPHI               485    494
010200020909     D  VARPGG               495    504
010300020909     D  VARFFA               505    514
010400020909     D  VARFLI               515    524
010500020909     D  VARDRW               525    528B 0
010600020909     D  VARFON               529    538
010700020909     D  VARSSP               539    544
010800020909     D  VARROT               545    548B 0
010900020909     D  VARJUS               549    552B 0
011000020909     D  VARBOT               553    562
011100020909     D  VARRCC               563    572
011200020909     D  VARCCH               573    582
011300020909     D  VARALG               583    592
011400020909     D  VARQUA               593    602
011500020909     D  VARFED               603    612
011600020909     D  VARVOL               613    683
011700020909     D  VARBID               684    700
011800020909     D  VAREXC               701    710
011900020909     D  VARCCD               711    720
012000020909     D  VARTRC               721    724B 0
012100020909      *
012200020909     D                 DS
012300020909     D  DATGMA                 1      6
012400020909     D  GMAGG                  1      2
012500020909     D  GMAMM                  3      4
012600020909     D  GMAAA                  5      6
012700020909     D                 DS
012800020909     D  DATAMG                 1      6
012900020909     D  AMGAA                  1      2
013000020909     D  AMGMM                  3      4
013100020909     D  AMGGG                  5      6
013200041006     D********
013300041006     D* DS X RIDEFINIZIONE PASSAGGIO SCHIERA SPOOL SELEZIONATI A VIDEO
013400041006     D********
013500041006     D DSSPOOL         DS
013600041006     D  dsVARFIL                     10
013700041006     D  dsVARJNM                     10
013800041006     D  dsVARUSN                     10
013900041006     D  dsJOBNUM                      6
014000041006     D  dsSPLNUM                      6
014100041006     D  dsVARDTA                     10
014200041006     D  dsNUMRIG                      9  0
014300041006     D  dsNUMCOL                      9  0
014400041006     D  dsNUMCPI                      9  1
014500041006     D  dpOUTQNM                     10
014600041006     D  ds$ROWS                      17
014700041006     D  dsS1TOTP                      6  0
014800041006     D  dsS1STAT                      6
014900051102     D*------------------
015000051102     D* DS REPERIMENTO LISTA SPOOL - INTERNAL SPOOL & JOB ID
015100051102     D*------------------
015200051102     D XLSTSPLDS2    E DS
015300051102     D*------------------
015400051102     D DSSPOOLID       DS
015500051102     D  Sp_Job_ID                          like(XLSTSPLDS2)
015600051102     D  Sp_Job_Codr                   3
015700041006     D********
015800041006     D* SCHIERE SPOOL SELEZIONATI A VIDEO
015900041006     D********
016000051102     D SKSPOOL         S                   like(DSSPOOL) dim(99)
016100051102     D SKSPOOLID       S                   like(DSSPOOLID) dim(99)
016200041006     D SKSPOOLMULT     S                   like(DSSPOOL) dim(99)
016300041006      *
016400021104     D $c5s0           S              5s 0
016500020909      *---------------------------------------------------------------*
016600970929     C     *ENTRY        PLIST
016700040929     C                   PARM                    p_USRDFT         10
016800040929     C                   PARM                    p_FILLIB         20
016900060810     C                   PARM                    p_FORMTYP        10
017000060810     C                   PARM                    p_USRDATA        10
017100040929     C                   PARM                    p_USRABIL         1
017200060810     C                   PARM                    p_BYPASS          1
017300060810     C                   PARM                    p_GESLIV          1
017400120312     C* Il parametri seguente rappresenta l'indirizzo e-mail BRT con cui forzare
017500120312     C* l'invio della mail, se valorizzato NN deve riportare il dominio '@brt.it'
017600040524     C* ma solo l'account (ad esempio: 'info001')
017700040929     C                   PARM                    p_USREMAIL       30
017800060810     C                   PARM                    o_VARFIL         10
017900060810     C                   PARM                    o_SPLNUM          6
018000060810     C                   PARM                    o_VARUSN         10
018100060810     C                   PARM                    o_VARJNM         10
018200060810     C                   PARM                    o_JOBNUM          6
018300060810     C                   PARM                    o_VARTYP         10
018400060810     C                   PARM                    o_VARDTA         10
018500040928      *
018600040928      * Gestisco il ricevimento parametri
018700060810     C                   IF        %parms <  15
018800060810     C                   SETON                                        44
018900040929     C                   MOVEL     *blanks       USRDFT           10
019000040929     C                   MOVEL     *blanks       FILLIB           20
019100060810     C                   MOVEL     *blanks       FORMTYP          10
019200060810     C                   MOVEL     *blanks       USRDATA          10
019300040929     C                   MOVEL     *blanks       USRABIL           1
019400060810     C                   MOVEL     *blanks       BYPASS            1
019500060810     C                   MOVEL     *blanks       GESLIV            1
019600040929     C                   MOVEL     *blanks       USREMAIL         30
019700040929     C                   ELSE
019800040929     C                   MOVEL     p_USRDFT      USRDFT           10
019900040929     C                   MOVEL     p_FILLIB      FILLIB           20
020000060810     C                   MOVEL     p_FORMTYP     FORMTYP          10
020100060810     C                   MOVEL     p_USRDATA     USRDATA          10
020200040929     C                   MOVEL     p_USRABIL     USRABIL           1
020300060810     C                   MOVEL     p_BYPASS      BYPASS            1
020400060810     C                   MOVEL     p_GESLIV      GESLIV            1
020500040929     C                   MOVEL     p_USREMAIL    USREMAIL         30
020600040928     C                   ENDIF
020700060810      *
020800060810      * Inizializzo subito i parametri d output
020900060810     C  N44              EVAL      o_VARFIL = *blanks
021000060810     C  N44              EVAL      o_SPLNUM = *blanks
021100060810     C  N44              EVAL      o_VARUSN = *blanks
021200060810     C  N44              EVAL      o_VARJNM = *blanks
021300060810     C  N44              EVAL      o_JOBNUM = *blanks
021400060810     C  N44              EVAL      o_VARTYP = *blanks
021500060810     C  N44              EVAL      o_VARDTA = *blanks
021600040203      *
021700040203      * Verifico se consentito cambio utente x la scelta degli spool
021800040203     C                   IF        USRABIL = *BLANKS
021900040203     C                   SETON                                        55
022000040203     C                   ENDIF
022100021014      *
022200020909     C                   EXSR      DLTOUT
022300040202      *
022400040202      * Verifico i parametri d input
022500040202     C     USRDFT        IFEQ      *BLANKS
022600040202     C                   MOVEL     USNAM         USRDFT
022700040202     C                   ENDIF
022800060810      *
022900060810     C     FORMTYP       IFEQ      *BLANKS
023000060810     C                   MOVEL     '*ALL'        FORMTYP
023100060810     C                   ENDIF
023200060810      *
023300060810     C     USRDATA       IFEQ      *BLANKS
023400060810     C                   MOVEL     '*ALL'        USRDATA
023500060810     C                   ENDIF
023600060810      *
023700060810     C     BYPASS        IFEQ      'S'
023800060810     C                   SETON                                        45
023900060810     C                   ENDIF
024000060810      *
024100060810     C     GESLIV        IFEQ      'S'                                          * S = solo scelta
024200060810     C                   SETON                                        46
024300060810     C                   ENDIF
024400040202      *
024500040202     C                   MOVEL     FILLIB        INOUTQ           10
024600040202     C                   MOVE      FILLIB        LIBOTQ           10
024700040202     C                   IF        INOUTQ = *BLANKS
024800040202     C                   EVAL      INOUTQ = '*ALL'
024900040202     C                   EVAL      LIBOTQ = *BLANKS
025000040202     C                   ELSE
025100040202     C                   IF        INOUTQ <> '*ALL' AND
025200040202     C                             LIBOTQ = *BLANKS
025300040202     C                   EVAL      LIBOTQ = '*LIBL'
025400040202     C                   ENDIF
025500040202     C                   ENDIF
025600040202      *
025700060810      * Se nn richiesto in ingresso by-pass delle selezioni spool =>
025800060810      * visualizzo la videata d scelta iniziale (l'utente è fisso su quello passato in passaggio parametri d entrata)
025900040520     C     SCELTA        TAG
026000040202     C                   MOVEL     USRDFT        USER
026100040202     C                   MOVEL     INOUTQ        OUTQ
026200040202     C                   MOVEL     LIBOTQ        OUTLB
026300060810     C                   MOVEL     FORMTYP       FORM
026400060810     C                   MOVEL     USRDATA       DATA
026500040520     C                   Z-ADD     *ZEROS        PAGINE
026600060810     C                   IF        *IN45 = *OFF
026700040202     C                   EXFMT     UL83D1
026800040202     C     *IN03         IFEQ      *ON
026900040202     C     *IN12         OREQ      *ON
027000040202     C                   GOTO      FINE
027100040202     C                   ENDIF
027200060810     C                   ENDIF
027300040202      *
027400040202      * Verifico i parametri immessi a video dall'utente
027500040202     C                   IF        OUTQ  = *BLANKS
027600040202     C                   EVAL      OUTQ  = '*ALL'
027700040202     C                   EVAL      OUTLB = *BLANKS
027800040202     C                   ELSE
027900040202     C                   IF        OUTQ  <> '*ALL' AND
028000040202     C                             OUTLB = *BLANKS
028100040202     C                   EVAL      OUTLB = '*LIBL'
028200040202     C                   ENDIF
028300040202     C                   ENDIF
028400910722     C*--------------------------------------------
028500970929     C* IMPOSTA I DEFAULT
028600910722     C*--------------------------------------------
028700971016     C     INIZIO        TAG
028800910702     C                   SETOFF                                         9091
028900040202     C                   WRITE     UL83C1
029000910702     C                   SETON                                          9091
029100910702     C                   Z-ADD     0             S1NRR             5 0
029200020909     C*
029300020909     C                   MOVEL     'USSPOOL'     USNAME
029400020909     C                   MOVEL     'QTEMP'       USLIBR
029500020909     C*
029600020909     C* CREA USER-SPACE (USSPOOL IN QTEMP)
029700020909     C                   Z-ADD     00100         USLEN
029800020909     C                   MOVEL     '*ALL'        USAUT
029900020909     C                   MOVEL     'SPOOL'       USTXT
030000020909     C                   CALL      'QUSCRTUS'
030100020909     C                   PARM                    USRSPC
030200020909     C                   PARM                    USFMT
030300020909     C                   PARM                    USLEN
030400020909     C                   PARM                    USINZ
030500020909     C                   PARM                    USAUT
030600020909     C                   PARM                    USTXT
030700020909      *
030800020909     C* ESEGUE LISTA SPOOL-FILES IN USER-SPACE
030900020909     C                   MOVEL     'SPLF0100'    RECFOR            8
031000040202     C                   MOVEL     USER          UTENTE           10
031100040202     C                   MOVEL     OUTQ          NMOUTQ
031200040202     C                   MOVEL     OUTLB         LBOUTQ
031300040202     C                   MOVEL     FORM          FRMTYP           10
031400040202     C                   MOVEL     DATA          USRDAT           10
031500040202     C                   CALL(e)   'QUSLSPL'
031600020909     C                   PARM                    USRSPC
031700020909     C                   PARM                    RECFOR
031800020909     C                   PARM                    UTENTE
031900020909     C                   PARM                    OUTLIB
032000020909     C                   PARM                    FRMTYP
032100020909     C                   PARM                    USRDAT
032200040202      *
032300040202      * Gestisco l'errore (probabile indicazione OUTQ e/o libreria QUTQ errati)
032400040202     C                   IF        %error
032500040202     C                   GOTO      FINE
032600040202     C                   ENDIF
032700020909     C*-------------------------------------------------------
032800020909     C                   Z-ADD     16            LENDTA
032900020909     C                   Z-ADD     125           STRPOS
033000020909     C* REPERIMENTO SPAZIO UTENTE INIZIALE
033100020909     C                   CALL      'QUSRTVUS'
033200020909     C                   PARM                    USRSPC
033300020909     C                   PARM                    STRPOS
033400020909     C                   PARM                    LENDTA
033500020909     C                   PARM                    RCVPRI
033600020909     C*
033700020909     C                   Z-ADD     PRISET        STRPOS
033800020909     C                   ADD       1             STRPOS
033900020909     C                   Z-ADD     PRISIZ        LENDTA
034000020909     C                   Z-ADD     1             COUNT            15 0
034100020910     C*
034200020910     C                   EXSR      CICLO
034300040202      *
034400020910     C                   EXSR      GESVID
034500020910      *
034600910702     C     FINE          TAG
034700020909     C                   EXSR      DLTOUT
034800910702     C                   SETON                                          LR
034900020910      *---------------------------------------------------------------*
035000020910      *  ELIMINA LO SPAZIO UTENTE DALLA QTEMP
035100020910      *---------------------------------------------------------------*
035200020909     C     DLTOUT        BEGSR
035300020910      *
035400020909     C                   CALL      'QCMDEXC'                            93
035500020909     C                   PARM                    CMD
035600020909     C                   PARM      80            LEN              15 5
035700020910      *
035800020909     C                   ENDSR
035900020910      *---------------------------------------------------------------*
036000020910      *  OUTPUT A VIDEO
036100020910      *---------------------------------------------------------------*
036200020910     C     VIDEO         BEGSR
036300020910      *
036400020910     C                   ADD       1             S1NRR
036500020910     C                   MOVEL     VARSTS        S1STAT
036600020910     C                   Z-ADD     VARTPG        S1TOTP
036700020910     C                   MOVE      VARFNR        FILN
036800020910     C                   MOVE      VARJNR        JOBNUM
036900021017     C                   MOVE      VARFOP        DATAMG
037000021017     C                   MOVE      AMGGG         GMAGG
037100021017     C                   MOVE      AMGMM         GMAMM
037200021017     C                   MOVE      AMGAA         GMAAA
037300021017     C                   MOVE      DATGMA        DATAVV
037400021017     C                   MOVE      VARTIM        VARTVV
037500021104     C                   if        not *in77
037600021104     C                   eval      $rows = %editc(datavv:'Y')
037700021104     C                                     + ' ' +
037800021104     C                                     %editw(vartvv:'  :  :  ')
037900021104     C                   else
038000021104     C                   z-add     varcpy        $c5s0
038100021104     C                   eval      $rows = vartyp
038200021104     C                                     + ' ' +
038300021104     C                                     %editc($c5s0:'Z')
038400021104     C                   endif
038500040202     C                   WRITE     UL83S1
038600020910      *
038700020910     C                   ENDSR
038800020910      *---------------------------------------------------------------*
038900020910      *  LETTURA USER-SPACE
039000020910      *---------------------------------------------------------------*
039100020910     C     CICLO         BEGSR
039200020910      *
039300020910     C     COUNT         DOWLE     PRINUM
039400020910      * REPERIMENTO SPAZIO UTENTE
039500020910     C                   CALL      'QUSRTVUS'
039600020910     C                   PARM                    USRSPC
039700020910     C                   PARM                    STRPOS
039800020910     C                   PARM                    LENDTA
039900020910     C                   PARM                    RCVDAT
040000020910      * REPERIMENTO  SPOOL-FILES-ATTRUBUTES
040100020910     C                   Z-ADD     724           SPALEN
040200020910     C                   MOVEL     'SPLA0100'    SPAIDR            8
040300020910     C                   MOVEL     '*INT'        SPAQJN           26
040400020910     C                   MOVEL     DATJID        SPAIJN           16
040500020910     C                   MOVEL     DATSPL        SPASPL           16
040600020910     C                   MOVEL     '*INT'        SPASNM           10
040700020910     C                   Z-ADD     0             SPASNR
040800020910     C                   CALL      'QUSRSPLA'
040900020910     C                   PARM                    RCVVAR
041000020910     C                   PARM                    SPALEN
041100020910     C                   PARM                    SPAIDR
041200020910     C                   PARM                    SPAQJN
041300020910     C                   PARM                    SPAIJN
041400020910     C                   PARM                    SPASPL
041500020910     C                   PARM                    SPASNM
041600020910     C                   PARM                    SPASNR
041700020910      *
041800020910     C                   ADD       1             COUNT
041900020910     C                   ADD       PRISIZ        STRPOS
042000040520      *
042100041006      * Inizializzo lo spool come "da visualizzare" (*IN21)
042200040520     C                   SETOFF                                       21
042300040520      * Inserire qui eventuali controlli sul singolo spool
042400040520     C                   IF        PAGINE > 0 AND VARTPG < PAGINE
042500040520     C                   SETON                                        21
042600040520     C                   ENDIF
042700121214     C***                IF        %trim(VARPRT) = '*AFPDS' and
042800121214     C***                          %subst(%trim(USNAM):1:3) <> 'EDP' and
042900121214     C***                          %subst(%trim(USNAM):1:3) <> 'POC' and
043000121214     C***                          %subst(%trim(USNAM):1:3) <> 'ROP' and
043100121214     C***                          %subst(%trim(USNAM):1:3) <> 'RP1' and
043200121214     C***                          %subst(%trim(USNAM):1:3) <> 'RP2'
043300121214     C***                SETON                                        21
043400121214     C***                ENDIF
043500020910      *
043600020910     C     *IN21         IFEQ      *OFF
043700020910     C                   EXSR      VIDEO
043800020910     C                   ENDIF
043900020910     C                   ENDDO
044000020910      *
044100020910     C                   ENDSR
044200020910      *---------------------------------------------------------------*
044300020910      *  GESTIONE DEL VIDEO
044400020910      *---------------------------------------------------------------*
044500020910     C     GESVID        BEGSR
044600020910      *
044700020910     C     S1NRR         IFEQ      0
044800020910     C                   ADD       1             S1NRR
044900020910     C                   Z-ADD     *ZEROS        S1TOTP
045000020910     C                   MOVEL(P)  '*'           VARFIL
045100020910     C                   MOVEL(P)  '*'           VARUSN
045200020910     C                   MOVEL(P)  '*'           OUTQNM
045300020910     C                   MOVEL(P)  '*'           VARDTA
045400020910     C                   MOVEL(P)  '*'           S1STAT
045500040202     C                   WRITE     UL83S1
045600041006     C                   ENDIF
045700020910      *
045800020910     C                   DO        *HIVAL
045900040202     C                   WRITE     UL83P1
046000020910     C     C1RCD         IFEQ      0
046100020910     C                   Z-ADD     1             C1RCD
046200020910     C                   ENDIF
046300040202     C                   EXFMT     UL83C1
046400040218     C                   EVAL      MESSAGGIO = *blanks
046500040520     C* Gestisco F03
046600020910     C     *IN03         IFEQ      *ON
046700020910     C                   LEAVE
046800020910     C                   ENDIF
046900040520     C* Gestisco F12
047000040520     C     *IN12         IFEQ      *ON
047100040520     C                   EXSR      DLTOUT
047200040520     C                   GOTO      SCELTA
047300040520     C                   ENDIF
047400021104      * swap modalità visualizzazione
047500021104      * n77 = dataora 77=Modulo/Copie
047600041006     C                   IF        *in11
047700041006     C                   EVAL      *in77=not *in77
047800021104     C                   EXSR      DLTOUT
047900021104     C                   Z-ADD     1             C1RCD
048000021104     C                   GOTO      INIZIO
048100041006     C                   ENDIF
048200021104      *
048300041006     C* Gestisco F05
048400021104     C     *IN05         IFEQ      *ON
048500020910     C                   EXSR      DLTOUT
048600021017     C                   Z-ADD     1             C1RCD
048700020910     C                   GOTO      INIZIO
048800020910     C                   ENDIF
048900041006     C*
049000041006     C* Inizializzo il flag che indica la scelta multipla spool da parte dell'utente
049100041006     C                   MOVEL     'N'           wFlgMulti         1
049200041006     C*
049300041006     C* Inizializzo le schiera x il passaggio spool x la creazione e-mail
049400041006     C                   CLEAR                   SKSPOOL
049500041006     C                   CLEAR                   SKSPOOLMULT
049600041006     C*
049700041006     C* Inizializzo l'indice della schiera multiplae x il passaggio spool x la creazione e-mail
049800041006     C                   Z-ADD     *zeros        skjMULT           3 0
049900041006     C*
050000041006     C     ancora        TAG
050100041006     C*
050200040202     C                   READC     UL83S1                                 95
050300020910     C     *IN95         IFEQ      *OFF
050400020910     C                   Z-ADD     S1NRR         C1RCD
050500041006     C*
050600041006     C**** OPZIONE 1 ****
050700060810     C                   IF        S1SCE = '1'
050800060810     C                   IF        *IN46 = *OFF
050900041006     C* Incremento il contatore della schiera x il passaggio spool x la creazione e-mail
051000041006     C                   Z-ADD     1             skj               3 0
051100041006     C*
051200040202     C                   EVAL      SPLNUM = *all'0'
051300040217     C                   MOVE      FILN          SPLNUM            6
051400040217     C                   MOVE(P)   VARCPI        NUMCPI            9 1
051500040217     C                   Z-ADD     VARLEN        NUMRIG            9 0
051600040217     C                   Z-ADD     VARWID        NUMCOL            9 0
051700041006     C* Valorizzo la schiera x il passaggio spool x la creazione e-mail
051800041006     C                   EXSR      VALDSSPOOL
051900041006     C                   EVAL      SKSPOOL(skj) = DSSPOOL
052000041006     C* Spedisco la mail
052100041006     C                   EVAL      FLGSND = 'S'
052200041006     C                   EXSR      SNDEML
052300040218     C                   IF        ESITO = '2'
052400040218     C                   EVAL      MESSAGGIO = MSG
052500041006     C                   ENDIF
052600060810     C*
052700060810     C                   ELSE
052800060810     C                   EVAL      MESSAGGIO = 'Opzione non gestita'
052900060810     C                   ENDIF
053000040202     C                   ENDIF
053100041006     C*
053200041006     C**** OPZIONE 9 ****
053300041006     C                   IF        S1SCE = '9'
053400060810     C                   IF        *IN46 = *OFF
053500041006     C* Incremento il contatore della schiera x il passaggio spool x la creazione e-mail
053600041006     C                   Z-ADD     1             skj               3 0
053700041006     C                   ADD       1             skjMULT
053800041006     C*
053900041006     C                   MOVEL     'S'           wFlgMulti
054000041006     C                   EVAL      SPLNUM = *all'0'
054100041006     C                   MOVE      FILN          SPLNUM
054200041006     C                   MOVE(P)   VARCPI        NUMCPI
054300041006     C                   Z-ADD     VARLEN        NUMRIG
054400041006     C                   Z-ADD     VARWID        NUMCOL
054500041006     C* Valorizzo la schiera x il passaggio spool x la creazione e-mail
054600041006     C                   EXSR      VALDSSPOOL
054700041006     C                   EVAL      SKSPOOL(skj) = DSSPOOL
054800041006     C                   EVAL      SKSPOOLMULT(skjMULT) = DSSPOOL
054900041006     C* Effettuo solo controllo abilitazione allo spool corrente
055000041006     C                   EVAL      FLGSND = 'N'
055100041006     C                   EXSR      SNDEML
055200041006     C                   IF        ESITO = '2'
055300041006     C                   EVAL      MESSAGGIO = MSG
055400041006     C                   ENDIF
055500060810     C*
055600060810     C                   ELSE
055700060810     C                   EVAL      MESSAGGIO = 'Opzione non gestita'
055800060810     C                   ENDIF
055900041006     C                   ENDIF
056000041006     C*
056100041006     C**** OPZIONE 4 ****
056200041006     C                   IF        S1SCE = '4'
056300060810     C                   IF        *IN46 = *OFF
056400041006     C                   EVAL      SPLNUM = *all'0'
056500041006     C                   MOVE      FILN          SPLNUM
056600041006     C                   CALL      'TRUL83C'
056700041006     C                   PARM                    S1SCE
056800041006     C                   PARM                    VARFIL
056900041006     C                   PARM                    VARJNM
057000041006     C                   PARM                    VARUSN
057100041006     C                   PARM                    JOBNUM
057200041006     C                   PARM                    SPLNUM
057300041006     C                   PARM                    ESITO
057400041006     C                   IF        ESITO = '2'
057500041006     C                   EVAL      MESSAGGIO = 'Errori nella eliminazione ' +
057600041006     C                                         'dello spool'
057700041006     C                   ELSE
057800041006     C                   SETON                                        05
057900041006     C                   ENDIF
058000060810     C*
058100060810     C                   ELSE
058200060810     C                   EVAL      MESSAGGIO = 'Opzione non gestita'
058300060810     C                   ENDIF
058400041006     C                   ENDIF
058500041006     C*
058600041006     C**** OPZIONE 5 ****
058700040218     C                   IF        S1SCE = '5'
058800040218     C                   EVAL      SPLNUM = *all'0'
058900040218     C                   MOVE      FILN          SPLNUM
059000041006     C                   CALL      'TRUL83C'
059100040218     C                   PARM                    S1SCE
059200040218     C                   PARM                    VARFIL
059300040218     C                   PARM                    VARJNM
059400040218     C                   PARM                    VARUSN
059500040218     C                   PARM                    JOBNUM
059600040218     C                   PARM                    SPLNUM
059700040218     C                   PARM                    ESITO
059800040218     C                   IF        ESITO = '2'
059900040218     C                   EVAL      MESSAGGIO = 'Errori nella visualizzazione ' +
060000040218     C                                         'dello spool'
060100040218     C                   ENDIF
060200060810     C*
060300040218     C                   ENDIF
060400060810     C*
060500060810     C**** OPZIONE X ****
060600060810     C                   IF        S1SCE = 'X'
060700060810     C                   IF        *IN46 = *ON AND *IN44 = *OFF
060800060810     C                   EVAL      SPLNUM = *all'0'
060900060810     C                   MOVE      FILN          SPLNUM
061000060810     C                   EVAL      o_VARFIL = VARFIL
061100060810     C                   EVAL      o_SPLNUM = SPLNUM
061200060810     C                   EVAL      o_VARUSN = VARUSN
061300060810     C                   EVAL      o_VARJNM = VARJNM
061400060810     C                   EVAL      o_JOBNUM = JOBNUM
061500060810     C                   EVAL      o_VARTYP = VARTYP
061600060810     C                   EVAL      o_VARDTA = VARDTA
061700060810     C                   GOTO      FINE
061800060810     C*
061900060810     C                   ELSE
062000060810     C                   EVAL      MESSAGGIO = 'Opzione non gestita'
062100060810     C                   ENDIF
062200060810     C                   ENDIF
062300041006     C*
062400041006     C* Proseguo con la lettura dei record modificati del sub-file
062500040202     C                   MOVE      *BLANK        S1SCE
062600040202     C                   UPDATE    UL83S1
062700040218     C                   GOTO      ancora
062800041006     C*
062900021111     C                   ENDIF
063000041006     C*
063100041006     C**** GESTIONE OPZIONE 9 (SCELTA MULTIPLA = <N> SPOOL IN 1 SINGOLA MAIL)
063200041006     C                   IF        wFlgMulti = 'S'
063300041011     C*
063400041011     C* Verifico che al massimo si possano effettuare al massimo 5 scelte multiple (solo x utenti nn 'EDP*')
063500041011     C                   IF        skjMULT > 5 AND
063600041011     C                             %subst(%trim(USNAM):1:3) <> 'EDP'
063700041011     C                   EVAL      MESSAGGIO = 'Possibili al massimo 5 scelte' +
063800041011     C                                         ' per l''invio multiplo'
063900041011     C                   ELSE
064000041006     C* Valorizzo la schiera x il passaggio spool x la creazione e-mail
064100041006     C                   EVAL      SKSPOOL = SKSPOOLMULT
064200041006     C* Spedisco la mail
064300041006     C                   EVAL      FLGSND = 'S'
064400041006     C                   EXSR      SNDEML
064500041006     C                   IF        ESITO = '2'
064600041006     C                   EVAL      MESSAGGIO = MSG
064700041006     C                   ENDIF
064800041006     C                   ENDIF
064900041011     C                   ENDIF
065000041006     C*
065100020910     C                   ENDDO
065200040218     C*
065300020910     C                   ENDSR
065400041006      *---------------------------------------------------------------*
065500041006      *  VALDSSPOOL - VALORIZZAZIONE DS DI PASSAGGIO DATI SPOOL
065600041006      *---------------------------------------------------------------*
065700041006     C     VALDSSPOOL    BEGSR
065800041006     C*
065900041006     C                   CLEAR                   DSSPOOL
066000041006     C*
066100041006     C                   EVAL      dsVARFIL = VARFIL
066200041006     C                   EVAL      dsVARJNM = VARJNM
066300041006     C                   EVAL      dsVARUSN = VARUSN
066400041006     C                   EVAL      dsJOBNUM = JOBNUM
066500041006     C                   EVAL      dsSPLNUM = SPLNUM
066600041006     C                   EVAL      dsVARDTA = VARDTA
066700041006     C                   EVAL      dsNUMRIG = NUMRIG
066800041006     C                   EVAL      dsNUMCOL = NUMCOL
066900041006     C                   EVAL      dsNUMCPI = NUMCPI
067000041006     C                   EVAL      dpOUTQNM = OUTQNM
067100041006     C                   EVAL      ds$ROWS  = $ROWS
067200041006     C                   EVAL      dsS1TOTP = S1TOTP
067300041006     C                   EVAL      dsS1STAT = S1STAT
067400041006     C*
067500041006     C                   ENDSR
067600041006      *---------------------------------------------------------------*
067700041006      *  SNDEML - CREAZIONE E-MAIL
067800041006      *---------------------------------------------------------------*
067900041006     C     SNDEML        BEGSR
068000041006     C*
068100041006     C                   CALL      'TRUL83R1'
068200041006     C                   PARM                    SKSPOOL
068300051102     C                   PARM                    SKSPOOLID
068400051102     C                   PARM                    FLGSND            1
068500041006     C                   PARM                    USREMAIL
068600051102     C                   PARM      *blanks       EmailDest       110
068700051102     C                   PARM      *blanks       EmailObj         55
068800051102     C                   PARM      *blanks       EmailTxt1        75
068900051102     C                   PARM      *blanks       EmailTxt2        75
069000051102     C                   PARM      *blanks       EmailTxt3        75
069100051102     C                   PARM      *blanks       EmailTxt4        75
069200051102     C                   PARM      *blanks       EmailTxt5        75
069300051102     C                   PARM      *blanks       EmailTxt6        75
069400051102     C                   PARM      *blanks       EmailTxt7        75
069500051102     C                   PARM      *blanks       EmailTxt8        75
069600051102     C                   PARM      'N'           FlgTappoLast      1
069700051102     C                   PARM      'N'           FlgBypChSp        1
069800051102     C                   PARM      'S'           FlgDspVid         1
069900051102     C                   PARM      'N'           FlgDltSplIn       1
070000051102     C                   PARM      '0'           ESITO             1
070100051102     C                   PARM      *blanks       MSG              50
070200041006     C*
070300041006     C                   ENDSR
070400020909      *---------------------------------------------------------------*
070500020910**
070600020909DLTUSRSPC  USRSPC(QTEMP/USSPOOL)
