000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940726      * FNLRB5R *----------------------------------------------------*
000300940726      *-----------------*
000400940726      *            VISUALIZZAZIONE EVENTI
000500940726      *--------------------------------------------------------------*
000600940726      *  Indicatori utilizzati:
000700940726      *  20   ON   SFLDSP
000800940726      *  21   ON   SFLDSPCTL
000900940726      *  21   OFF  SFLCLR
001000940726      *  23   ON   SFLEND
001100940726      *  30
001200940726      *  31
001300940726      *  33
001400010122      *  41   ON   PRESENTE DEPOT DPD             (IN CAMPO NOTE)
001500010122      *  42   ON   PRESENTE DISTINTA + PADRONCINO (IN CAMPO NOTE)
001600940726      *--------------------------------------------------------------*
001700020920     FFNLRB5D   CF   E             WORKSTN SFILE(LRB5S01:NRR1)
001800990805     FFNEVB01L  IF   E           K DISK
001801140226     FFNEVB22L  IF   E           K DISK
001900930525     FAZORG01L  IF   E           K DISK
002000930525     FTABEL00F  IF   E           K DISK
002100101206     Ffipct01l  IF   E           K DISK    usropn infds(PCT_filefbk)
002200130409     Ffidst01l  IF   E           K DISK    usropn
002300020920
002400101206     d PCT_filefbk     DS
002500101206     D pct_FILE          *FILE                                                   File name
002600101206     D pct_OPEN_IND            9      9N                                        * File open?
002700101206     D pct_EOF_IND            10     10N                                        * File at eof?
002800101206     D pct_STATUS        *STATUS                                                *STATUS
002900101206     D pct_OPCODE        *OPCODE                                                * last OPCODE
003000101206     D pct_ROUTINE       *ROUTINE                                               * RPG Routine
003100101206     D pct_LIST_NUM           30     37                                         * Listing line
003200101206     D pct_SPCL_STAT          38     42S 0                                      * SPECIAL status
003300101206     D pct_RECORD        *RECORD                                                * Record name
003400101206     D pct_MSGID              46     52                                         * Error MSGID
003500101206
003600020920     D POG             S              3    DIM(250)  inz                        FIL GESTITE
003700930526     D FI1             S              3    DIM(29)                              FIL. COMODO
003800930525     D CDU             S              1    DIM(14)                              COMODO X CDU
003900930525     D FLE             S              1    DIM(13)                              COMODO X FLE
004000930525     D CEV             S              1    DIM(63)                              COMODO X CEV
004100101111
004200101111     D fidg46ds      E DS
004300020920
004400131031     D Devb01        E DS
004500131031
004600900618     D PARAM           DS
004700940726     D  PARAAS                14     17  0
004800940726     D  VIDLNP                18     20  0
004900940726     D  VIDNRS                21     22  0
005000940726     D  VIDNSP                23     29  0
005100940726     D  PARF03                30     30
005200940726     D  PARFLG                31     31
005300940726     D  PARPGM                32     41
005400940726     D  PARDSA                42     61
005500940726     D  VIDDSP                63     70  0
005600940726     D  VIDRMN                71     85  0
005700940726     D  VIDLNA                86     88  0
005800940726     D  PADLNA                89    108
005900941031     D  PARRSD               109    143
006000941031     D  VIDDCP               144    151  0
006100941031     D  VIDTMC               152    152
006200020920
006300930525     D                 DS
006400930525     D  AA                     1      2  0
006500930525     D  GG                     5      6  0
006600930525     D  DATA                   1      6  0
006700020920
006800021014     d codut           s                   like(TBLKUT) inz(1)
006900021014     d kcod            s                   like(TBLcod) INZ('2A')
007000070427     d kcodz           s                   like(TBLcod) INZ('2Z')
007100021014     d kkey            s                   like(TBLkey)
007200021014     d kaas2           s                   like(EVBAAS)
007300130409     d npg             s                   like(dstnpg) inz(4)
007400021014
007500930525     D DS2A          E DS
007600070427     D DS2Z          E DS
007700000000     D KPJBA         E DS
007800130227     d*trul06ds      e ds
007900130227     d*tibs56ds      e ds
008000021014     D ddatiute      e ds
008100021014     D azuteds       e ds                  extname(AZUTE00F)
008200021014     D tibs34ds      E DS                  inz
008300021112     D tibs36ds      E DS
008400130411     D ddstflr       E DS
008500940726     C**************************************************************************
008600000000     C     *ENTRY        PLIST
008700000000     C                   PARM                    KPJBA
008800900409     C                   MOVEL     KPJBU         PARAM
008900101202     C*
009000990805     C                   Z-ADD     PARAAS        KAAS2                          ANNO DI 4 CIFRE
009100940726     C                   MOVEL     PARDSA        V1CDSA
009200940726     C                   MOVEL     PADLNA        DESLNA                         DECOD.LINEA ARRIVO
009300940726     C                   MOVEL     PARRSD        VIDRSD                         DESTINATARIO
009400021014
009500021014     c     *dtaara       define    §azute        azuteds
009600021014     c     *dtaara       define    §datiute      ddatiute
009700021014
009800021014     C                   in(E)     *dtaara
009900021014     C                   IF        %Error  or  RSUT = *blanks
010000021014     c                   clear                   tibs34ds
010100021014     C                   call      'TIBS34R'
010200021014     C                   parm                    Tibs34Ds
010300021014     C                   in        *dtaara
010400021014     c                   ENDIF
010500021014
010600930525     C*---------------------------------------------------------------*
010700930525     C*
010800940726     C* PARFLG = 'C' CHIUDO CON LR SENZA VISUALIZZARE
010900940726     C     PARFLG        IFEQ      'C'
011000930525     C                   SETON                                        LR
011100930525     C                   ELSE
011200020920      *
011300020920      * Determino in che AS sono
011400130227     C**   SIMFEL        CHAIN     AZORG                              31
011500020920      * Se sono sull'as di sede carico i p.o. presenti
011600130227     C***                IF        orgfel = 046
011700130227     C***                EXSR      CARPO
011800130227     c***                ELSE
011900020920      * Se sono su as periferico carico i p.o. in £1
012000130227     C***                EXSR      CAR£1
012100130227     c***                ENDIF
012200101203     c****
012300930525     C*** P U L I Z I A   S U B F I L E
012400940726     C                   SETOFF                                       202123
012500940725     C                   WRITE     LRB5C01
012600940726     C                   SETON                                        2123
012700930525     C*
012800940725     C                   Z-ADD     0             NRR1              4 0
012900930525     C*
013000930525     C*** C A R I C O    S U B F I L E
013100990805     C     KARB1         SETLL     FNEVB000
013200990805     C     KARB1         READE     FNEVB000                               30
013300930525     C*
013400930525     C     *IN30         DOWEQ     '0'
013500930526     C** ESCLUDO RECORD ANNULLATI
013600930525     C     EVBATB        IFEQ      ' '
013700930525     C*
013800930526     C** DATA E ORA EVENTO
013900990805     C                   MOVE      EVBDEV        WDAT6             6 0
014000990805     C                   MOVEL     WDAT6         VIDDEV
014100990805     C                   MOVEL     WDAT6         DATA
014200930525     C                   MOVEL     GG            VIDDEV
014300930525     C                   MOVE      AA            VIDDEV
014400930525     C*
014500930525     C                   MOVEL     EVBHEV        VIDHEV
014600930525     C*
014700930526     C** CAUSALE EVENTO
014800930526     C                   MOVEL     *BLANKS       VIDCEV
014900930526     C                   MOVEA     *BLANKS       CEV
015000021014     C                   MOVEL(P)  EVBCEV        KKEY
015100940726     C     KTAB1         CHAIN     TABEL00F                           31
015200930525     C  N31              MOVEA     TBLUNI        CEV(5)
015300930525     C   31              MOVEA     *BLANKS       CEV(5)
015400930525     C                   MOVEA     EVBCEV        CEV(1)
015500930525     C                   MOVEA     '-'           CEV(4)
015600930525     C                   MOVEA     CEV           VIDCEV
015700930525     C*
015800930526     C** DATA E ORA IMMISSIONE EVENTO
015900990805     C                   MOVE      EVBDTV        WDAT6
016000990805     C                   MOVEL     WDAT6         VIDDTV
016100990805     C                   MOVEL     WDAT6         DATA
016200930525     C                   MOVEL     GG            VIDDTV
016300930525     C                   MOVE      AA            VIDDTV
016400930525     C*
016500930525     C                   MOVEL     EVBORV        VIDORV
016501140226     C** se consegna parziale inserisce nella parte finale della descrizione evento il firmatario
016502140226     c                   clear                   wfirm            30
016503140227     c                   clear                   wfirm37          37
016504140226     c                   if        evbcev = 'P  '
016505140226     c     kevb2         setll     fnevb22l
016506140226     c                   do        *hival
016507140226     c     kevb2         reade     fnevb22l
016508140226     c                   if        %eof(fnevb22l)
016509140226     c                   leave
016510140226     c                   endif
016511140226     c                   if        evb2trd <> 'FIR'
016512140226     c                   iter
016513140226     c                   endif
016514140226     c                   movel     evb2dati      wfirm
016515140227     c                   eval      wfirm37 = 'Firma ' + wfirm
016516140227     c                   move      wfirm37       vidcev
016517140226     c                   enddo
016518140226     c                   endif
016600930525     C*
016700930526     C** CODICE UTENTE VIDEO
016800930526     C                   MOVEL     *BLANKS       VIDCDU
016900930526     C                   MOVEA     *BLANKS       CDU
017000020920      * decodifico il cod.utente solo se EVBFLE è tra i p.o. gestiti
017100130227     C***                movel     evbfle        W003A             3
017200130227     C***  W003A         LOOKUP    POG                                    33
017300020920      *
017400020920     C                   MOVEA     *BLANKS       CDU(5)
017500020920      *
017600130227     C***                IF        *in33 = *on
017700021112     c                   clear                   TIBS36DS
017800021112     C                   eval      i36tla = 'L'
017900021112     c                   eval      i36pou = EVBFLE
018000021112     c                   eval      i36cou = EVBCDU
018100021112     c                   call      'TIBS36R'
018200021112     c                   parm                    tibs36ds
018300021112     c                   IF        O36err = *blanks
018400021112     C                   MOVEA     O36UTE        CDU(5)
018500020920     C                   endif
018600130227     C****               ENDIF
018700020920      *
018800930525     C                   MOVEL     EVBCDU        COMCDU            3
018900930525     C                   MOVEA     COMCDU        CDU(1)
019000130227     C** 33              MOVEA     '-'           CDU(4)
019100130227     C                   MOVEA     '-'           CDU(4)
019200930525     C                   MOVEA     CDU           VIDCDU
019300930525     C*
019400930526     C** COD. FILIALE IMMISSIONE EVENTO
019500930525     C                   MOVEL     *BLANKS       VIDFLE
019600930526     C                   MOVEA     *BLANKS       FLE
019700930525     C     EVBFLE        CHAIN     AZORG                              31
019800930525     C  N31              MOVEA     ORGDES        FLE(5)
019900930525     C   31              MOVEA     *BLANKS       FLE(5)
020000930525     C                   MOVEL     EVBFLE        COMFLE            3
020100930525     C                   MOVEA     COMFLE        FLE(1)
020200930525     C                   MOVEA     '-'           FLE(4)
020300930525     C                   MOVEA     FLE           VIDFLE
020400010122     C*
020500010122     C** CAMPO NOTE (POSSO AVERE BLANK O DEPOT DPD O DISTINTA+PADRONCINO)
020600010122     C                   SETOFF                                       41  42
020700131031     C                   MOVEL     EVBNOT        Devb01
020800131031      *
020900131031     c                   IF           §NOTNDC <> *blank or
021000131031     c                                §NOTPDC <> *blank
021100131111     c                                  or §notDEP <> *blank
021200131111      *  DEPOT DPD da mostrare
021300131113      *    ma poichè alcuni eventi sono generati con NDC/PDC oltre che DEP
021400131113      *    x allineare a quanto faceva prima devo testare con il SUBST l'NDC
021500131113      *    x fare in modo che il programma giri come prima.
021600131113      *  Se si deve fare diversamente Si ragionerà in seguito. Addesso deve dare gli
021700131113      *  stessi risultati di prima.
021800131113     c                   IF         §notDEP <> *blank and
021900131113     c                                %subst(§notNDC:5:3) = *blank
022000010122     C                   SETON                                        41
022100131111     c                   MOVEL     §notDEP       VIDPOD                         DEPOT DPD
022200131031      ****
022300010122     C                   ELSE
022400131031      ****
022500010122     C                   SETON                                        42
022600131031      ****
022700131031     c                   MOVEL     §NOTNDC       VIDDIS                         DISTINTA
022800131031     C                   MOVEL     §NOTPDC       VIDPOP                         P.O. PADRONCINO
022900131031     C                   MOVE      §NOTPDC       VIDCOP                         COD. PADRONCINO
023000070427      *
023100010122     C                   ENDIF
023200010122     C                   ENDIF
023300010122     C*
023400940725     C                   ADD       1             NRR1
023500941031     C  N20              SETON                                        20        SFLDSP
023600940725     C                   WRITE     LRB5S01
023700930525     C                   END
023800930525     C*
023900990805     C     KARB1         READE     FNEVB000                               30
024000930525     C                   END
024100101111     C*
024200101202     c                   setoff                                       44
024300101111     C* sente se presenti dei records con 'CET' x Esiti con il PDA
024400101203     C* solo in ambiente di Filiale.
024500101206     c************       if        knsif <> 'GAITRA201'
024600101206     C*
024700101206     c                   if        pct_OPEN_IND = '0'
024800101206     c                   open(e)   Fipct01L
024900130409     c                   open(e)   Fidst01l
025000101206     c                   if        Pct_STATUS = 0
025100101206     C*
025200101206     c************       if        knsif <> 'GAITRA201'
025300101111     c                   eval        PCTAAS =  PARAAS
025400101111     c                   eval        PCTLNP =  VIDLNP
025500101111     c                   eval        PCTNRS =  VIDNRS
025600101111     c                   eval        PCTNSP =  VIDNSP
025700101111     c                   eval        PCTTRD = 'CET'
025800101111     C*
025900101202     C     KCET01        chain     fiPCT01l
026000101202     c                   if        %found(fiPCT01l)
026100130409     c     kdst          chain     fidst01l
026200130411     c                   if        %found(fidst01l)
026300130411     c                   movel     dstflr        ddstflr
026400130411     c                   if        §DSTTSTPDA = *blank or
026500130411     c                             §DSTTSTPDA = 'O'
026600101111     c                   setoN                                        44
026700130411     c                   end
026800130409     c                   end
026900101206     c**********         end
027000101202     c                   end
027100101206     C*
027200101206     c                   close     Fipct01L
027300130409     c                   close     Fidst01L
027400101206     c                   end
027500101206     c                   end
027600930525     C*
027700930525     C*** E M E T T O    S U B F I L E
027800940726     C******     NRR1      IFGT 0
027900940725     C                   Z-ADD     1             REC1
028000940725     C                   WRITE     LRB5T01                                      INTESTAZIONE VIDEO
028100940725     C                   WRITE     LRB5Z01                                      TASTI FUNZIONALI
028200940726     C  N20              WRITE     LRB5D02                                      SFL VUOTO, MESSAGGIO
028300110201     c     video         tag
028400940725     C                   EXFMT     LRB5C01
028500941109     C*
028600120921     c* F3 - fine
028700941109     C   KC              MOVEL     '1'           PARF03                         PREMUTO F3=FINE
028800941109     C   KC              MOVEL     PARAM         KPJBU
028900941109     C*
029000120921     c* f12 - rimuovo param in kpjbu
029100120921     C   KL              MOVEL     PARAM         KPJBU
029200120921     c
029300110201     c                   clear                   torna_aVideo      1
029400120921     c* f05 - Esisti PDa
029500101111     C   KE              exsr      esitiPDA
029600110201     c                   if        torna_aVideo ='S'
029700110201     c                   goto      video
029800110201     c                   end
029900101111     C*
030000930525     C* CMD12 O ENTER --> RITORNO
030100940726     C******               END
030200930525     C*
030300930525     C                   RETURN
030400930525     C                   END
030500940726     C**************************************************************************
030600101111     C*    F5 ->  ESITI PDA
030700940726     C**************************************************************************
030800101111     C     esitiPDA      BEGSR
030900020920      *
031000101111     C                   clear                   fidg46ds
031100101111     C                   Z-ADD     VIDlna        dg46fgsi
031200101111     C                   Z-ADD     PCTndc        dg46ndci
031300101111     C                   Z-ADD     PCTCODAUT     dg46codaut
031400101111     C                   Z-ADD     PARaas        dg46aasi
031500101111     C                   Z-ADD     VIDLNP        dg46LNPi
031600101111     C                   Z-ADD     VIDNSP        dg46NSPi
031700101111     C                   Z-ADD     VIDNRS        dg46NRSi
031800101111     C                   MOVEL(P)  fidg46ds      KPJBU
031900101111     C                   CALL      'FIDG49R'
032000101111     c                   PARM                    KPJBA
032100101111     c                   movel     kpjbu         fidg46ds
032200101111     C                   if        dg46erro = '1'
032300101111     c                   end
032400110201     C                   if        dg46erro = '2'
032500110201     c                   eval      torna_aVideo ='S'
032600110201     c                   end
032700101111      *
032800101111     C                   ENDsr
032900101111     C**************************************************************************
033000101111     C*    CARICO P.O. presenti sull'as di sede
033100101111     C**************************************************************************
033200130227     **    CARPO         BEGSR
033300101111      *
033400130227     c*                  clear                   tibs56ds
033500130227     c*                  eval      I56ppo = simfel
033600130227     c*                  eval      I56mod = 'POA'
033700130227     c*                  call      'TIBS56R'
033800130227     c*                  parm                    tibs56ds
033900130227     c*                  moveA     o56ski        POG
034000020920      *
034100130227     C*                  ENDSR
034200020920     C**************************************************************************
034300020920     C*    CARICO TABELLA FILIALI GESTITE £1
034400020920     C**************************************************************************
034500130227     C*    CAR£1         BEGSR
034600020920     C*
034700130227     c*                  clear                   trul06ds
034800130227     c*                  eval      d06cod = '£1'
034900130227     c*                  movel     simfel        d06key
035000130227     c*                  movel     trul06ds      kpjbu
035100130227     c*                  call      'TRUL06R'
035200130227     c*                  parm                    kpjba
035300130227     c*                  movel     kpjbu         trul06ds
035400130227     c*                  moveA     D06lia        POG
035500020920      * Reimposto la kpjbu con i dati della param
035600130227     c*                  movel     param         kpjbu
035700020920     C*
035800130227     C*                  ENDSR
035900940726     C**************************************************************************
036000940726     C*    R O U T I N E     I N I Z I A L E
036100940726     C**************************************************************************
036200940726     C     *INZSR        BEGSR
036300940726     C*
036400940726     C     KARB1         KLIST
036500990805     C                   KFLD                    KAAS2                          ..ANNO DI 4 CIFRE
036600940726     C                   KFLD                    VIDLNP
036700940726     C                   KFLD                    VIDNRS
036800940726     C                   KFLD                    VIDNSP
036900940726     C     KTAB1         KLIST
037000940726     C                   KFLD                    CODUT
037100940726     C                   KFLD                    KCOD
037200940726     C                   KFLD                    KKEY
037300021014      *
037400070427     C     KTAB1Z        KLIST
037500070427     C                   KFLD                    CODUT
037600070427     C                   KFLD                    KCODZ
037700070427     C                   KFLD                    KKEY
037800070427      *
037900140226     C     Kevb2         KLIST
038000070430     C                   KFLD                    EVBAAS
038100070430     C                   KFLD                    EVBLNP
038200070430     C                   KFLD                    EVBNRS
038300070430     C                   KFLD                    EVBNSP
038400070430     C                   KFLD                    EVBDTV
038500070430     C                   KFLD                    EVBORV
038600070430     C                   KFLD                    EVBCEV
038700101111      *
038800101111     C     KCET01        klist
038900101111     C                   KFLD                    PCTAAS
039000101111     C                   KFLD                    PCTLNP
039100101111     C                   KFLD                    PCTNRS
039200101111     C                   KFLD                    PCTNSP
039300101111     C                   KFLD                    PCTTRD
039400130409     C     Kdst          klist
039500130409     C                   KFLD                    npg
039600130409     C                   KFLD                    PCTndc
039700130409     C                   KFLD                    PCTfgs
039800070427      *
039900940726     C                   ENDSR
