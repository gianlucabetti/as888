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
001900140226     FFNEVB22L  IF   E           K DISK
002000930525     FAZORG01L  IF   E           K DISK
002100930525     FTABEL00F  IF   E           K DISK
002200101206     Ffipct01l  IF   E           K DISK    usropn infds(PCT_filefbk)
002300130409     Ffidst01l  IF   E           K DISK    usropn
002400020920
002500101206     d PCT_filefbk     DS
002600101206     D pct_FILE          *FILE                                                   File name
002700101206     D pct_OPEN_IND            9      9N                                        * File open?
002800101206     D pct_EOF_IND            10     10N                                        * File at eof?
002900101206     D pct_STATUS        *STATUS                                                *STATUS
003000101206     D pct_OPCODE        *OPCODE                                                * last OPCODE
003100101206     D pct_ROUTINE       *ROUTINE                                               * RPG Routine
003200101206     D pct_LIST_NUM           30     37                                         * Listing line
003300101206     D pct_SPCL_STAT          38     42S 0                                      * SPECIAL status
003400101206     D pct_RECORD        *RECORD                                                * Record name
003500101206     D pct_MSGID              46     52                                         * Error MSGID
003600101206
003700020920     D POG             S              3    DIM(250)  inz                        FIL GESTITE
003800930526     D FI1             S              3    DIM(29)                              FIL. COMODO
003900930525     D CDU             S              1    DIM(14)                              COMODO X CDU
004000930525     D FLE             S              1    DIM(13)                              COMODO X FLE
004100930525     D CEV             S              1    DIM(63)                              COMODO X CEV
004200101111
004300101111     D fidg46ds      E DS
004400020920
004500131031     D Devb01        E DS
004600131031
004700900618     D PARAM           DS
004800940726     D  PARAAS                14     17  0
004900940726     D  VIDLNP                18     20  0
005000940726     D  VIDNRS                21     22  0
005100940726     D  VIDNSP                23     29  0
005200940726     D  PARF03                30     30
005300940726     D  PARFLG                31     31
005400940726     D  PARPGM                32     41
005500940726     D  PARDSA                42     61
005600940726     D  VIDDSP                63     70  0
005700940726     D  VIDRMN                71     85  0
005800940726     D  VIDLNA                86     88  0
005900940726     D  PADLNA                89    108
006000941031     D  PARRSD               109    143
006100941031     D  VIDDCP               144    151  0
006200941031     D  VIDTMC               152    152
006300020920
006400930525     D                 DS
006500930525     D  AA                     1      2  0
006600930525     D  GG                     5      6  0
006700930525     D  DATA                   1      6  0
006800020920
006900021014     d codut           s                   like(TBLKUT) inz(1)
007000021014     d kcod            s                   like(TBLcod) INZ('2A')
007100070427     d kcodz           s                   like(TBLcod) INZ('2Z')
007200021014     d kkey            s                   like(TBLkey)
007300021014     d kaas2           s                   like(EVBAAS)
007400130409     d npg             s                   like(dstnpg) inz(4)
007500021014
007600930525     D DS2A          E DS
007700070427     D DS2Z          E DS
007800000000     D KPJBA         E DS
007900130227     d*trul06ds      e ds
008000130227     d*tibs56ds      e ds
008100021014     D ddatiute      e ds
008200021014     D azuteds       e ds                  extname(AZUTE00F)
008300021014     D tibs34ds      E DS                  inz
008400021112     D tibs36ds      E DS
008500130411     D ddstflr       E DS
008600940726     C**************************************************************************
008700000000     C     *ENTRY        PLIST
008800000000     C                   PARM                    KPJBA
008900900409     C                   MOVEL     KPJBU         PARAM
009000101202     C*
009100990805     C                   Z-ADD     PARAAS        KAAS2                          ANNO DI 4 CIFRE
009200940726     C                   MOVEL     PARDSA        V1CDSA
009300940726     C                   MOVEL     PADLNA        DESLNA                         DECOD.LINEA ARRIVO
009400940726     C                   MOVEL     PARRSD        VIDRSD                         DESTINATARIO
009500021014
009600021014     c     *dtaara       define    §azute        azuteds
009700021014     c     *dtaara       define    §datiute      ddatiute
009800021014
009900021014     C                   in(E)     *dtaara
010000021014     C                   IF        %Error  or  RSUT = *blanks
010100021014     c                   clear                   tibs34ds
010200021014     C                   call      'TIBS34R'
010300021014     C                   parm                    Tibs34Ds
010400021014     C                   in        *dtaara
010500021014     c                   ENDIF
010600021014
010700930525     C*---------------------------------------------------------------*
010800930525     C*
010900940726     C* PARFLG = 'C' CHIUDO CON LR SENZA VISUALIZZARE
011000940726     C     PARFLG        IFEQ      'C'
011100930525     C                   SETON                                        LR
011200930525     C                   ELSE
011300020920      *
011400020920      * Determino in che AS sono
011500130227     C**   SIMFEL        CHAIN     AZORG                              31
011600020920      * Se sono sull'as di sede carico i p.o. presenti
011700130227     C***                IF        orgfel = 046
011800130227     C***                EXSR      CARPO
011900130227     c***                ELSE
012000020920      * Se sono su as periferico carico i p.o. in £1
012100130227     C***                EXSR      CAR£1
012200130227     c***                ENDIF
012300101203     c****
012400930525     C*** P U L I Z I A   S U B F I L E
012500940726     C                   SETOFF                                       202123
012600940725     C                   WRITE     LRB5C01
012700940726     C                   SETON                                        2123
012800930525     C*
012900940725     C                   Z-ADD     0             NRR1              4 0
013000930525     C*
013100930525     C*** C A R I C O    S U B F I L E
013200990805     C     KARB1         SETLL     FNEVB000
013300990805     C     KARB1         READE     FNEVB000                               30
013400930525     C*
013500930525     C     *IN30         DOWEQ     '0'
013600930526     C** ESCLUDO RECORD ANNULLATI
013700930525     C     EVBATB        IFEQ      ' '
013800930525     C*
013900930526     C** DATA E ORA EVENTO
014000990805     C                   MOVE      EVBDEV        WDAT6             6 0
014100990805     C                   MOVEL     WDAT6         VIDDEV
014200990805     C                   MOVEL     WDAT6         DATA
014300930525     C                   MOVEL     GG            VIDDEV
014400930525     C                   MOVE      AA            VIDDEV
014500930525     C*
014600930525     C                   MOVEL     EVBHEV        VIDHEV
014700930525     C*
014800930526     C** CAUSALE EVENTO
014900930526     C                   MOVEL     *BLANKS       VIDCEV
015000930526     C                   MOVEA     *BLANKS       CEV
015100021014     C                   MOVEL(P)  EVBCEV        KKEY
015200940726     C     KTAB1         CHAIN     TABEL00F                           31
015300930525     C  N31              MOVEA     TBLUNI        CEV(5)
015400930525     C   31              MOVEA     *BLANKS       CEV(5)
015500930525     C                   MOVEA     EVBCEV        CEV(1)
015600930525     C                   MOVEA     '-'           CEV(4)
015700930525     C                   MOVEA     CEV           VIDCEV
015800930525     C*
015900930526     C** DATA E ORA IMMISSIONE EVENTO
016000990805     C                   MOVE      EVBDTV        WDAT6
016100990805     C                   MOVEL     WDAT6         VIDDTV
016200990805     C                   MOVEL     WDAT6         DATA
016300930525     C                   MOVEL     GG            VIDDTV
016400930525     C                   MOVE      AA            VIDDTV
016500930525     C*
016600930525     C                   MOVEL     EVBORV        VIDORV
016700140226     C** se consegna parziale inserisce nella parte finale della descrizione evento il firmatario
016800140226     c                   clear                   wfirm            30
016900140227     c                   clear                   wfirm37          37
017000140226     c                   if        evbcev = 'P  '
017100140226     c     kevb2         setll     fnevb22l
017200140226     c                   do        *hival
017300140226     c     kevb2         reade     fnevb22l
017400140226     c                   if        %eof(fnevb22l)
017500140226     c                   leave
017600140226     c                   endif
017700140226     c                   if        evb2trd <> 'FIR'
017800140226     c                   iter
017900140226     c                   endif
018000140226     c                   movel     evb2dati      wfirm
018100140227     c                   eval      wfirm37 = 'Firma ' + wfirm
018200140227     c                   move      wfirm37       vidcev
018300140226     c                   enddo
018400140226     c                   endif
018500930525     C*
018600930526     C** CODICE UTENTE VIDEO
018700930526     C                   MOVEL     *BLANKS       VIDCDU
018800930526     C                   MOVEA     *BLANKS       CDU
018900020920      * decodifico il cod.utente solo se EVBFLE è tra i p.o. gestiti
019000130227     C***                movel     evbfle        W003A             3
019100130227     C***  W003A         LOOKUP    POG                                    33
019200020920      *
019300020920     C                   MOVEA     *BLANKS       CDU(5)
019400020920      *
019500130227     C***                IF        *in33 = *on
019600021112     c                   clear                   TIBS36DS
019700021112     C                   eval      i36tla = 'L'
019800021112     c                   eval      i36pou = EVBFLE
019900021112     c                   eval      i36cou = EVBCDU
020000021112     c                   call      'TIBS36R'
020100021112     c                   parm                    tibs36ds
020200021112     c                   IF        O36err = *blanks
020300021112     C                   MOVEA     O36UTE        CDU(5)
020400020920     C                   endif
020500130227     C****               ENDIF
020600020920      *
020700930525     C                   MOVEL     EVBCDU        COMCDU            3
020800930525     C                   MOVEA     COMCDU        CDU(1)
020900130227     C** 33              MOVEA     '-'           CDU(4)
021000130227     C                   MOVEA     '-'           CDU(4)
021100930525     C                   MOVEA     CDU           VIDCDU
021200930525     C*
021300930526     C** COD. FILIALE IMMISSIONE EVENTO
021400930525     C                   MOVEL     *BLANKS       VIDFLE
021500930526     C                   MOVEA     *BLANKS       FLE
021600930525     C     EVBFLE        CHAIN     AZORG                              31
021700930525     C  N31              MOVEA     ORGDES        FLE(5)
021800930525     C   31              MOVEA     *BLANKS       FLE(5)
021900930525     C                   MOVEL     EVBFLE        COMFLE            3
022000930525     C                   MOVEA     COMFLE        FLE(1)
022100930525     C                   MOVEA     '-'           FLE(4)
022200930525     C                   MOVEA     FLE           VIDFLE
022300010122     C*
022400010122     C** CAMPO NOTE (POSSO AVERE BLANK O DEPOT DPD O DISTINTA+PADRONCINO)
022500010122     C                   SETOFF                                       41  42
022600131031     C                   MOVEL     EVBNOT        Devb01
022700131031      *
022800131031     c                   IF           §NOTNDC <> *blank or
022900131031     c                                §NOTPDC <> *blank
023000131111     c                                  or §notDEP <> *blank
023100131111      *  DEPOT DPD da mostrare
023200131113      *    ma poichè alcuni eventi sono generati con NDC/PDC oltre che DEP
023300131113      *    x allineare a quanto faceva prima devo testare con il SUBST l'NDC
023400131113      *    x fare in modo che il programma giri come prima.
023500131113      *  Se si deve fare diversamente Si ragionerà in seguito. Addesso deve dare gli
023600131113      *  stessi risultati di prima.
023700171204     c                   IF         §notDEP <> *blank
023800171204      *****  La visualizzazione del DEPOT DPD:
023900171204     c********* con questa specifica in AND non avrebbe mai funzionato correttamente
024000171204     c*********                       and %subst(§notNDC:5:3) = *blank
024100010122     C                   SETON                                        41
024200131111     c                   MOVEL     §notDEP       VIDPOD                         DEPOT DPD
024300131031      ****
024400010122     C                   ELSE
024500131031      ****
024600010122     C                   SETON                                        42
024700131031      ****
024800131031     c                   MOVEL     §NOTNDC       VIDDIS                         DISTINTA
024900131031     C                   MOVEL     §NOTPDC       VIDPOP                         P.O. PADRONCINO
025000131031     C                   MOVE      §NOTPDC       VIDCOP                         COD. PADRONCINO
025100070427      *
025200010122     C                   ENDIF
025300010122     C                   ENDIF
025400010122     C*
025500940725     C                   ADD       1             NRR1
025600941031     C  N20              SETON                                        20        SFLDSP
025700940725     C                   WRITE     LRB5S01
025800930525     C                   END
025900930525     C*
026000990805     C     KARB1         READE     FNEVB000                               30
026100930525     C                   END
026200101111     C*
026300101202     c                   setoff                                       44
026400101111     C* sente se presenti dei records con 'CET' x Esiti con il PDA
026500101203     C* solo in ambiente di Filiale.
026600101206     c************       if        knsif <> 'GAITRA201'
026700101206     C*
026800101206     c                   if        pct_OPEN_IND = '0'
026900101206     c                   open(e)   Fipct01L
027000130409     c                   open(e)   Fidst01l
027100101206     c                   if        Pct_STATUS = 0
027200101206     C*
027300101206     c************       if        knsif <> 'GAITRA201'
027400101111     c                   eval        PCTAAS =  PARAAS
027500101111     c                   eval        PCTLNP =  VIDLNP
027600101111     c                   eval        PCTNRS =  VIDNRS
027700101111     c                   eval        PCTNSP =  VIDNSP
027800101111     c                   eval        PCTTRD = 'CET'
027900101111     C*
028000101202     C     KCET01        chain     fiPCT01l
028100101202     c                   if        %found(fiPCT01l)
028200130409     c     kdst          chain     fidst01l
028300130411     c                   if        %found(fidst01l)
028400130411     c                   movel     dstflr        ddstflr
028500130411     c                   if        §DSTTSTPDA = *blank or
028600130411     c                             §DSTTSTPDA = 'O'
028700101111     c                   setoN                                        44
028800130411     c                   end
028900130409     c                   end
029000101206     c**********         end
029100101202     c                   end
029200101206     C*
029300101206     c                   close     Fipct01L
029400130409     c                   close     Fidst01L
029500101206     c                   end
029600101206     c                   end
029700930525     C*
029800930525     C*** E M E T T O    S U B F I L E
029900940726     C******     NRR1      IFGT 0
030000940725     C                   Z-ADD     1             REC1
030100940725     C                   WRITE     LRB5T01                                      INTESTAZIONE VIDEO
030200940725     C                   WRITE     LRB5Z01                                      TASTI FUNZIONALI
030300940726     C  N20              WRITE     LRB5D02                                      SFL VUOTO, MESSAGGIO
030400110201     c     video         tag
030500940725     C                   EXFMT     LRB5C01
030600941109     C*
030700120921     c* F3 - fine
030800941109     C   KC              MOVEL     '1'           PARF03                         PREMUTO F3=FINE
030900941109     C   KC              MOVEL     PARAM         KPJBU
031000941109     C*
031100120921     c* f12 - rimuovo param in kpjbu
031200120921     C   KL              MOVEL     PARAM         KPJBU
031300120921     c
031400110201     c                   clear                   torna_aVideo      1
031500120921     c* f05 - Esisti PDa
031600101111     C   KE              exsr      esitiPDA
031700110201     c                   if        torna_aVideo ='S'
031800110201     c                   goto      video
031900110201     c                   end
032000101111     C*
032100930525     C* CMD12 O ENTER --> RITORNO
032200940726     C******               END
032300930525     C*
032400930525     C                   RETURN
032500930525     C                   END
032600940726     C**************************************************************************
032700101111     C*    F5 ->  ESITI PDA
032800940726     C**************************************************************************
032900101111     C     esitiPDA      BEGSR
033000020920      *
033100101111     C                   clear                   fidg46ds
033200101111     C                   Z-ADD     VIDlna        dg46fgsi
033300101111     C                   Z-ADD     PCTndc        dg46ndci
033400101111     C                   Z-ADD     PCTCODAUT     dg46codaut
033500101111     C                   Z-ADD     PARaas        dg46aasi
033600101111     C                   Z-ADD     VIDLNP        dg46LNPi
033700101111     C                   Z-ADD     VIDNSP        dg46NSPi
033800101111     C                   Z-ADD     VIDNRS        dg46NRSi
033900101111     C                   MOVEL(P)  fidg46ds      KPJBU
034000101111     C                   CALL      'FIDG49R'
034100101111     c                   PARM                    KPJBA
034200101111     c                   movel     kpjbu         fidg46ds
034300101111     C                   if        dg46erro = '1'
034400101111     c                   end
034500110201     C                   if        dg46erro = '2'
034600110201     c                   eval      torna_aVideo ='S'
034700110201     c                   end
034800101111      *
034900101111     C                   ENDsr
035000101111     C**************************************************************************
035100101111     C*    CARICO P.O. presenti sull'as di sede
035200101111     C**************************************************************************
035300130227     **    CARPO         BEGSR
035400101111      *
035500130227     c*                  clear                   tibs56ds
035600130227     c*                  eval      I56ppo = simfel
035700130227     c*                  eval      I56mod = 'POA'
035800130227     c*                  call      'TIBS56R'
035900130227     c*                  parm                    tibs56ds
036000130227     c*                  moveA     o56ski        POG
036100020920      *
036200130227     C*                  ENDSR
036300020920     C**************************************************************************
036400020920     C*    CARICO TABELLA FILIALI GESTITE £1
036500020920     C**************************************************************************
036600130227     C*    CAR£1         BEGSR
036700020920     C*
036800130227     c*                  clear                   trul06ds
036900130227     c*                  eval      d06cod = '£1'
037000130227     c*                  movel     simfel        d06key
037100130227     c*                  movel     trul06ds      kpjbu
037200130227     c*                  call      'TRUL06R'
037300130227     c*                  parm                    kpjba
037400130227     c*                  movel     kpjbu         trul06ds
037500130227     c*                  moveA     D06lia        POG
037600020920      * Reimposto la kpjbu con i dati della param
037700130227     c*                  movel     param         kpjbu
037800020920     C*
037900130227     C*                  ENDSR
038000940726     C**************************************************************************
038100940726     C*    R O U T I N E     I N I Z I A L E
038200940726     C**************************************************************************
038300940726     C     *INZSR        BEGSR
038400940726     C*
038500940726     C     KARB1         KLIST
038600990805     C                   KFLD                    KAAS2                          ..ANNO DI 4 CIFRE
038700940726     C                   KFLD                    VIDLNP
038800940726     C                   KFLD                    VIDNRS
038900940726     C                   KFLD                    VIDNSP
039000940726     C     KTAB1         KLIST
039100940726     C                   KFLD                    CODUT
039200940726     C                   KFLD                    KCOD
039300940726     C                   KFLD                    KKEY
039400021014      *
039500070427     C     KTAB1Z        KLIST
039600070427     C                   KFLD                    CODUT
039700070427     C                   KFLD                    KCODZ
039800070427     C                   KFLD                    KKEY
039900070427      *
040000140226     C     Kevb2         KLIST
040100070430     C                   KFLD                    EVBAAS
040200070430     C                   KFLD                    EVBLNP
040300070430     C                   KFLD                    EVBNRS
040400070430     C                   KFLD                    EVBNSP
040500070430     C                   KFLD                    EVBDTV
040600070430     C                   KFLD                    EVBORV
040700070430     C                   KFLD                    EVBCEV
040800101111      *
040900101111     C     KCET01        klist
041000101111     C                   KFLD                    PCTAAS
041100101111     C                   KFLD                    PCTLNP
041200101111     C                   KFLD                    PCTNRS
041300101111     C                   KFLD                    PCTNSP
041400101111     C                   KFLD                    PCTTRD
041500130409     C     Kdst          klist
041600130409     C                   KFLD                    npg
041700130409     C                   KFLD                    PCTndc
041800130409     C                   KFLD                    PCTfgs
041900070427      *
042000940726     C                   ENDSR
