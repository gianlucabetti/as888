000100020916      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400040227     Ftntbe01l  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700020916     FFIVABwwr  O    E             DISK    usropn
000800031229     Ftitv82p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000031229     Ftitv82ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000724     D*------------
001800030811     D* DS ESTERNE
001900030811     D*------------
002000040108     D kpjba         e ds                  inz
002100040108     D fivabds       e ds                  extname(fivab00f)
002200040227     D tivlrds       e ds                  extname(tivlr00f)
002300041025     D tisi95ds      e ds
002400040227     D fnvabaar      e ds                  inz
002500030811     D dscmz         e ds                  inz
002600040227     D dosr          e ds                  inz
002700080502     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
002800080502     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
002900080502     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
003000080502     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
003100080502     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
003200080502     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
003300080502     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
003400080502     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
003500080502     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
003600080502     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
003700080502     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
003800990910     D psds           sds
003900990910     D  procname         *PROC
004000030811     D*------------
004100030811     D* DS INTERNE
004200030811     D*------------
004300941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
004400941229     D* 10+3
004500040302     D                 DS
004600040302     D  WS15                   1      1
004700040302     D  WI15                   2     11
004800040302     D  WV15                  12     12
004900040302     D  WD15                  13     15
005000040302     D  WASC15                 1     15
005100040302     D*  6+1
005200040302     D                 DS
005300040302     D  WS09                   1      1
005400040302     D  WI09                   2      7
005500040302     D  WV09                   8      8
005600040302     D  WD09                   9      9
005700040302     D  WASC09                 1      9
005800040302     D*  2+3
005900040302     D                 DS
006000040302     D  WS07                   1      1
006100040302     D  WI07                   2      3
006200040302     D  WV07                   4      4
006300040302     D  WD07                   5      7
006400040302     D  WASC07                 1      7
006500000313     D*
006600990910     D esito           s              1
006700000724     D prmlit          s             10
006800000710     D prmfir          s             10
006900990921     D wrkesito        s                   like(esito)
007000990915     D wrkdata         s               d
007100990915     D wrkora          s               t
007200000613     D rrnum           s              6  0 INZ(*zeros)
007300000621     D recko           s            150    INZ(*blanks)
007400011123     D depcmd          s            150    INZ(*blanks)
007500031229     D wCTM            s                   LIKE(vabCTM)
007600040227     D wNSR            s              2    INZ
007700040227     D wCRC            s             10    INZ
007800040227     D wKSC            s              7    INZ
007900040227     D wCTR            s              3    INZ
008000040108     D wfivabds        s                   LIKE(fivabds)
008100010606     D*------------------
008200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
008300010606     D*------------------
008400010606     D WLBDA8          DS                  INZ
008500010606     D  G08DAT                 1      8  0
008600010606     D  G08INV                 9     16  0
008700010606     D  G08ERR                17     17
008800010606     D  G08TGI                18     22  0
008900080502     D*------------------
009000080502     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
009100080502     D*------------------
009200080502     D SkCliOSR        s             10    DIM(900)
009300080502     D*
009400080502     D DsCliOSR        ds
009500080502     D  wLnpOSR                       3
009600080502     D  wCliOSR                       7
009700080502     D*
009800080502     D SkLnpNrsOSR     s              5    DIM(900)
009900010606
010000010606
010100010606
010200010606
010300990915     C                   time                    wrkdata
010400990915     C                   time                    wrkora
010500000913     C                   reset                   rrnum
010600990921     C                   reset                   esito
010700990921     C                   reset                   wrkesito
010800010601     C*
010900010601     C                   exsr      opeini
011000010601     C                   exsr      rwvab
011100041025     C*
011200041025     C* Esegue lancio TISI95R solo x chiusura
011300041025     C                   CLEAR                   TISI95DS
011400041025     C                   EVAL      I95TLA = 'C'
011500041025     C                   CALL      'TISI95R'
011600041025     C                   PARM                    TISI95DS
011700010601     C*
011800010601     C                   seton                                        lr
011900010601
012000010601
012100010601
012200010601
012300010601     C*--------------------------------------------------------
012400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012500010601     C*--------------------------------------------------------
012600010601     C     PREELA        BEGSR
012700010601     C*
012800010601     C* SE OCCORRE SPEDIRE IN FILIALE
012900010601     C                   if        invfil <> *zeros and
013000010601     C                             flgGiro = '0'
013100010601     C*
013200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013300010601     C                   eval      flgGiro = '1'
013400000724     C*
013500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013700010601     C     invfil        chain     azorg01l
013800000724     C                   if        %found
013900000724     C                   movel(p)  CMD(1)        depcmd
014000020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
014100000724     C*
014200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014300011123     C                   Z-ADD     150           LENGH            15 5
014400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014500000724     C                   PARM                    depcmd
014600000724     C                   PARM                    LENGH
014700000724     C*
014800000724     C                   endif
014900000616     C*
015000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015100000616     C                   movel(p)  CMD(2)        depcmd
015200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015300000616     C*
015400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015500011123     C                   Z-ADD     150           LENGH            15 5
015600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015700000616     C                   PARM                    depcmd
015800000616     C                   PARM                    LENGH
015900000613     C*
016000031229     C                   if        not %open(titv82ps)
016100031229     C                   open      titv82ps
016200030407     C   50              except    testdett
016300000613     C                   endif
016400010601     C*
016500010601     C                   endif
016600010601     C*
016700010601     C                   ENDSR
016800010601     C***
016900010601
017000010601
017100010601
017200010601
017300010601
017400010601
017500010601     C*--------------------------------------------------------
017600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017700010601     C*--------------------------------------------------------
017800010601     C     ENDELA        BEGSR
017900990915     C*
018000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
018100000613     C*
018200031229     C                   if        %open(titv82ps)
018300030407     C   50              except    findett
018400031229     C                   close     titv82ps
018500000613     C                   endif
018600000616     C*
018700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018800011123     C                   Z-ADD     150           LENGH            15 5
018900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019000000616     C                   PARM                    CMD(3)
019100000616     C                   PARM                    LENGH
019200040108     C*
019300040108     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
019400040923     C   54              call(e)   'TITV84R'
019500040108     C                   parm      'C'           InTipl
019600040108     C                   parm                    tivlrds
019700040108     C                   parm                    wfivabds
019800010601     C*
019900010601     C                   ENDSR
020000010601     C***
020100010601
020200010601
020300000613
020400000613
020500000613
020600000613     C*--------------------------------------------------------
020700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020800000613     C*--------------------------------------------------------
020900000621     C     STPR          BEGSR
021000000613     C*
021100031229     C                   if        not %open(titv82p)
021200031229     C                   open      titv82p
021300990915     C                   endif
021400990915     C*
021500030407     C   50              except    riepilogo
021600990915     C*
021700031229     C                   if        %open(titv82p)
021800031229     C                   close     titv82p
021900990914     C                   endif
022000000613     C*
022100000613     C                   ENDSR
022200000613     C***
022300000613
022400010601
022500010601
022600010601
022700010601
022800910830     C*--------------------------------------------------------
022900020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
023000910830     C*--------------------------------------------------------
023100941229     C     RWVAB         BEGSR
023200990910     C*
023300990914     C                   if        not %open(tivin00r)
023400990908     C                   open      tivin00r
023500990914     C                   endif
023600020916     C                   if        not %open(fivabwwr)
023700020916     C                   open      fivabwwr
023800990914     C                   endif
023900990910     C*
024000990910     C                   clear                   §CTROK
024100990910     C                   clear                   §CTRMO
024200990910     C                   clear                   §CTRNO
024300990910     C*
024400921023     C                   DO        *HIVAL
024500990913     C*
024600990915     C                   READ      tivin00r                               70
024700010618     C*
024800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024900010618     C                   if        vinflg = '1'
025000010618     C                   eval      flgOk = '1'
025100010618     C                   endif
025200010618     C*
025300000905     C                   if        vindta > *blanks
025400000613     C                   add       1             rrnum
025500990913     C*
025600010601     C                   if        *in70 = *off and
025700990913     C                             (vinflg = *blanks
025800010711     C                              or vinflg = '0'
025900010711     C                              or vinflg = '2')
026000990913     C*
026100020916     C                   clear                   FIVAB000
026200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026300010711     C                   if        vinflg = *blanks or vinflg = '0'
026400010711     C                   clear                   vinmsg
026500010711     C                   endif
026600010711     C*
026700040109     C                   eval      fnvabaar = vindta
026800991022     C*
026900991022     C                   testb     x'3F'         vhbatb                   02
027000991022     C                   if        not *in02
027100991022     C*
027200971021     C                   SETOFF                                       303132
027300971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
027400971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
027500971023     C*    OPPURE TUTTI BLANKS
027600010601     C*
027700010601     C                   TESTN                   VHBLNP               30
027800010601     C  N30              TESTN                   VHBLNP                 30
027900010601     C  N30              TESTN                   VHBLNP                   30
028000010601     C                   MOVE      VHBLNP        W001A             1
028100010601     C     *IN30         IFEQ      *OFF
028200010601     C     W001A         ORLT      '0'
028300010601     C                   SETON                                        31
028400010601     C                   eval      vinmsg = %trimr(vinmsg)
028500010601     C                             + ' ' + 'VHBLNP'
028600010601     C                   ELSE
028700010601     C                   MOVE      VHBLNP        VABLNP
028800010601     C     VABLNP        IFLE      *ZEROS
028900010601     C                   SETON                                        31
029000010601     C                   eval      vinmsg = %trimr(vinmsg)
029100010601     C                             + ' ' + 'VHBLNP'
029200010601     C                   ENDIF
029300010601     C                   ENDIF
029400010601     C*
029500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029600010601     C                   eval      depfil = VABLNP
029700010601     C                   exsr      repfil
029800010601     C                   if        depfil = invfil
029900021014     C                   if        vlrpoi = 999
030000021010     C                   MOVE(P)   invfil        VABFGS
030100021014     C                   else
030200021014     C                   MOVE(P)   vlrpoi        VABFGS
030300021014     C                   endif
030400010601     C*
030500010601     C                   exsr      PREELA
030600010601     C*
030700971023     C                   TESTN                   VHBCCM               30
030800971023     C  N30              TESTN                   VHBCCM                 30
030900971023     C  N30              TESTN                   VHBCCM                   30
031000971021     C                   MOVE      VHBCCM        W001A             1
031100971021     C     *IN30         IFEQ      *OFF
031200971021     C     W001A         ORLT      '0'
031300971021     C                   SETON                                        31
031400010604     C                   eval      vinmsg = %trimr(vinmsg)
031500010604     C                             + ' ' + 'VHBCCM'
031600971021     C                   ELSE
031700971021     C                   MOVE      VHBCCM        VABCCM
031800971024     C     VABCCM        IFLE      *ZEROS
031900971024     C                   SETON                                        31
032000010604     C                   eval      vinmsg = %trimr(vinmsg)
032100010604     C                             + ' ' + 'VHBCCM'
032200971024     C                   ENDIF
032300971021     C                   ENDIF
032400971021     C*
032500971023     C                   TESTN                   VHBAAS               30
032600971023     C  N30              TESTN                   VHBAAS                 30
032700971023     C  N30              TESTN                   VHBAAS                   30
032800971021     C                   MOVE      VHBAAS        W001A             1
032900971021     C     *IN30         IFEQ      *OFF
033000971021     C     W001A         ORLT      '0'
033100971023     C     W001A         ANDNE     *BLANK
033200971021     C                   SETON                                        32
033300010606     C                   MOVEL     datcor        VABAAS
033400010604     C                   eval      vinmsg = %trimr(vinmsg)
033500010604     C                             + ' ' + 'VHBAAS'
033600971021     C                   ELSE
033700971021     C                   MOVE      VHBAAS        VABAAS
033800971021     C                   ENDIF
033900971021     C*
034000971023     C                   TESTN                   VHBMGS               30
034100971023     C  N30              TESTN                   VHBMGS                 30
034200971023     C  N30              TESTN                   VHBMGS                   30
034300971021     C                   MOVE      VHBMGS        W001A             1
034400971021     C     *IN30         IFEQ      *OFF
034500971021     C     W001A         ORLT      '0'
034600971023     C     W001A         ANDNE     *BLANK
034700971021     C                   SETON                                        32
034800010601     C                   MOVE      '0101'        VABMGS
034900010604     C                   eval      vinmsg = %trimr(vinmsg)
035000010604     C                             + ' ' + 'VHBMGS'
035100971021     C                   ELSE
035200971021     C                   MOVE      VHBMGS        VABMGS
035300971021     C                   ENDIF
035400971021     C*
035500971023     C                   TESTN                   VHBNRS               30
035600971023     C  N30              TESTN                   VHBNRS                 30
035700971023     C  N30              TESTN                   VHBNRS                   30
035800971021     C                   MOVE      VHBNRS        W001A             1
035900971021     C     *IN30         IFEQ      *OFF
036000971021     C     W001A         ORLT      '0'
036100971023     C     W001A         ANDNE     *BLANK
036200971021     C                   SETON                                        32
036300971021     C                   MOVE      *ZERO         VABNRS
036400010604     C                   eval      vinmsg = %trimr(vinmsg)
036500010604     C                             + ' ' + 'VHBNRS'
036600971021     C                   ELSE
036700971021     C                   MOVE      VHBNRS        VABNRS
036800971021     C                   ENDIF
036900971021     C*
037000971023     C                   TESTN                   VHBNSP               30
037100971023     C  N30              TESTN                   VHBNSP                 30
037200971023     C  N30              TESTN                   VHBNSP                   30
037300971021     C                   MOVE      VHBNSP        W001A             1
037400971021     C     *IN30         IFEQ      *OFF
037500971021     C     W001A         ORLT      '0'
037600971023     C     W001A         ANDNE     *BLANK
037700971021     C                   SETON                                        32
037800971021     C                   MOVE      *ZERO         VABNSP
037900010604     C                   eval      vinmsg = %trimr(vinmsg)
038000010604     C                             + ' ' + 'VHBNSP'
038100971021     C                   ELSE
038200971021     C                   MOVE      VHBNSP        VABNSP
038300971021     C                   ENDIF
038400971021     C*
038500971023     C                   TESTN                   VHBLNA               30
038600971023     C  N30              TESTN                   VHBLNA                 30
038700971023     C  N30              TESTN                   VHBLNA                   30
038800971021     C                   MOVE      VHBLNA        W001A             1
038900971021     C     *IN30         IFEQ      *OFF
039000971021     C     W001A         ORLT      '0'
039100971023     C     W001A         ANDNE     *BLANK
039200971021     C                   SETON                                        32
039300971021     C                   MOVE      *ZERO         VABLNA
039400010604     C                   eval      vinmsg = %trimr(vinmsg)
039500010604     C                             + ' ' + 'VHBLNA'
039600971021     C                   ELSE
039700971021     C                   MOVE      VHBLNA        VABLNA
039800971021     C                   ENDIF
039900971021     C*
040000971023     C                   TESTN                   VHBCTR               30
040100971023     C  N30              TESTN                   VHBCTR                 30
040200971023     C  N30              TESTN                   VHBCTR                   30
040300971021     C                   MOVE      VHBCTR        W001A             1
040400971021     C     *IN30         IFEQ      *OFF
040500971021     C     W001A         ORLT      '0'
040600971023     C     W001A         ANDNE     *BLANK
040700971021     C                   SETON                                        32
040800971021     C                   MOVE      *ZERO         VABCTR
040900010604     C                   eval      vinmsg = %trimr(vinmsg)
041000010604     C                             + ' ' + 'VHBCTR'
041100971021     C                   ELSE
041200971021     C                   MOVE      VHBCTR        VABCTR
041300971021     C                   ENDIF
041400941229     C*
041500971021     C                   MOVE      *ZERO         VABIAS
041600941229     C                   MOVE      VHBIAS        WASC15
041700971023     C                   TESTN                   WI15                 30
041800971023     C  N30              TESTN                   WI15                   30
041900971023     C  N30              TESTN                   WI15                     30
042000971021     C                   MOVE      WI15          W001A             1
042100971021     C     *IN30         IFEQ      *OFF
042200971021     C     W001A         ORLT      '0'
042300971023     C     W001A         ANDNE     *BLANK
042400971021     C                   SETON                                        32
042500971021     C                   MOVE      *ZERO         VABIAS
042600010604     C                   eval      vinmsg = %trimr(vinmsg)
042700010604     C                             + ' ' + 'VHBIAS'
042800971021     C                   ELSE
042900971021     C                   MOVEL     WI15          VABIAS
043000971023     C                   TESTN                   WD15                 30
043100971023     C  N30              TESTN                   WD15                   30
043200971023     C  N30              TESTN                   WD15                     30
043300971021     C                   MOVE      WD15          W001A             1
043400971021     C     *IN30         IFEQ      *OFF
043500971021     C     W001A         ORLT      '0'
043600971023     C     W001A         ANDNE     *BLANK
043700971021     C                   SETON                                        32
043800971021     C                   MOVE      *ZERO         VABIAS
043900010604     C                   eval      vinmsg = %trimr(vinmsg)
044000010604     C                             + ' ' + 'VHBIAS'
044100971021     C                   ELSE
044200971021     C                   MOVE      WD15          VABIAS
044300971021     C                   ENDIF
044400050118     C                   ENDIF
044500941229     C*
044600971023     C                   TESTN                   VHBNCL               30
044700971023     C  N30              TESTN                   VHBNCL                 30
044800971023     C  N30              TESTN                   VHBNCL                   30
044900971021     C                   MOVE      VHBNCL        W001A             1
045000971021     C     *IN30         IFEQ      *OFF
045100971021     C     W001A         ORLT      '0'
045200971021     C                   SETON                                        32
045300971021     C                   MOVE      *ZERO         VABNCL
045400010604     C                   eval      vinmsg = %trimr(vinmsg)
045500010604     C                             + ' ' + 'VHBNCL'
045600971021     C                   ELSE
045700971021     C                   MOVE      VHBNCL        VABNCL
045800971021     C                   ENDIF
045900941229     C*
046000971021     C                   MOVE      *ZERO         VABPKB
046100971021     C                   MOVE      VHBPKB        WASC09
046200050503     C                   IF        WI09 = *blanks
046300050503     C                   EVAL      WI09 = *zeros
046400050503     C                   ENDIF
046500971023     C                   TESTN                   WI09                 30
046600971023     C  N30              TESTN                   WI09                   30
046700971023     C  N30              TESTN                   WI09                     30
046800971021     C                   MOVE      WI09          W001A             1
046900971021     C     *IN30         IFEQ      *OFF
047000971021     C     W001A         ORLT      '0'
047100971021     C                   SETON                                        32
047200971021     C                   MOVE      *ZERO         VABPKB
047300010604     C                   eval      vinmsg = %trimr(vinmsg)
047400010604     C                             + ' ' + 'VHBPKB'
047500971021     C                   ELSE
047600971021     C                   MOVEL     WI09          VABPKB
047700971023     C                   TESTN                   WD09                 30
047800971023     C  N30              TESTN                   WD09                     30
047900971021     C                   MOVE      WD09          W001A             1
048000971021     C     *IN30         IFEQ      *OFF
048100971021     C     W001A         ORLT      '0'
048200971023     C     W001A         ANDNE     *BLANK
048300971021     C                   SETON                                        32
048400971021     C                   MOVE      *ZERO         VABPKB
048500010604     C                   eval      vinmsg = %trimr(vinmsg)
048600010604     C                             + ' ' + 'VHBPKB'
048700971021     C                   ELSE
048800971021     C                   MOVE      WD09          VABPKB
048900971021     C                   ENDIF
049000050118     C                   ENDIF
049100971021     C*
049200041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
049300041001     C                   IF        *IN51 = *OFF
049400971021     C                   MOVE      *ZERO         VABVLB
049500971021     C                   MOVE      VHBVLB        WASC07
049600050503     C                   IF        WI07 = *blanks
049700050503     C                   EVAL      WI07 = *zeros
049800050503     C                   ENDIF
049900971023     C                   TESTN                   WI07                 30
050000971023     C  N30              TESTN                   WI07                   30
050100971023     C  N30              TESTN                   WI07                     30
050200971021     C                   MOVE      WI07          W001A             1
050300971021     C     *IN30         IFEQ      *OFF
050400971021     C     W001A         ORLT      '0'
050500971023     C     W001A         ANDNE     *BLANK
050600971021     C                   SETON                                        32
050700971021     C                   MOVE      *ZERO         VABVLB
050800010604     C                   eval      vinmsg = %trimr(vinmsg)
050900010604     C                             + ' ' + 'VHBVLB'
051000971021     C                   ELSE
051100971021     C                   MOVEL     WI07          VABVLB
051200971023     C                   TESTN                   WD07                 30
051300971023     C  N30              TESTN                   WD07                   30
051400971023     C  N30              TESTN                   WD07                     30
051500971021     C                   MOVE      WD07          W001A             1
051600971021     C     *IN30         IFEQ      *OFF
051700971021     C     W001A         ORLT      '0'
051800971023     C     W001A         ANDNE     *BLANK
051900971021     C                   SETON                                        32
052000971021     C                   MOVE      *ZERO         VABVLB
052100010604     C                   eval      vinmsg = %trimr(vinmsg)
052200010604     C                             + ' ' + 'VHBVLB'
052300971021     C                   ELSE
052400971021     C                   MOVE      WD07          VABVLB
052500971021     C                   ENDIF
052600050118     C                   ENDIF
052700041001     C                   ELSE
052800041001     C* ...altrimenti forzo azzeramento del volume
052900041001     C                   Z-ADD     *zeros        VABVLB
053000041001     C                   ENDIF
053100941229     C*
053200971021     C                   MOVE      *ZERO         VABQFT
053300971021     C                   MOVE      VHBQFT        WASC15
053400971023     C                   TESTN                   WI15                 30
053500971023     C  N30              TESTN                   WI15                   30
053600971023     C  N30              TESTN                   WI15                     30
053700971021     C                   MOVE      WI15          W001A             1
053800971021     C     *IN30         IFEQ      *OFF
053900971021     C     W001A         ORLT      '0'
054000971023     C     W001A         ANDNE     *BLANK
054100971021     C                   SETON                                        32
054200971021     C                   MOVE      *ZERO         VABQFT
054300010604     C                   eval      vinmsg = %trimr(vinmsg)
054400010604     C                             + ' ' + 'VHBQFT'
054500971021     C                   ELSE
054600971021     C                   MOVEL     WI15          VABQFT
054700971023     C                   TESTN                   WD15                 30
054800971023     C  N30              TESTN                   WD15                   30
054900971023     C  N30              TESTN                   WD15                     30
055000971021     C                   MOVE      WD15          W001A             1
055100971021     C     *IN30         IFEQ      *OFF
055200971021     C     W001A         ORLT      '0'
055300971023     C     W001A         ANDNE     *BLANK
055400971021     C                   SETON                                        32
055500971021     C                   MOVE      *ZERO         VABQFT
055600010604     C                   eval      vinmsg = %trimr(vinmsg)
055700010604     C                             + ' ' + 'VHBQFT'
055800971021     C                   ELSE
055900971021     C                   MOVE      WD15          VABQFT
056000971021     C                   ENDIF
056100050118     C                   ENDIF
056200941229     C*
056300971021     C                   MOVE      *ZERO         VABCAS
056400070726     C* ......VABCAS
056500070726     C                   EVAL      PiStr=VHBCAS
056600070726     C                   EXSR      CHKNUM
056700070726     C                   IF        PiNum=*on
056800070726     C                   Z-ADD     PiVal         VABCAS
056900070726     C                   ELSE
057000070726     C                   SETON                                        32
057100070726     C                   MOVE      *ZERO         VABQFT
057200070726     C                   EVAL      vinmsg = %trimr(vinmsg)
057300070726     C                             + ' ' + 'VABCAS'
057400070726     C                   ENDIF
057500971021     C*
057600971023     C                   TESTN                   VHBRMN               30
057700971023     C  N30              TESTN                   VHBRMN                 30
057800971023     C  N30              TESTN                   VHBRMN                   30
057900971021     C                   MOVE      VHBRMN        W001A             1
058000971021     C     *IN30         IFEQ      *OFF
058100971021     C     W001A         ORLT      '0'
058200971021     C                   SETON                                        32
058300971021     C                   MOVE      *ZERO         VABRMN
058400010604     C                   eval      vinmsg = %trimr(vinmsg)
058500010604     C                             + ' ' + 'VHBRMN'
058600971021     C                   ELSE
058700971021     C                   MOVE      VHBRMN        VABRMN
058800971021     C                   ENDIF
058900971021     C*
059000971023     C                   TESTN                   VHBNCD               30
059100971023     C  N30              TESTN                   VHBNCD                 30
059200971023     C  N30              TESTN                   VHBNCD                   30
059300971021     C                   MOVE      VHBNCD        W001A             1
059400971021     C     *IN30         IFEQ      *OFF
059500971021     C     W001A         ORLT      '0'
059600971023     C     W001A         ANDNE     *BLANK
059700971021     C                   SETON                                        32
059800971021     C                   MOVE      *ZERO         VABNCD
059900010604     C                   eval      vinmsg = %trimr(vinmsg)
060000010604     C                             + ' ' + 'VHBNCD'
060100971021     C                   ELSE
060200971021     C                   MOVE      VHBNCD        VABNCD
060300971021     C                   ENDIF
060400971021     C*
060500971023     C                   TESTN                   VHBNCA               30
060600971023     C  N30              TESTN                   VHBNCA                 30
060700971023     C  N30              TESTN                   VHBNCA                   30
060800971021     C                   MOVE      VHBNCA        W001A             1
060900971021     C     *IN30         IFEQ      *OFF
061000971021     C     W001A         ORLT      '0'
061100971023     C     W001A         ANDNE     *BLANK
061200971021     C                   SETON                                        32
061300971021     C                   MOVE      *ZERO         VABNCA
061400010604     C                   eval      vinmsg = %trimr(vinmsg)
061500010604     C                             + ' ' + 'VHBNCA'
061600971021     C                   ELSE
061700971021     C                   MOVE      VHBNCA        VABNCA
061800971021     C                   ENDIF
061900971021     C*
062000971023     C                   TESTN                   VHBZNC               30
062100971023     C  N30              TESTN                   VHBZNC                 30
062200971023     C  N30              TESTN                   VHBZNC                   30
062300971021     C                   MOVE      VHBZNC        W001A             1
062400971021     C     *IN30         IFEQ      *OFF
062500971021     C     W001A         ORLT      '0'
062600971023     C     W001A         ANDNE     *BLANK
062700971021     C                   SETON                                        32
062800971021     C                   MOVE      *ZERO         VABZNC
062900010604     C                   eval      vinmsg = %trimr(vinmsg)
063000010604     C                             + ' ' + 'VHBZNC'
063100971021     C                   ELSE
063200971021     C                   MOVE      VHBZNC        VABZNC
063300971021     C                   ENDIF
063400971021     C*
063500971023     C                   TESTN                   VHBDCR               30
063600971023     C  N30              TESTN                   VHBDCR                 30
063700971023     C  N30              TESTN                   VHBDCR                   30
063800971021     C                   MOVE      VHBDCR        W001A             1
063900971021     C     *IN30         IFEQ      *OFF
064000971021     C     W001A         ORLT      '0'
064100971023     C     W001A         ANDNE     *BLANK
064200971021     C                   SETON                                        32
064300971021     C                   MOVE      *ZERO         VABDCR
064400010604     C                   eval      vinmsg = %trimr(vinmsg)
064500010604     C                             + ' ' + 'VHBDCR'
064600971021     C                   ELSE
064700971021     C                   MOVE      VHBDCR        VABDCR
064800971021     C                   ENDIF
064900971021     C*
065000971023     C                   TESTN                   VHBHCR               30
065100971023     C  N30              TESTN                   VHBHCR                 30
065200971023     C  N30              TESTN                   VHBHCR                   30
065300971021     C                   MOVE      VHBHCR        W001A             1
065400971021     C     *IN30         IFEQ      *OFF
065500971021     C     W001A         ORLT      '0'
065600971023     C     W001A         ANDNE     *BLANK
065700971021     C                   SETON                                        32
065800971021     C                   MOVE      *ZERO         VABHCR
065900010604     C                   eval      vinmsg = %trimr(vinmsg)
066000010604     C                             + ' ' + 'VHBHCR'
066100971021     C                   ELSE
066200971021     C                   MOVE      VHBHCR        VABHCR
066300971021     C                   ENDIF
066400941229     C*
066500971021     C                   MOVE      *ZERO         VABVMD
066600971021     C                   MOVE      VHBVMD        WASC15
066700971023     C                   TESTN                   WI15                 30
066800971023     C  N30              TESTN                   WI15                   30
066900971023     C  N30              TESTN                   WI15                     30
067000971021     C                   MOVE      WI15          W001A             1
067100971021     C     *IN30         IFEQ      *OFF
067200971021     C     W001A         ORLT      '0'
067300971023     C     W001A         ANDNE     *BLANK
067400971021     C                   SETON                                        32
067500971021     C                   MOVE      *ZERO         VABVMD
067600010604     C                   eval      vinmsg = %trimr(vinmsg)
067700010604     C                             + ' ' + 'VHBVMD'
067800971021     C                   ELSE
067900971021     C                   MOVEL     WI15          VABVMD
068000971023     C                   TESTN                   WD15                 30
068100971023     C  N30              TESTN                   WD15                   30
068200971023     C  N30              TESTN                   WD15                     30
068300971021     C                   MOVE      WD15          W001A             1
068400971021     C     *IN30         IFEQ      *OFF
068500971021     C     W001A         ORLT      '0'
068600971023     C     W001A         ANDNE     *BLANK
068700971021     C                   SETON                                        32
068800971021     C                   MOVE      *ZERO         VABVMD
068900010604     C                   eval      vinmsg = %trimr(vinmsg)
069000010604     C                             + ' ' + 'VHBVMD'
069100971021     C                   ELSE
069200971021     C                   MOVE      WD15          VABVMD
069300971021     C                   ENDIF
069400050118     C                   ENDIF
069500971021     C*
069600971023     C                   TESTN                   VHBANT               30
069700971023     C  N30              TESTN                   VHBANT                 30
069800971023     C  N30              TESTN                   VHBANT                   30
069900971021     C                   MOVE      VHBANT        W001A             1
070000971021     C     *IN30         IFEQ      *OFF
070100971021     C     W001A         ORLT      '0'
070200971023     C     W001A         ANDNE     *BLANK
070300971021     C                   SETON                                        32
070400971021     C                   MOVE      *ZERO         VABANT
070500010604     C                   eval      vinmsg = %trimr(vinmsg)
070600010604     C                             + ' ' + 'VHBANT'
070700971021     C                   ELSE
070800971021     C                   MOVE      VHBANT        VABANT
070900971021     C                   ENDIF
071000971021     C*
071100971021     C* CAMPI ALFABETICI:
071200971021     C                   MOVE      VHBATB        VABATB
071300971021     C                   MOVE      VHBCBO        VABCBO
071400971021     C                   MOVE      VHBRSD        VABRSD
071500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
071600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
071700020117     C* ==
071800971021     C                   MOVE      VHBRD2        VABRD2
071900971021     C                   MOVE      VHBIND        VABIND
072000971021     C                   MOVE      VHBCAD        VABCAD
072100971021     C                   MOVE      VHBLOD        VABLOD
072200971021     C                   MOVE      VHBPRD        VABPRD
072300971021     C                   MOVE      VHBNZD        VABNZD
072400971021     C                   MOVE      VHBGC1        VABGC1
072500971021     C                   MOVE      VHBGC2        VABGC2
072600971021     C                   MOVE      VHBTSP        VABTSP
072700971021     C                   MOVE      VHBVAS        VABVAS
072800971021     C                   MOVE      VHBNAS        VABNAS
072900971021     C                   MOVE      VHBTIC        VABTIC
073000971021     C                   MOVE      VHBVCA        VABVCA
073100971021     C                   MOVE      VHBGCA        VABGCA
073200971021     C                   MOVE      VHBRMA        VABRMA
073300971021     C                   MOVE      VHBXCO        VABXCO
073400971021     C                   MOVE      VHBNOT        VABNOT
073500971021     C                   MOVE      VHBNT2        VABNT2
073600971021     C                   MOVE      VHBCTM        VABCTM
073700971021     C                   MOVE      VHBFFD        VABFFD
073800971021     C                   MOVE      VHBTCR        VABTCR
073900971021     C                   MOVE      VHBCTS        VABCTS
074000971021     C                   MOVE      VHBFTM        VABFTM
074100971021     C                   MOVE      VHBVAD        VABVAD
074200971021     C                   MOVE      VHBGMA        VABGMA
074300971021     C                   MOVE      VHBGGA        VABGGA
074400971021     C                   MOVE      VHBGVA        VABGVA
074500971021     C                   MOVE      VHBTC1        VABTC1
074600971021     C                   MOVE      VHBTC2        VABTC2
074700971021     C                   MOVE      VHBSCL        VABSCL
074800971021     C                   MOVE      VHBRMO        VABRMO
074900971021     C                   MOVE      VHBCMO        VABCMO
075000971021     C                   MOVE      VHBNMO        VABNMO
075100971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
075200971021     C* NELLA NAZIONE ORIGINALE
075300971021     C                   MOVE      VHBNMO        WA1D              1
075400971021     C                   MOVEL     VHBNMO        WA1S              1
075500971021     C     WA1D          IFNE      *BLANK
075600971021     C     WA1S          ANDEQ     *BLANK
075700971021     C                   MOVE      *BLANK        VABNMO
075800040227     C                   ENDIF
075900031229     C*
076000031229     C* Se il codice trattamento merce nn viene passato dal cliente lo valorizzo con il valore
076100031229     C* d default passato come parametro dall'anagrafica clienti/tipo file
076200031229     C                   IF        vabCTM = *blanks
076300031229     C                   EVAL      vabCTM = wCTM
076400031229     C                   ENDIF
076500031229     C*
076600031229     C* Verifica codice trattamento merce: utilizzabili solo i '6C' '6D' '7P' (x mantenimento
076700031229     C* numero d spedizione)
076800040302     C*                  IF        vabCTM = '6C' OR
076900040302     C*                            vabCTM = '6D' OR
077000040302     C*                            vabCTM = '7P'
077100040302     C*                  ELSE
077200040302     C*                  SETON                                        31
077300040302     C*                  EVAL      vinmsg = %trimr(vinmsg)
077400040302     C*                            + ' ' + 'VABCTM(non valido)'
077500040302     C*                  ENDIF
077600040227     C*
077700040227     C* Se presente particolarità consegna 'RC' e se il codice
077800040227     C* cliente mittente è valorizzato e numero serie ORM nn passata
077900040227     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
078000040227     C                   IF        VABGMA = 'RC'
078100040227     C                   IF        VABCCM > *ZEROS
078200040227     C                   IF        wNSR = *BLANKS
078300040227     C                   Z-ADD     1             jOSR
078400080502     C                   move(p)   VABLNP        wLnpOSR
078500080502     C                   move(p)   VABCCM        wCliOSR
078600080502     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
078700040227     C                   IF        %found
078800040227     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
078900040227     C                   EVAL      %subst(vlrppt:4:2) = wNSR
079000040227     C                   ELSE
079100040227     C                   SETON                                        31
079200040227     C                   EVAL      vinmsg = %trimr(vinmsg)
079300040227     C                             + ' ' + 'ORMNRS(tabellaOSR)'
079400040227     C                   ENDIF
079500040227     C                   ENDIF
079600040227     C                   ENDIF
079700040227     C                   ENDIF
079800041025     C*
079900041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
080000041025     C                   IF        *IN52 = *ON
080100041025     C* Reperisco la provincia dal CAP e dalla località
080200041025     C                   IF        VABCAD <> *blanks AND
080300050907     C                             VABPRD  = *blanks AND
080400050907     C                             VABNZD  = *blanks
080500041025     C                   CLEAR                   TISI95DS
080600041025     C                   EVAL      I95TCN = '3'
080700041025     C                   Z-ADD     datcor        I95DAT
080800041025     C                   EVAL      I95CAP = VABCAD
080900041025     C                   EVAL      I95LOC = VABLOD
081000041025     C                   CALL      'TISI95R'
081100041025     C                   PARM                    TISI95DS
081200041025     C                   EVAL      VABPRD = O95PRV
081300041025     C                   ENDIF
081400041025     C                   ENDIF
081500930409     C*
081600010604     C  N31              ADD       1             §CTROK            7 0
081700010604     C   32              ADD       1             §CTRMO            7 0
081800010604     C   31              ADD       1             §CTRNO            7 0
081900020916     C  N31              WRITE     FIVAB000
082000030811     C*
082100040108     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
082200030811     C                   if        *IN31 = *OFF AND
082300030811     C                             vabGMA = 'RC'
082400040923     C*
082500040923     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
082600040923     C                   seton                                        54
082700040108     C*
082800040108     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
082900040108     C                   eval      wfivabds = fivabds
083000040920     C                   call      'TITV84R'
083100040108     C                   parm      *blanks       InTipl            1
083200040108     C                   parm                    tivlrds
083300040108     C                   parm                    wfivabds
083400040108     C*
083500030811     C                   endif
083600990910     C*
083700010601     C                   if        *in31 = *off and
083800010601     C                             *in32 = *off
083900990910     C                   eval      vinflg = '1'
084000990910     C                   else
084100000621     C                   eval      recko = vindta
084200031229     C                   if        %open(titv82ps)
084300030407     C   50              except    rigadett
084400010612     C                   endif
084500990910     C                   eval      vinflg = '2'
084600990910     C                   endif
084700000905     C                   endif
084800000905     C                   endif
084900010601     C*
085000010601     C                   endif
085100000905     C*
085200000905     C                   else
085300000905     C                   eval      vinflg = '1'
085400000905     C                   endif
085500000905     C*
085600000905     C  N70              update    tivin000
085700000905     C*
085800991022     C  N70              ENDdo
085900010601     C
086000010601     C*
086100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
086200010601     C                   if        cntNonEl = *zeros or
086300010601     C                             flgMulti = '0'
086400990910     C* Se non ci sono record con errori ...
086500010601     C                   if        §ctrno = 0 and
086600010601     C                             §ctrmo = 0 and
086700010601     C                             flgStato <> '2'
086800990910     C* ... restituisco esito OK.
086900990921     C                   eval      wrkesito = '0'
087000990910     C                   else
087100010601     C                   if        §ctrok > 0
087200000710     C                   eval      wrkesito = '1'
087300000710     C                   else
087400010615     C                   if        flgOk = '0'
087500010615     C                   eval      wrkesito = '2'
087600010615     C                   else
087700010615     C                   eval      wrkesito = '6'
087800010615     C                   endif
087900000710     C                   endif
088000010615     C                   endif
088100010601     C                   else
088200010601     C                   eval      wrkesito = '9'
088300010601     C                   endif
088400990910     C*
088500990914     C                   if        %open(tivin00r)
088600990908     C                   close     tivin00r
088700990914     C                   endif
088800020916     C                   if        %open(fivabwwr)
088900020916     C                   close     fivabwwr
089000990914     C                   endif
089100010601     C*
089200010601     C                   if        vlrpoi <> 999
089300010601     C                   eval      invfil = vlrpoi
089400010601     C                   endif
089500990910     C*
089600990920     C                   if        §ctrok > 0
089700010601     C                             and invfil > *zeros
089800990920     C                   exsr      invio
089900990920     C                   endif
090000010612     C*
090100010612     C                   if        flgGiro = '1'
090200040108     C                   exsr      endela
090300010612     C                   endif
090400990920     C*
090500910830     C                   ENDSR
090600000613     C***
090700030811
090800030811
090900030811
091000010601
091100010601
091200010601
091300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
091400010601     C     repfil        BEGSR
091500010601     C*
091600010601     C                   if        invfil = *zeros and
091700010601     C                             depfil > *zeros and
091800010613     C                             (vinflg = *blanks or
091900010613     C                              vinflg = *zeros)
092000010601     C
092100010601     C                   eval      invfil = depfil
092200010601     C                   endif
092300010601     C*
092400010601     C                   if        depfil <> invfil and
092500010601     C                             invfil > *zeros
092600010601     C                   eval      flgMulti = '1'
092700010601     C                   if        vinflg = *blanks
092800010601     C                   add       1             cntNonEl
092900010601     C                   endif
093000010601     C                   endif
093100010601     C*
093200010601     C                   if        vinflg = '2'
093300010601     C                   eval      flgStato = '2'
093400010601     C                   endif
093500010601     C*
093600010601     C                   ENDSR
093700010601     C***
093800010601
093900010601
094000030811
094100010601
094200990920      /TITLE Invio dei dati al punto operativo.
094300000613     C     invio         BEGSR
094400990920     C*
094500990920     C                   reset                   dscmz
094600010601     C                   move      invfil        cmzdst
094700990920     C                   eval      cmzfld = vlrfou
094800990920     C                   eval      cmzmbd = vlrhdl
094900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
095000000710     C                   if        prmfir = *blanks
095100020916     C                   eval      cmzfla = 'FIVAB00F'
095200020916     C                   eval      cmzmba = 'FIVAB00F'
095300000710     C                   else
095400000710     C                   eval      cmzfla = prmfir
095500000710     C                   eval      cmzmba = prmfir
095600000710     C                   endif
095700990920     C                   eval      cmznrr = *zeros
095800990920     C                   move      §ctrok        cmznrr
095900021003     C                   eval      cmzlba = vlrfl1
096000990920     C                   call(e)   'TIS711C'
096100990920     C                   parm                    dscmz
096200990921     C                   parm      *blanks       esito
096300990920     C                   if        %error
096400990920     C                             or cmzerr = '1'
096500990921     C                             or esito  = '1'
096600000710     C                   eval      wrkesito = '3'
096700990920     C                   endif
096800990920     C*
096900000613     C                   ENDSR
097000010601
097100010601
097200010601
097300010601
097400031229      /TITLE Operazioni iniziali e impostazoini d default
097500010601     C     opeini        BEGSR
097600010601     C*
097700010601     C* Inizializzo flag e contatori operativi
097800010601     C                   movel     '0'           flgGiro           1
097900010601     C                   movel     '0'           flgMulti          1
098000010601     C                   movel     '1'           flgStato          1
098100010615     C                   movel     '0'           flgOk             1
098200010601     C                   z-add     *zeros        cntNonEl         10 0
098300010601     C                   z-add     *zeros        depfil            3 0
098400010601     C                   z-add     *zeros        invfil            3 0
098500030407     C*
098600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
098700030407     C                   if        %subst(vlrppt:1:1) = 'S'
098800030407     C                   seton                                        50
098900030407     C                   else
099000030407     C                   setoff                                       50
099100030407     C                   endif
099200041001     C*
099300041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
099400041001     C                   if        %subst(vlrppt:26:1) = 'S'
099500041001     C                   seton                                        51
099600041001     C                   else
099700041001     C                   setoff                                       51
099800041001     C                   endif
099900041025     C*
100000041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
100100041025     C* (naturalmente solo se nn già presente)
100200041025     C                   if        %subst(vlrppt:27:1) = 'S'
100300041025     C                   seton                                        52
100400041025     C                   else
100500041025     C                   setoff                                       52
100600041025     C                   endif
100700031229     C*
100800031229     C* Verifico se impostato il parametro relativo al codice trattamento merce (VABCTM)
100900031229     C                   if        %subst(vlrppt:2:2) <> *blanks
101000031229     C                   eval      wCTM = %subst(vlrppt:2:2)
101100031229     C                   endif
101200031229     C*
101300031229     C* Verifico se impostato il parametro relativo al numero serie orm (ORMNSR)
101400031229     C                   if        %subst(vlrppt:4:2) <> *blanks
101500031229     C                   eval      wNSR = %subst(vlrppt:4:2)
101600031229     C                   endif
101700031229     C*
101800031229     C* Verifico se impostato il parametro relativo al codice consegna (ORMCRC)
101900031229     C                   if        %subst(vlrppt:6:10) <> *blanks
102000031229     C                   eval      wCRC = %subst(vlrppt:6:10)
102100031229     C                   endif
102200031229     C*
102300031229     C* Verifico se impostato il parametro relativo al codice pagante (ORMKSC)
102400031229     C                   if        %subst(vlrppt:16:7) <> *blanks
102500031229     C                   eval      wKSC = %subst(vlrppt:16:7)
102600031229     C                   endif
102700031229     C*
102800031229     C* Verifico se impostato il parametro relativo al codice pagante (ORMCTR)
102900031229     C                   if        %subst(vlrppt:23:3) <> *blanks
103000031229     C                   eval      wCTR = %subst(vlrppt:23:3)
103100031229     C                   endif
103200010601     C*
103300010601     C                   ENDSR
103400010601     C***
103500040302
103600040302
103700040302
103800080502     C*--------------------------------------------------------
103900080502     C* CARTAB - CARICAMENTO TABELLE                          *
104000080502     C*--------------------------------------------------------
104100080502     C     CARTAB        BEGSR
104200080502     C*
104300080502     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
104400080502     C                   z-add     *zeros        jOSR              3 0
104500080502     C     'OSR'         setll     tntbe01l
104600080502     C                   if        %equal(tntbe01l)
104700080502     C     'OSR'         reade     tntbe01l
104800080502     C                   dow       not %eof(tntbe01l)
104900080502     C                   eval      DOSR = tbeUNI
105000080502     c*
105100080502     c* Verifica e forzatura x nuovi campi aggiunti
105200080502     c                   if        aD§OSRcl6 = *blanks
105300080502     c                   eval      D§OSRcl6 = *zeros
105400080502     c                   endif
105500080502     c                   if        aD§OSRcl7 = *blanks
105600080502     c                   eval      D§OSRcl7 = *zeros
105700080502     c                   endif
105800080502     c                   if        aD§OSRcl8 = *blanks
105900080502     c                   eval      D§OSRcl8 = *zeros
106000080502     c                   endif
106100080502     c                   if        aD§OSRcl9 = *blanks
106200080502     c                   eval      D§OSRcl9 = *zeros
106300080502     c                   endif
106400080502     c                   if        aD§OSRcl10 = *blanks
106500080502     c                   eval      D§OSRcl10 = *zeros
106600080502     c                   endif
106700080502     c                   if        aD§OSRcl11 = *blanks
106800080502     c                   eval      D§OSRcl11 = *zeros
106900080502     c                   endif
107000080502     c                   if        aD§OSRcl12 = *blanks
107100080502     c                   eval      D§OSRcl12 = *zeros
107200080502     c                   endif
107300080502     c                   if        aD§OSRcl13 = *blanks
107400080502     c                   eval      D§OSRcl13 = *zeros
107500080502     c                   endif
107600080502     c                   if        aD§OSRcl14 = *blanks
107700080502     c                   eval      D§OSRcl14 = *zeros
107800080502     c                   endif
107900080502     c                   if        aD§OSRcl15 = *blanks
108000080502     c                   eval      D§OSRcl15 = *zeros
108100080502     c                   endif
108200080502     c                   if        aD§OSRcl16 = *blanks
108300080502     c                   eval      D§OSRcl16 = *zeros
108400080502     c                   endif
108500080502     c*
108600080502     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
108700080502     C                   if        D§OSRCLI <> *zeros
108800080502     C                   add       1             jOSR
108900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
109000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
109100080502     C                   movel     D§OSRCLI      wCliOSR
109200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
109300080502     C                   endif
109400080502     C*
109500080502     C                   if        D§OSRCL2 <> *zeros
109600080502     C                   add       1             jOSR
109700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
109800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
109900080502     C                   movel     D§OSRCL2      wCliOSR
110000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
110100080502     C                   endif
110200080502     C*
110300080502     C                   if        D§OSRCL3 <> *zeros
110400080502     C                   add       1             jOSR
110500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
110600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
110700080502     C                   movel     D§OSRCL3      wCliOSR
110800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
110900080502     C                   endif
111000080502     C*
111100080502     C                   if        D§OSRCL4 <> *zeros
111200080502     C                   add       1             jOSR
111300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
111400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
111500080502     C                   movel     D§OSRCL4      wCliOSR
111600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
111700080502     C                   endif
111800080502     C*
111900080502     C                   if        D§OSRCL5 <> *zeros
112000080502     C                   add       1             jOSR
112100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
112200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
112300080502     C                   movel     D§OSRCL5      wCliOSR
112400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
112500080502     C                   endif
112600080502     C*
112700080502     C                   if        D§OSRCL6 <> *zeros
112800080502     C                   add       1             jOSR
112900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
113000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
113100080502     C                   movel     D§OSRCL6      wCliOSR
113200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
113300080502     C                   endif
113400080502     C*
113500080502     C                   if        D§OSRCL7 <> *zeros
113600080502     C                   add       1             jOSR
113700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
113800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
113900080502     C                   movel     D§OSRCL7      wCliOSR
114000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
114100080502     C                   endif
114200080502     C*
114300080502     C                   if        D§OSRCL8 <> *zeros
114400080502     C                   add       1             jOSR
114500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
114600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
114700080502     C                   movel     D§OSRCL8      wCliOSR
114800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
114900080502     C                   endif
115000080502     C*
115100080502     C                   if        D§OSRCL9 <> *zeros
115200080502     C                   add       1             jOSR
115300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
115400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
115500080502     C                   movel     D§OSRCL9      wCliOSR
115600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
115700080502     C                   endif
115800080502     C*
115900080502     C                   if        D§OSRCL10 <> *zeros
116000080502     C                   add       1             jOSR
116100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
116200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
116300080502     C                   movel     D§OSRCL10     wCliOSR
116400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
116500080502     C                   endif
116600080502     C*
116700080502     C                   if        D§OSRCL11 <> *zeros
116800080502     C                   add       1             jOSR
116900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
117000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
117100080502     C                   movel     D§OSRCL11     wCliOSR
117200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
117300080502     C                   endif
117400080502     C*
117500080502     C                   if        D§OSRCL12 <> *zeros
117600080502     C                   add       1             jOSR
117700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
117800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
117900080502     C                   movel     D§OSRCL12     wCliOSR
118000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
118100080502     C                   endif
118200080502     C*
118300080502     C                   if        D§OSRCL13 <> *zeros
118400080502     C                   add       1             jOSR
118500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
118600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
118700080502     C                   movel     D§OSRCL13     wCliOSR
118800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
118900080502     C                   endif
119000080502     C*
119100080502     C                   if        D§OSRCL14 <> *zeros
119200080502     C                   add       1             jOSR
119300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
119400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
119500080502     C                   movel     D§OSRCL14     wCliOSR
119600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
119700080502     C                   endif
119800080502     C*
119900080502     C                   if        D§OSRCL15 <> *zeros
120000080502     C                   add       1             jOSR
120100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
120200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
120300080502     C                   movel     D§OSRCL15     wCliOSR
120400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
120500080502     C                   endif
120600080502     C*
120700080502     C                   if        D§OSRCL16 <> *zeros
120800080502     C                   add       1             jOSR
120900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
121000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
121100080502     C                   movel     D§OSRCL16     wCliOSR
121200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
121300080502     C                   endif
121400080502     C*
121500080502     C     'OSR'         reade     tntbe01l
121600080502     C                   enddo
121700080502     C                   endif
121800080502     C*
121900080502     C                   ENDSR
122000080502     C***
122100070726
122200070726
122300070726     C*----------------------------------------------------*
122400070726     C*  CONTROLLO NUMERICITA' CAMPI
122500070726     C*----------------------------------------------------*
122600070726     C     CHKNUM        BEGSR
122700070726     C*
122800070726     C                   call(e)   'ISNUMERIC'
122900070726     C                   PARM                    PiStr            30
123000070726     C                   PARM      ','           PiDecChr          1
123100070726     C                   PARM      *ZEROS        PiVal            30 9
123200070726     C                   PARM      '0'           PiInt             1
123300070726     C                   PARM      '0'           PiNum             1
123400070726     C                   IF        %error
123500070726     C                   EVAL      PiInt=*off
123600070726     C                   ENDIF
123700070726     C*
123800070726     C                   ENDSR
123900070726     C***
124000010601
124100010601
124200990910
124300000613     C     *inzsr        BEGSR
124400990910     C*
124500990910     C     *entry        plist
124600990920     C                   parm                    tivlrds
124700990921     C                   parm      wrkesito      esito
124800000724     C                   parm                    prmlit
124900000710     C                   parm                    prmfir
125000040227     C*
125100040227     C* CARICO LE TABELLE OCCORRENTI
125200040227     C                   exsr      CARTAB
125300010606     C*
125400010606     C* CALCOLA LA DATA CORRENTE
125500010606     C                   time                    wn14             14 0
125600010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
125700030811     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
125800010606     C                   z-add     wn8           g08dat
125900010606     C                   z-add     *zeros        g08inv
126000010606     C                   movel     '0'           g08err
126100010606     C                   call      'XSRDA8'
126200010606     C                   parm                    wlbda8
126300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
126400000613     C*
126500000613     C                   ENDSR
126600000613     C***
126700990908
126800031229     Otitv82p   E            riepilogo         2
126900990915     O                                              'Upload via Internet'
127000990915     O                                           +1 'Traduzione TIVIN00R -'
127100020916     O                                           +1 'FIVABWWR'
127200990915     O                                           +1 'Testate bolle'
127300990915     O          E            riepilogo   2
127400990915     O                       wrkdata
127500990915     O                       wrkora              +1
127600990915     O                       procname            +1
127700990915     O          E            riepilogo   2
127800990915     O                                              'Cliente..................:'
127900990915     O                       VABCCM        z     +1
128000990915     O          E            riepilogo   2
128100990920     O                                              'Riferimento Strategi.....:'
128200990920     O                       vlrhdl              +1
128300990915     O          E            riepilogo   2
128400990915     O                                              'Giusti...................:'
128500971022     O                       §CTROK        2   +  1
128600990915     O          E            riepilogo   2
128700990915     O                                              'Sbagliati e corretti.....:'
128800971022     O                       §CTRMO        2   +  1
128900990915     O          E            riepilogo   2
129000990915     O                                              'Sbagliati e scartati.....:'
129100971022     O                       §CTRNO        2   +  1
129200000613
129300031229     Otitv82ps  E            testdett          2
129400000613     O                                              'Upload via Internet'
129500000613     O                                           +1 'Traduzione TIVIN00R -'
129600020916     O                                           +1 'FIVABWWR'
129700000613     O                                           +1 'Testate bolle'
129800000616     O          E            testdett    3
129900000613     O                                           +2 'N° rec'
130000000613     O                                           +3 'Anteprima contenuto'
130100000616     O          E            rigadett    2
130200000613     O                       rrnum               +2
130300000621     O                       recko               +3
130400000616     O          E            findett     2
130500000613     O                       wrkdata
130600000613     O                       wrkora              +1
130700000613     O                       procname            +1
130800000616     O          E            findett     2
130900000613     O                                              'Cliente..................:'
131000000613     O                       VABCCM        z     +1
131100000616     O          E            findett     2
131200000613     O                                              'Riferimento Strategi.....:'
131300000613     O                       vlrhdl              +1
131400000616     O          E            findett     2
131500000613     O                                              'Giusti...................:'
131600000613     O                       §CTROK        2   +  1
131700000616     O          E            findett     2
131800000613     O                                              'Sbagliati e corretti.....:'
131900000613     O                       §CTRMO        2   +  1
132000000616     O          E            findett     2
132100000613     O                                              'Sbagliati e scartati.....:'
132200000613     O                       §CTRNO        2   +  1
132300000613** CMD - COMANDI CL
132400031229OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV82P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
132500031229OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV82PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
132600031229DLTOVR FILE(TITV82P TITV82PS) LVL(*)
132700000613
132800000613
