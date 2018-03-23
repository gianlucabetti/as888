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
056400971021     C                   MOVE      VHBCAS        WASC15
056500971023     C                   TESTN                   WI15                 30
056600971023     C  N30              TESTN                   WI15                   30
056700971023     C  N30              TESTN                   WI15                     30
056800971021     C                   MOVE      WI15          W001A             1
056900971021     C     *IN30         IFEQ      *OFF
057000971021     C     W001A         ORLT      '0'
057100971023     C     W001A         ANDNE     *BLANK
057200971021     C                   SETON                                        32
057300971021     C                   MOVE      *ZERO         VABCAS
057400010604     C                   eval      vinmsg = %trimr(vinmsg)
057500010604     C                             + ' ' + 'VHBCAS'
057600971021     C                   ELSE
057700971021     C                   MOVEL     WI15          VABCAS
057800971023     C                   TESTN                   WD15                 30
057900971023     C  N30              TESTN                   WD15                   30
058000971023     C  N30              TESTN                   WD15                     30
058100971021     C                   MOVE      WD15          W001A             1
058200971021     C     *IN30         IFEQ      *OFF
058300971021     C     W001A         ORLT      '0'
058400971023     C     W001A         ANDNE     *BLANK
058500971021     C                   SETON                                        32
058600971021     C                   MOVE      *ZERO         VABCAS
058700010604     C                   eval      vinmsg = %trimr(vinmsg)
058800010604     C                             + ' ' + 'VHBCAS'
058900971021     C                   ELSE
059000971021     C                   MOVE      WD15          VABCAS
059100971021     C                   ENDIF
059200050118     C                   ENDIF
059300971021     C*
059400971023     C                   TESTN                   VHBRMN               30
059500971023     C  N30              TESTN                   VHBRMN                 30
059600971023     C  N30              TESTN                   VHBRMN                   30
059700971021     C                   MOVE      VHBRMN        W001A             1
059800971021     C     *IN30         IFEQ      *OFF
059900971021     C     W001A         ORLT      '0'
060000971021     C                   SETON                                        32
060100971021     C                   MOVE      *ZERO         VABRMN
060200010604     C                   eval      vinmsg = %trimr(vinmsg)
060300010604     C                             + ' ' + 'VHBRMN'
060400971021     C                   ELSE
060500971021     C                   MOVE      VHBRMN        VABRMN
060600971021     C                   ENDIF
060700971021     C*
060800971023     C                   TESTN                   VHBNCD               30
060900971023     C  N30              TESTN                   VHBNCD                 30
061000971023     C  N30              TESTN                   VHBNCD                   30
061100971021     C                   MOVE      VHBNCD        W001A             1
061200971021     C     *IN30         IFEQ      *OFF
061300971021     C     W001A         ORLT      '0'
061400971023     C     W001A         ANDNE     *BLANK
061500971021     C                   SETON                                        32
061600971021     C                   MOVE      *ZERO         VABNCD
061700010604     C                   eval      vinmsg = %trimr(vinmsg)
061800010604     C                             + ' ' + 'VHBNCD'
061900971021     C                   ELSE
062000971021     C                   MOVE      VHBNCD        VABNCD
062100971021     C                   ENDIF
062200971021     C*
062300971023     C                   TESTN                   VHBNCA               30
062400971023     C  N30              TESTN                   VHBNCA                 30
062500971023     C  N30              TESTN                   VHBNCA                   30
062600971021     C                   MOVE      VHBNCA        W001A             1
062700971021     C     *IN30         IFEQ      *OFF
062800971021     C     W001A         ORLT      '0'
062900971023     C     W001A         ANDNE     *BLANK
063000971021     C                   SETON                                        32
063100971021     C                   MOVE      *ZERO         VABNCA
063200010604     C                   eval      vinmsg = %trimr(vinmsg)
063300010604     C                             + ' ' + 'VHBNCA'
063400971021     C                   ELSE
063500971021     C                   MOVE      VHBNCA        VABNCA
063600971021     C                   ENDIF
063700971021     C*
063800971023     C                   TESTN                   VHBZNC               30
063900971023     C  N30              TESTN                   VHBZNC                 30
064000971023     C  N30              TESTN                   VHBZNC                   30
064100971021     C                   MOVE      VHBZNC        W001A             1
064200971021     C     *IN30         IFEQ      *OFF
064300971021     C     W001A         ORLT      '0'
064400971023     C     W001A         ANDNE     *BLANK
064500971021     C                   SETON                                        32
064600971021     C                   MOVE      *ZERO         VABZNC
064700010604     C                   eval      vinmsg = %trimr(vinmsg)
064800010604     C                             + ' ' + 'VHBZNC'
064900971021     C                   ELSE
065000971021     C                   MOVE      VHBZNC        VABZNC
065100971021     C                   ENDIF
065200971021     C*
065300971023     C                   TESTN                   VHBDCR               30
065400971023     C  N30              TESTN                   VHBDCR                 30
065500971023     C  N30              TESTN                   VHBDCR                   30
065600971021     C                   MOVE      VHBDCR        W001A             1
065700971021     C     *IN30         IFEQ      *OFF
065800971021     C     W001A         ORLT      '0'
065900971023     C     W001A         ANDNE     *BLANK
066000971021     C                   SETON                                        32
066100971021     C                   MOVE      *ZERO         VABDCR
066200010604     C                   eval      vinmsg = %trimr(vinmsg)
066300010604     C                             + ' ' + 'VHBDCR'
066400971021     C                   ELSE
066500971021     C                   MOVE      VHBDCR        VABDCR
066600971021     C                   ENDIF
066700971021     C*
066800971023     C                   TESTN                   VHBHCR               30
066900971023     C  N30              TESTN                   VHBHCR                 30
067000971023     C  N30              TESTN                   VHBHCR                   30
067100971021     C                   MOVE      VHBHCR        W001A             1
067200971021     C     *IN30         IFEQ      *OFF
067300971021     C     W001A         ORLT      '0'
067400971023     C     W001A         ANDNE     *BLANK
067500971021     C                   SETON                                        32
067600971021     C                   MOVE      *ZERO         VABHCR
067700010604     C                   eval      vinmsg = %trimr(vinmsg)
067800010604     C                             + ' ' + 'VHBHCR'
067900971021     C                   ELSE
068000971021     C                   MOVE      VHBHCR        VABHCR
068100971021     C                   ENDIF
068200941229     C*
068300971021     C                   MOVE      *ZERO         VABVMD
068400971021     C                   MOVE      VHBVMD        WASC15
068500971023     C                   TESTN                   WI15                 30
068600971023     C  N30              TESTN                   WI15                   30
068700971023     C  N30              TESTN                   WI15                     30
068800971021     C                   MOVE      WI15          W001A             1
068900971021     C     *IN30         IFEQ      *OFF
069000971021     C     W001A         ORLT      '0'
069100971023     C     W001A         ANDNE     *BLANK
069200971021     C                   SETON                                        32
069300971021     C                   MOVE      *ZERO         VABVMD
069400010604     C                   eval      vinmsg = %trimr(vinmsg)
069500010604     C                             + ' ' + 'VHBVMD'
069600971021     C                   ELSE
069700971021     C                   MOVEL     WI15          VABVMD
069800971023     C                   TESTN                   WD15                 30
069900971023     C  N30              TESTN                   WD15                   30
070000971023     C  N30              TESTN                   WD15                     30
070100971021     C                   MOVE      WD15          W001A             1
070200971021     C     *IN30         IFEQ      *OFF
070300971021     C     W001A         ORLT      '0'
070400971023     C     W001A         ANDNE     *BLANK
070500971021     C                   SETON                                        32
070600971021     C                   MOVE      *ZERO         VABVMD
070700010604     C                   eval      vinmsg = %trimr(vinmsg)
070800010604     C                             + ' ' + 'VHBVMD'
070900971021     C                   ELSE
071000971021     C                   MOVE      WD15          VABVMD
071100971021     C                   ENDIF
071200050118     C                   ENDIF
071300971021     C*
071400971023     C                   TESTN                   VHBANT               30
071500971023     C  N30              TESTN                   VHBANT                 30
071600971023     C  N30              TESTN                   VHBANT                   30
071700971021     C                   MOVE      VHBANT        W001A             1
071800971021     C     *IN30         IFEQ      *OFF
071900971021     C     W001A         ORLT      '0'
072000971023     C     W001A         ANDNE     *BLANK
072100971021     C                   SETON                                        32
072200971021     C                   MOVE      *ZERO         VABANT
072300010604     C                   eval      vinmsg = %trimr(vinmsg)
072400010604     C                             + ' ' + 'VHBANT'
072500971021     C                   ELSE
072600971021     C                   MOVE      VHBANT        VABANT
072700971021     C                   ENDIF
072800971021     C*
072900971021     C* CAMPI ALFABETICI:
073000971021     C                   MOVE      VHBATB        VABATB
073100971021     C                   MOVE      VHBCBO        VABCBO
073200971021     C                   MOVE      VHBRSD        VABRSD
073300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
073400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
073500020117     C* ==
073600971021     C                   MOVE      VHBRD2        VABRD2
073700971021     C                   MOVE      VHBIND        VABIND
073800971021     C                   MOVE      VHBCAD        VABCAD
073900971021     C                   MOVE      VHBLOD        VABLOD
074000971021     C                   MOVE      VHBPRD        VABPRD
074100971021     C                   MOVE      VHBNZD        VABNZD
074200971021     C                   MOVE      VHBGC1        VABGC1
074300971021     C                   MOVE      VHBGC2        VABGC2
074400971021     C                   MOVE      VHBTSP        VABTSP
074500971021     C                   MOVE      VHBVAS        VABVAS
074600971021     C                   MOVE      VHBNAS        VABNAS
074700971021     C                   MOVE      VHBTIC        VABTIC
074800971021     C                   MOVE      VHBVCA        VABVCA
074900971021     C                   MOVE      VHBGCA        VABGCA
075000971021     C                   MOVE      VHBRMA        VABRMA
075100971021     C                   MOVE      VHBXCO        VABXCO
075200971021     C                   MOVE      VHBNOT        VABNOT
075300971021     C                   MOVE      VHBNT2        VABNT2
075400971021     C                   MOVE      VHBCTM        VABCTM
075500971021     C                   MOVE      VHBFFD        VABFFD
075600971021     C                   MOVE      VHBTCR        VABTCR
075700971021     C                   MOVE      VHBCTS        VABCTS
075800971021     C                   MOVE      VHBFTM        VABFTM
075900971021     C                   MOVE      VHBVAD        VABVAD
076000971021     C                   MOVE      VHBGMA        VABGMA
076100971021     C                   MOVE      VHBGGA        VABGGA
076200971021     C                   MOVE      VHBGVA        VABGVA
076300971021     C                   MOVE      VHBTC1        VABTC1
076400971021     C                   MOVE      VHBTC2        VABTC2
076500971021     C                   MOVE      VHBSCL        VABSCL
076600971021     C                   MOVE      VHBRMO        VABRMO
076700971021     C                   MOVE      VHBCMO        VABCMO
076800971021     C                   MOVE      VHBNMO        VABNMO
076900971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
077000971021     C* NELLA NAZIONE ORIGINALE
077100971021     C                   MOVE      VHBNMO        WA1D              1
077200971021     C                   MOVEL     VHBNMO        WA1S              1
077300971021     C     WA1D          IFNE      *BLANK
077400971021     C     WA1S          ANDEQ     *BLANK
077500971021     C                   MOVE      *BLANK        VABNMO
077600040227     C                   ENDIF
077700031229     C*
077800031229     C* Se il codice trattamento merce nn viene passato dal cliente lo valorizzo con il valore
077900031229     C* d default passato come parametro dall'anagrafica clienti/tipo file
078000031229     C                   IF        vabCTM = *blanks
078100031229     C                   EVAL      vabCTM = wCTM
078200031229     C                   ENDIF
078300031229     C*
078400031229     C* Verifica codice trattamento merce: utilizzabili solo i '6C' '6D' '7P' (x mantenimento
078500031229     C* numero d spedizione)
078600040302     C*                  IF        vabCTM = '6C' OR
078700040302     C*                            vabCTM = '6D' OR
078800040302     C*                            vabCTM = '7P'
078900040302     C*                  ELSE
079000040302     C*                  SETON                                        31
079100040302     C*                  EVAL      vinmsg = %trimr(vinmsg)
079200040302     C*                            + ' ' + 'VABCTM(non valido)'
079300040302     C*                  ENDIF
079400040227     C*
079500040227     C* Se presente particolarità consegna 'RC' e se il codice
079600040227     C* cliente mittente è valorizzato e numero serie ORM nn passata
079700040227     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
079800040227     C                   IF        VABGMA = 'RC'
079900040227     C                   IF        VABCCM > *ZEROS
080000040227     C                   IF        wNSR = *BLANKS
080100040227     C                   Z-ADD     1             jOSR
080200080502     C                   move(p)   VABLNP        wLnpOSR
080300080502     C                   move(p)   VABCCM        wCliOSR
080400080502     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
080500040227     C                   IF        %found
080600040227     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
080700040227     C                   EVAL      %subst(vlrppt:4:2) = wNSR
080800040227     C                   ELSE
080900040227     C                   SETON                                        31
081000040227     C                   EVAL      vinmsg = %trimr(vinmsg)
081100040227     C                             + ' ' + 'ORMNRS(tabellaOSR)'
081200040227     C                   ENDIF
081300040227     C                   ENDIF
081400040227     C                   ENDIF
081500040227     C                   ENDIF
081600041025     C*
081700041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
081800041025     C                   IF        *IN52 = *ON
081900041025     C* Reperisco la provincia dal CAP e dalla località
082000041025     C                   IF        VABCAD <> *blanks AND
082100050907     C                             VABPRD  = *blanks AND
082200050907     C                             VABNZD  = *blanks
082300041025     C                   CLEAR                   TISI95DS
082400041025     C                   EVAL      I95TCN = '3'
082500041025     C                   Z-ADD     datcor        I95DAT
082600041025     C                   EVAL      I95CAP = VABCAD
082700041025     C                   EVAL      I95LOC = VABLOD
082800041025     C                   CALL      'TISI95R'
082900041025     C                   PARM                    TISI95DS
083000041025     C                   EVAL      VABPRD = O95PRV
083100041025     C                   ENDIF
083200041025     C                   ENDIF
083300930409     C*
083400010604     C  N31              ADD       1             §CTROK            7 0
083500010604     C   32              ADD       1             §CTRMO            7 0
083600010604     C   31              ADD       1             §CTRNO            7 0
083700020916     C  N31              WRITE     FIVAB000
083800030811     C*
083900040108     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
084000030811     C                   if        *IN31 = *OFF AND
084100030811     C                             vabGMA = 'RC'
084200040923     C*
084300040923     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
084400040923     C                   seton                                        54
084500040108     C*
084600040108     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
084700040108     C                   eval      wfivabds = fivabds
084800040920     C                   call      'TITV84R'
084900040108     C                   parm      *blanks       InTipl            1
085000040108     C                   parm                    tivlrds
085100040108     C                   parm                    wfivabds
085200040108     C*
085300030811     C                   endif
085400990910     C*
085500010601     C                   if        *in31 = *off and
085600010601     C                             *in32 = *off
085700990910     C                   eval      vinflg = '1'
085800990910     C                   else
085900000621     C                   eval      recko = vindta
086000031229     C                   if        %open(titv82ps)
086100030407     C   50              except    rigadett
086200010612     C                   endif
086300990910     C                   eval      vinflg = '2'
086400990910     C                   endif
086500000905     C                   endif
086600000905     C                   endif
086700010601     C*
086800010601     C                   endif
086900000905     C*
087000000905     C                   else
087100000905     C                   eval      vinflg = '1'
087200000905     C                   endif
087300000905     C*
087400000905     C  N70              update    tivin000
087500000905     C*
087600991022     C  N70              ENDdo
087700010601     C
087800010601     C*
087900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
088000010601     C                   if        cntNonEl = *zeros or
088100010601     C                             flgMulti = '0'
088200990910     C* Se non ci sono record con errori ...
088300010601     C                   if        §ctrno = 0 and
088400010601     C                             §ctrmo = 0 and
088500010601     C                             flgStato <> '2'
088600990910     C* ... restituisco esito OK.
088700990921     C                   eval      wrkesito = '0'
088800990910     C                   else
088900010601     C                   if        §ctrok > 0
089000000710     C                   eval      wrkesito = '1'
089100000710     C                   else
089200010615     C                   if        flgOk = '0'
089300010615     C                   eval      wrkesito = '2'
089400010615     C                   else
089500010615     C                   eval      wrkesito = '6'
089600010615     C                   endif
089700000710     C                   endif
089800010615     C                   endif
089900010601     C                   else
090000010601     C                   eval      wrkesito = '9'
090100010601     C                   endif
090200990910     C*
090300990914     C                   if        %open(tivin00r)
090400990908     C                   close     tivin00r
090500990914     C                   endif
090600020916     C                   if        %open(fivabwwr)
090700020916     C                   close     fivabwwr
090800990914     C                   endif
090900010601     C*
091000010601     C                   if        vlrpoi <> 999
091100010601     C                   eval      invfil = vlrpoi
091200010601     C                   endif
091300990910     C*
091400990920     C                   if        §ctrok > 0
091500010601     C                             and invfil > *zeros
091600990920     C                   exsr      invio
091700990920     C                   endif
091800010612     C*
091900010612     C                   if        flgGiro = '1'
092000040108     C                   exsr      endela
092100010612     C                   endif
092200990920     C*
092300910830     C                   ENDSR
092400000613     C***
092500030811
092600030811
092700030811
092800010601
092900010601
093000010601
093100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
093200010601     C     repfil        BEGSR
093300010601     C*
093400010601     C                   if        invfil = *zeros and
093500010601     C                             depfil > *zeros and
093600010613     C                             (vinflg = *blanks or
093700010613     C                              vinflg = *zeros)
093800010601     C
093900010601     C                   eval      invfil = depfil
094000010601     C                   endif
094100010601     C*
094200010601     C                   if        depfil <> invfil and
094300010601     C                             invfil > *zeros
094400010601     C                   eval      flgMulti = '1'
094500010601     C                   if        vinflg = *blanks
094600010601     C                   add       1             cntNonEl
094700010601     C                   endif
094800010601     C                   endif
094900010601     C*
095000010601     C                   if        vinflg = '2'
095100010601     C                   eval      flgStato = '2'
095200010601     C                   endif
095300010601     C*
095400010601     C                   ENDSR
095500010601     C***
095600010601
095700010601
095800030811
095900010601
096000990920      /TITLE Invio dei dati al punto operativo.
096100000613     C     invio         BEGSR
096200990920     C*
096300990920     C                   reset                   dscmz
096400010601     C                   move      invfil        cmzdst
096500990920     C                   eval      cmzfld = vlrfou
096600990920     C                   eval      cmzmbd = vlrhdl
096700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
096800000710     C                   if        prmfir = *blanks
096900020916     C                   eval      cmzfla = 'FIVAB00F'
097000020916     C                   eval      cmzmba = 'FIVAB00F'
097100000710     C                   else
097200000710     C                   eval      cmzfla = prmfir
097300000710     C                   eval      cmzmba = prmfir
097400000710     C                   endif
097500990920     C                   eval      cmznrr = *zeros
097600990920     C                   move      §ctrok        cmznrr
097700021003     C                   eval      cmzlba = vlrfl1
097800990920     C                   call(e)   'TIS711C'
097900990920     C                   parm                    dscmz
098000990921     C                   parm      *blanks       esito
098100990920     C                   if        %error
098200990920     C                             or cmzerr = '1'
098300990921     C                             or esito  = '1'
098400000710     C                   eval      wrkesito = '3'
098500990920     C                   endif
098600990920     C*
098700000613     C                   ENDSR
098800010601
098900010601
099000010601
099100010601
099200031229      /TITLE Operazioni iniziali e impostazoini d default
099300010601     C     opeini        BEGSR
099400010601     C*
099500010601     C* Inizializzo flag e contatori operativi
099600010601     C                   movel     '0'           flgGiro           1
099700010601     C                   movel     '0'           flgMulti          1
099800010601     C                   movel     '1'           flgStato          1
099900010615     C                   movel     '0'           flgOk             1
100000010601     C                   z-add     *zeros        cntNonEl         10 0
100100010601     C                   z-add     *zeros        depfil            3 0
100200010601     C                   z-add     *zeros        invfil            3 0
100300030407     C*
100400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
100500030407     C                   if        %subst(vlrppt:1:1) = 'S'
100600030407     C                   seton                                        50
100700030407     C                   else
100800030407     C                   setoff                                       50
100900030407     C                   endif
101000041001     C*
101100041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
101200041001     C                   if        %subst(vlrppt:26:1) = 'S'
101300041001     C                   seton                                        51
101400041001     C                   else
101500041001     C                   setoff                                       51
101600041001     C                   endif
101700041025     C*
101800041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
101900041025     C* (naturalmente solo se nn già presente)
102000041025     C                   if        %subst(vlrppt:27:1) = 'S'
102100041025     C                   seton                                        52
102200041025     C                   else
102300041025     C                   setoff                                       52
102400041025     C                   endif
102500031229     C*
102600031229     C* Verifico se impostato il parametro relativo al codice trattamento merce (VABCTM)
102700031229     C                   if        %subst(vlrppt:2:2) <> *blanks
102800031229     C                   eval      wCTM = %subst(vlrppt:2:2)
102900031229     C                   endif
103000031229     C*
103100031229     C* Verifico se impostato il parametro relativo al numero serie orm (ORMNSR)
103200031229     C                   if        %subst(vlrppt:4:2) <> *blanks
103300031229     C                   eval      wNSR = %subst(vlrppt:4:2)
103400031229     C                   endif
103500031229     C*
103600031229     C* Verifico se impostato il parametro relativo al codice consegna (ORMCRC)
103700031229     C                   if        %subst(vlrppt:6:10) <> *blanks
103800031229     C                   eval      wCRC = %subst(vlrppt:6:10)
103900031229     C                   endif
104000031229     C*
104100031229     C* Verifico se impostato il parametro relativo al codice pagante (ORMKSC)
104200031229     C                   if        %subst(vlrppt:16:7) <> *blanks
104300031229     C                   eval      wKSC = %subst(vlrppt:16:7)
104400031229     C                   endif
104500031229     C*
104600031229     C* Verifico se impostato il parametro relativo al codice pagante (ORMCTR)
104700031229     C                   if        %subst(vlrppt:23:3) <> *blanks
104800031229     C                   eval      wCTR = %subst(vlrppt:23:3)
104900031229     C                   endif
105000010601     C*
105100010601     C                   ENDSR
105200010601     C***
105300040302
105400040302
105500040302
105600080502     C*--------------------------------------------------------
105700080502     C* CARTAB - CARICAMENTO TABELLE                          *
105800080502     C*--------------------------------------------------------
105900080502     C     CARTAB        BEGSR
106000080502     C*
106100080502     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
106200080502     C                   z-add     *zeros        jOSR              3 0
106300080502     C     'OSR'         setll     tntbe01l
106400080502     C                   if        %equal(tntbe01l)
106500080502     C     'OSR'         reade     tntbe01l
106600080502     C                   dow       not %eof(tntbe01l)
106700080502     C                   eval      DOSR = tbeUNI
106800080502     c*
106900080502     c* Verifica e forzatura x nuovi campi aggiunti
107000080502     c                   if        aD§OSRcl6 = *blanks
107100080502     c                   eval      D§OSRcl6 = *zeros
107200080502     c                   endif
107300080502     c                   if        aD§OSRcl7 = *blanks
107400080502     c                   eval      D§OSRcl7 = *zeros
107500080502     c                   endif
107600080502     c                   if        aD§OSRcl8 = *blanks
107700080502     c                   eval      D§OSRcl8 = *zeros
107800080502     c                   endif
107900080502     c                   if        aD§OSRcl9 = *blanks
108000080502     c                   eval      D§OSRcl9 = *zeros
108100080502     c                   endif
108200080502     c                   if        aD§OSRcl10 = *blanks
108300080502     c                   eval      D§OSRcl10 = *zeros
108400080502     c                   endif
108500080502     c                   if        aD§OSRcl11 = *blanks
108600080502     c                   eval      D§OSRcl11 = *zeros
108700080502     c                   endif
108800080502     c                   if        aD§OSRcl12 = *blanks
108900080502     c                   eval      D§OSRcl12 = *zeros
109000080502     c                   endif
109100080502     c                   if        aD§OSRcl13 = *blanks
109200080502     c                   eval      D§OSRcl13 = *zeros
109300080502     c                   endif
109400080502     c                   if        aD§OSRcl14 = *blanks
109500080502     c                   eval      D§OSRcl14 = *zeros
109600080502     c                   endif
109700080502     c                   if        aD§OSRcl15 = *blanks
109800080502     c                   eval      D§OSRcl15 = *zeros
109900080502     c                   endif
110000080502     c                   if        aD§OSRcl16 = *blanks
110100080502     c                   eval      D§OSRcl16 = *zeros
110200080502     c                   endif
110300080502     c*
110400080502     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
110500080502     C                   if        D§OSRCLI <> *zeros
110600080502     C                   add       1             jOSR
110700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
110800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
110900080502     C                   movel     D§OSRCLI      wCliOSR
111000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
111100080502     C                   endif
111200080502     C*
111300080502     C                   if        D§OSRCL2 <> *zeros
111400080502     C                   add       1             jOSR
111500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
111600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
111700080502     C                   movel     D§OSRCL2      wCliOSR
111800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
111900080502     C                   endif
112000080502     C*
112100080502     C                   if        D§OSRCL3 <> *zeros
112200080502     C                   add       1             jOSR
112300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
112400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
112500080502     C                   movel     D§OSRCL3      wCliOSR
112600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
112700080502     C                   endif
112800080502     C*
112900080502     C                   if        D§OSRCL4 <> *zeros
113000080502     C                   add       1             jOSR
113100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
113200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
113300080502     C                   movel     D§OSRCL4      wCliOSR
113400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
113500080502     C                   endif
113600080502     C*
113700080502     C                   if        D§OSRCL5 <> *zeros
113800080502     C                   add       1             jOSR
113900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
114000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
114100080502     C                   movel     D§OSRCL5      wCliOSR
114200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
114300080502     C                   endif
114400080502     C*
114500080502     C                   if        D§OSRCL6 <> *zeros
114600080502     C                   add       1             jOSR
114700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
114800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
114900080502     C                   movel     D§OSRCL6      wCliOSR
115000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
115100080502     C                   endif
115200080502     C*
115300080502     C                   if        D§OSRCL7 <> *zeros
115400080502     C                   add       1             jOSR
115500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
115600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
115700080502     C                   movel     D§OSRCL7      wCliOSR
115800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
115900080502     C                   endif
116000080502     C*
116100080502     C                   if        D§OSRCL8 <> *zeros
116200080502     C                   add       1             jOSR
116300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
116400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
116500080502     C                   movel     D§OSRCL8      wCliOSR
116600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
116700080502     C                   endif
116800080502     C*
116900080502     C                   if        D§OSRCL9 <> *zeros
117000080502     C                   add       1             jOSR
117100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
117200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
117300080502     C                   movel     D§OSRCL9      wCliOSR
117400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
117500080502     C                   endif
117600080502     C*
117700080502     C                   if        D§OSRCL10 <> *zeros
117800080502     C                   add       1             jOSR
117900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
118000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
118100080502     C                   movel     D§OSRCL10     wCliOSR
118200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
118300080502     C                   endif
118400080502     C*
118500080502     C                   if        D§OSRCL11 <> *zeros
118600080502     C                   add       1             jOSR
118700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
118800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
118900080502     C                   movel     D§OSRCL11     wCliOSR
119000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
119100080502     C                   endif
119200080502     C*
119300080502     C                   if        D§OSRCL12 <> *zeros
119400080502     C                   add       1             jOSR
119500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
119600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
119700080502     C                   movel     D§OSRCL12     wCliOSR
119800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
119900080502     C                   endif
120000080502     C*
120100080502     C                   if        D§OSRCL13 <> *zeros
120200080502     C                   add       1             jOSR
120300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
120400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
120500080502     C                   movel     D§OSRCL13     wCliOSR
120600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
120700080502     C                   endif
120800080502     C*
120900080502     C                   if        D§OSRCL14 <> *zeros
121000080502     C                   add       1             jOSR
121100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
121200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
121300080502     C                   movel     D§OSRCL14     wCliOSR
121400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
121500080502     C                   endif
121600080502     C*
121700080502     C                   if        D§OSRCL15 <> *zeros
121800080502     C                   add       1             jOSR
121900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
122000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
122100080502     C                   movel     D§OSRCL15     wCliOSR
122200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
122300080502     C                   endif
122400080502     C*
122500080502     C                   if        D§OSRCL16 <> *zeros
122600080502     C                   add       1             jOSR
122700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
122800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
122900080502     C                   movel     D§OSRCL16     wCliOSR
123000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
123100080502     C                   endif
123200080502     C*
123300080502     C     'OSR'         reade     tntbe01l
123400080502     C                   enddo
123500080502     C                   endif
123600080502     C*
123700080502     C                   ENDSR
123800080502     C***
123900010601
124000010601
124100990910
124200000613     C     *inzsr        BEGSR
124300990910     C*
124400990910     C     *entry        plist
124500990920     C                   parm                    tivlrds
124600990921     C                   parm      wrkesito      esito
124700000724     C                   parm                    prmlit
124800000710     C                   parm                    prmfir
124900040227     C*
125000040227     C* CARICO LE TABELLE OCCORRENTI
125100040227     C                   exsr      CARTAB
125200010606     C*
125300010606     C* CALCOLA LA DATA CORRENTE
125400010606     C                   time                    wn14             14 0
125500010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
125600030811     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
125700010606     C                   z-add     wn8           g08dat
125800010606     C                   z-add     *zeros        g08inv
125900010606     C                   movel     '0'           g08err
126000010606     C                   call      'XSRDA8'
126100010606     C                   parm                    wlbda8
126200010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
126300000613     C*
126400000613     C                   ENDSR
126500000613     C***
126600990908
126700031229     Otitv82p   E            riepilogo         2
126800990915     O                                              'Upload via Internet'
126900990915     O                                           +1 'Traduzione TIVIN00R -'
127000020916     O                                           +1 'FIVABWWR'
127100990915     O                                           +1 'Testate bolle'
127200990915     O          E            riepilogo   2
127300990915     O                       wrkdata
127400990915     O                       wrkora              +1
127500990915     O                       procname            +1
127600990915     O          E            riepilogo   2
127700990915     O                                              'Cliente..................:'
127800990915     O                       VABCCM        z     +1
127900990915     O          E            riepilogo   2
128000990920     O                                              'Riferimento Strategi.....:'
128100990920     O                       vlrhdl              +1
128200990915     O          E            riepilogo   2
128300990915     O                                              'Giusti...................:'
128400971022     O                       §CTROK        2   +  1
128500990915     O          E            riepilogo   2
128600990915     O                                              'Sbagliati e corretti.....:'
128700971022     O                       §CTRMO        2   +  1
128800990915     O          E            riepilogo   2
128900990915     O                                              'Sbagliati e scartati.....:'
129000971022     O                       §CTRNO        2   +  1
129100000613
129200031229     Otitv82ps  E            testdett          2
129300000613     O                                              'Upload via Internet'
129400000613     O                                           +1 'Traduzione TIVIN00R -'
129500020916     O                                           +1 'FIVABWWR'
129600000613     O                                           +1 'Testate bolle'
129700000616     O          E            testdett    3
129800000613     O                                           +2 'N° rec'
129900000613     O                                           +3 'Anteprima contenuto'
130000000616     O          E            rigadett    2
130100000613     O                       rrnum               +2
130200000621     O                       recko               +3
130300000616     O          E            findett     2
130400000613     O                       wrkdata
130500000613     O                       wrkora              +1
130600000613     O                       procname            +1
130700000616     O          E            findett     2
130800000613     O                                              'Cliente..................:'
130900000613     O                       VABCCM        z     +1
131000000616     O          E            findett     2
131100000613     O                                              'Riferimento Strategi.....:'
131200000613     O                       vlrhdl              +1
131300000616     O          E            findett     2
131400000613     O                                              'Giusti...................:'
131500000613     O                       §CTROK        2   +  1
131600000616     O          E            findett     2
131700000613     O                                              'Sbagliati e corretti.....:'
131800000613     O                       §CTRMO        2   +  1
131900000616     O          E            findett     2
132000000613     O                                              'Sbagliati e scartati.....:'
132100000613     O                       §CTRNO        2   +  1
132200000613** CMD - COMANDI CL
132300031229OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV82P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
132400031229OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV82PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
132500031229DLTOVR FILE(TITV82P TITV82PS) LVL(*)
132600000613
132700000613
