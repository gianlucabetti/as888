000100040325      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200081202     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300021014
000400000724     Fazorg01l  if   e           k disk
000500990914     Ftivin00r  uF   E             DISK    usropn
000600040325     FEDIVATWR  O    E             DISK
000700040325     Ftis7tetp  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040325     Ftis7tetps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990914
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000724     D*------------
001700990921     D dscmz         e ds                  inz
001800990914     D psds           sds
001900990914     D  procname         *PROC
002000990921     D tivlrds       e ds                  extname(tivlr00f)
002100990914     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500990915     D wrkdata         s               d
002600990915     D wrkora          s               t
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800000621     D recko           s            150    INZ(*blanks)
002900011123     D depcmd          s            150    INZ(*blanks)
003000030616     D wNomeFile       s             30    INZ(*blanks)
003100030710     D wCMR            s             35    INZ(*blanks)
003200030710     D wPosDaA         s              2    INZ(*blanks)
003300030710     D wPosDa          s              2  0 INZ(*zeros)
003400030710     D wLungA          s              2    INZ(*blanks)
003500030710     D wLung           s              2  0 INZ(*zeros)
003600010606     D*------------------
003700010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
003800010606     D*------------------
003900010606     D WLBDA8          DS                  INZ
004000010606     D  G08DAT                 1      8  0
004100010606     D  G08INV                 9     16  0
004200010606     D  G08ERR                17     17
004300010606     D  G08TGI                18     22  0
004400040325     D*
004500040325     D fnvataar        ds                  inz
004600050614     D  a_vatatb                      1
004700050614     D  a_vatccm                      7
004800050614     D  a_vatlnp                      3
004900050614     D  a_vataas                      4
005000050614     D  a_vatnrs                      2
005100050614     D  a_vatnsp                      7
005200050614     D  a_vattrc                      1
005300050614     D  a_vatnot                     35
005400081202     D  a_vatrma                     15
005500010606
005600081202
005700081202     D*------------------
005800081202     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
005900081202     D*------------------
006000081202     D PiStr           S            100A   INZ
006100081202     D PiDecChr        S              1A   INZ
006200081202     D PiVal           S             63S30 INZ
006300081202     D PiInt           S               N   INZ
006400081202     D PiNum           S               N   INZ
006500081202
006600081202     D*------------------
006700081202     D* LINKING A DEFINIZIONI ESTERNE
006800081202     D*------------------
006900081202     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007000081202
007100010606
007200010606
007300990914
007400990915     C                   time                    wrkdata
007500990915     C                   time                    wrkora
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900010531     C*
008000010531     C                   exsr      opeini
008100040325     C                   exsr      rwvat
008200010531     C*
008300010531     C                   seton                                        lr
008400010531
008500010531
008600010531
008700010531
008800010531     C*--------------------------------------------------------
008900010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009000010531     C*--------------------------------------------------------
009100010531     C     PREELA        BEGSR
009200000724     C*
009300000724     C* SE OCCORRE SPEDIRE IN FILIALE
009400010531     C                   if        invfil <> *zeros and
009500010531     C                             flgGiro = '0'
009600010531     C*
009700010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009800010531     C                   eval      flgGiro = '1'
009900000724     C*
010000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010200040325     C     VATFGS        chain     azorg01l
010300000724     C                   if        %found
010400000724     C                   movel(p)  CMD(1)        depcmd
010500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010600000724     C*
010700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010800011123     C                   Z-ADD     150           LENGH            15 5
010900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011000000724     C                   PARM                    depcmd
011100000724     C                   PARM                    LENGH
011200000724     C*
011300000724     C                   endif
011400000616     C*
011500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011600000616     C                   movel(p)  CMD(2)        depcmd
011700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011800000616     C*
011900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012000011123     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    depcmd
012300000616     C                   PARM                    LENGH
012400990914     C*
012500040325     C                   if        not %open(tis7tetps)
012600040325     C                   open      tis7tetps
012700030407     C   50              except    testdett
012800000613     C                   endif
012900010531     C*
013000010531     C                   endif
013100010531     C*
013200010531     C                   ENDSR
013300010531     C***
013400010531
013500010531
013600010531
013700010531
013800010531
013900010531
014000010531
014100010531     C*--------------------------------------------------------
014200010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014300010531     C*--------------------------------------------------------
014400010531     C     ENDELA        BEGSR
014500010531     C*
014600010531     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014700000613     C*
014800040325     C                   if        %open(tis7tetps)
014900030407     C   50              except    findett
015000040325     C                   close     tis7tetps
015100000613     C                   endif
015200000616     C*
015300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015400011123     C                   Z-ADD     150           LENGH            15 5
015500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015600000616     C                   PARM                    CMD(3)
015700000616     C                   PARM                    LENGH
015800000616     C*
015900010531     C                   ENDSR
016000010531     C***
016100010531
016200010531
016300000613
016400000613
016500000613
016600000613     C*--------------------------------------------------------
016700000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016800000613     C*--------------------------------------------------------
016900000621     C     STPR          BEGSR
017000000613     C*
017100040325     C                   if        not %open(tis7tetp)
017200040325     C                   open      tis7tetp
017300990915     C                   endif
017400990915     C*
017500030407     C   50              except    riepilogo
017600990915     C*
017700040325     C                   if        %open(tis7tetp)
017800040325     C                   close     tis7tetp
017900990914     C                   endif
018000000613     C*
018100000613     C                   ENDSR
018200000613     C***
018300990914
018400010531
018500010531
018600010531
018700010531
018800990914     C*--------------------------------------------------------
018900040325     C* RWVAT  LEGGE TIVIN00R E SCRIVE EDIVAT0F              *
019000990914     C*--------------------------------------------------------
019100040325     C     RWVAT         BEGSR
019200030710     C*
019300030710     C                   clear                   §CTDOK
019400030710     C                   clear                   §CTDNO
019500990914     C*
019600030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
019700030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
019800030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
019900030710     C                   if        *in60 = *on
020000990914     C                   if        not %open(tivin00r)
020100990914     C                   open      tivin00r
020200990914     C                   endif
020300040325     C                   if        not %open(edivatwr)
020400040325     C                   open      edivatwr
020500990914     C                   endif
020600990914     C*
020700990914     C                   DO        *HIVAL
020800010618     C*
020900990914     C                   READ      tivin00r                               70
021000010618     C*
021100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021200010618     C                   if        vinflg = '1'
021300010618     C                   eval      flgOk = '1'
021400010618     C                   endif
021500010618     C*
021600000905     C                   if        vindta > *blanks
021700000613     C                   add       1             rrnum
021800990914     C*
021900010601     C                   if        *in70 = *off and
022000990914     C                             (vinflg = *blanks
022100010711     C                              or vinflg = '0'
022200010711     C                              or vinflg = '2')
022300990914     C*
022400040325     C                   clear                   EDIVAT00
022500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022600010711     C                   if        vinflg = *blanks or vinflg = '0'
022700010711     C                   clear                   vinmsg
022800010711     C                   endif
022900010711     C*
023000040325     C                   eval      fnvataar = vindta
023100040325     C*
023200040325     C                   testb     x'3F'         a_vatatb                 02
023300040325     C                   if        not *in02
023400991022     C*
023500040325     C                   exsr      impvat
023600010531     C*
023700010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023800040325     C                   eval      depfil = VATLNP
023900010531     C                   exsr      repfil
024000010601     C                   if        depfil = invfil
024100021014     C                   if        vlrpoi = 999
024200040325     C                   MOVE(p)   invfil        VATFGS
024300021014     C                   else
024400040325     C                   MOVE(p)   vlrpoi        VATFGS
024500021014     C                   endif
024600010531     C*
024700010531     C                   exsr      PREELA
024800010531     C*
024900030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
025000040325     C                   EVAL      VATCMR = wCMR
025100040325     C                   EVAL      VATCNT = 1
025200081202
025300081202     C*
025400081202     C* Forzatura x NSP = RMA
025500081202     C                   EVAL      PiStr=a_vatrma
025600081202     C                   EXSR      CHKNUM
025700081202     C                   IF        PiInt=*on
025800081202     C                   Z-ADD     PiVal         VATNSP
025900081202     C                   ELSE
026000081202     C                   SETON                                        31
026100081202     C                   EVAL      vinmsg = %trimr(vinmsg)
026200081202     C                             + ' ' + 'VATNSP/RMA'
026300081202     C                   ENDIF
026400081202
026500010601     C*
026600010604     C  N31              ADD       1             §CTDOK            7 0
026700010601     C   32
026800010604     COR 31              ADD       1             §CTDNO            7 0
026900040325     C  N31              WRITE     EDIVAT00
027000010601     C*
027100010601     C                   if        *in31 = *off and
027200010601     C                             *in32 = *off
027300990914     C                   eval      vinflg = '1'
027400990914     C                   else
027500000621     C                   eval      recko = vindta
027600040325     C                   if        %open(tis7tetps)
027700030407     C   50              except    rigadett
027800010612     C                   endif
027900990914     C                   eval      vinflg = '2'
028000990914     C                   endif
028100000905     C                   endif
028200000905     C                   endif
028300010601     C*
028400010601     C                   endif
028500000905     C*
028600000905     C                   else
028700000905     C                   eval      vinflg = '1'
028800000905     C                   endif
028900000905     C*
029000000905     C  N70              update    tivin000
029100991022     C*
029200991022     C  N70              ENDdo
029300010531     C
029400010531     C*
029500010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029600010531     C                   if        cntNonEl = *zeros or
029700010531     C                             flgMulti = '0'
029800990914     C* Se non ci sono record con errori ...
029900010531     C                   if        §ctdno = 0 and
030000010531     C                             flgStato <> '2'
030100990914     C* ... restituisco esito OK.
030200990921     C                   eval      wrkesito = '0'
030300990914     C                   else
030400000710     C                   if        §ctdok > 0
030500990921     C                   eval      wrkesito = '1'
030600000710     C                   else
030700010615     C                   if        flgOk = '0'
030800010615     C                   eval      wrkesito = '2'
030900010615     C                   else
031000010615     C                   eval      wrkesito = '6'
031100010615     C                   endif
031200990914     C                   endif
031300000710     C                   endif
031400010531     C                   else
031500010531     C                   eval      wrkesito = '9'
031600010531     C                   endif
031700030710     C*
031800030710 X  1C                   else
031900030710     C                   eval      wrkesito = '8'
032000030710     C                   eval      flgGiro = '1'
032100030710 E  1C                   endif
032200990914     C*
032300990914     C                   if        %open(tivin00r)
032400990914     C                   close     tivin00r
032500990914     C                   endif
032600040325     C                   if        %open(edivatwr)
032700040325     C                   close     edivatwr
032800990914     C                   endif
032900010531     C*
033000010531     C                   if        vlrpoi <> 999
033100010531     C                   eval      invfil = vlrpoi
033200010531     C                   endif
033300990914     C*
033400990921     C                   if        §ctdok > 0
033500010531     C                             and invfil > *zeros
033600990921     C                   exsr      invio
033700990921     C                   endif
033800010612     C*
033900010612     C                   if        flgGiro = '1'
034000010612     C                   exsr      endela
034100010612     C                   endif
034200990921     C*
034300990914     C                   ENDSR
034400000613     C***
034500040325
034600040325
034700040325
034800040325
034900040325     C*----------------------------------------------------*
035000040325     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
035100040325     C*----------------------------------------------------*
035200040325     C     IMPVAT        BEGSR
035300040325     C*
035400040325     C                   SETOFF                                       3132
035500040325     C*
035600040325     C                   SETOFF                                       303132
035700040325     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
035800040325     C                   TESTN                   a_vatccm             30
035900040325     C  N30              TESTN                   a_vatccm               30
036000040325     C  N30              TESTN                   a_vatccm                 30
036100040325     C                   MOVE      a_vatccm      W001A             1
036200040325     C     *IN30         IFEQ      *OFF
036300040325     C     W001A         ORLT      '0'
036400040325     C                   SETON                                        31
036500040325     C                   Z-ADD     *zeros        VATCCM
036600040325     C                   EVAL      vinmsg = 'VATCCM'
036700040325     C                   ELSE
036800040325     C                   MOVE      a_vatccm      VATCCM
036900040325     C     VATCCM        IFLE      *ZEROS
037000040325     C                   SETON                                        31
037100040325     C                   Z-ADD     *zeros        VATCCM
037200040325     C                   EVAL      vinmsg = 'VATCCM'
037300040325     C                   ENDIF
037400040325     C                   ENDIF
037500040325     C*
037600040325     C                   TESTN                   a_vatlnp             30
037700040325     C  N30              TESTN                   a_vatlnp               30
037800040325     C  N30              TESTN                   a_vatlnp                 30
037900040325     C                   MOVE      a_vatlnp      W001A             1
038000040325     C     *IN30         IFEQ      *OFF
038100040325     C     W001A         ORLT      '0'
038200040325     C                   SETON                                        31
038300040325     C                   Z-ADD     *zeros        VATLNP
038400040325     C                   EVAL      vinmsg = %trimr(vinmsg)
038500040325     C                             + ' ' + 'VATLNP'
038600040325     C                   ELSE
038700040325     C                   MOVE      a_vatlnp      VATLNP
038800040325     C     VATLNP        IFLE      *ZEROS
038900040325     C                   SETON                                        31
039000040325     C                   Z-ADD     *zeros        VATLNP
039100040325     C                   EVAL      vinmsg = %trimr(vinmsg)
039200040325     C                             + ' ' + 'VATLNP'
039300040325     C                   ENDIF
039400040325     C                   ENDIF
039500040325     C*
039600040325     C                   TESTN                   a_vataas             30
039700040325     C  N30              TESTN                   a_vataas               30
039800040325     C  N30              TESTN                   a_vataas                 30
039900040325     C                   MOVE      a_vataas      W001A             1
040000040325     C     *IN30         IFEQ      *OFF
040100040325     C     W001A         ORLT      '0'
040200040325     C                   SETON                                        31
040300040325     C                   MOVEL(P)  datcor        VATAAS
040400040325     C                   EVAL      vinmsg = %trimr(vinmsg)
040500040325     C                             + ' ' + 'VATAAS'
040600040325     C                   ELSE
040700040325     C                   MOVE      a_vataas      VATAAS
040800040325     C                   ENDIF
040900040325     C*
041000040325     C                   TESTN                   a_vatnrs             30
041100040325     C  N30              TESTN                   a_vatnrs               30
041200040325     C  N30              TESTN                   a_vatnrs                 30
041300040325     C                   MOVE      a_vatnrs      W001A             1
041400040325     C     *IN30         IFEQ      *OFF
041500040325     C     W001A         ORLT      '0'
041600040325     C     W001A         ANDNE     *BLANK
041700040325     C                   SETON                                        31
041800040325     C                   Z-ADD     *zeros        VATNRS
041900040325     C                   EVAL      vinmsg = %trimr(vinmsg)
042000040325     C                             + ' ' + 'VATNRS'
042100040325     C                   ELSE
042200040325     C                   MOVE      a_vatnrs      VATNRS
042300040325     C                   ENDIF
042400040325     C*
042500040325     C                   TESTN                   a_vatnsp             30
042600040325     C  N30              TESTN                   a_vatnsp               30
042700040325     C  N30              TESTN                   a_vatnsp                 30
042800040325     C                   MOVE      a_vatnsp      W001A             1
042900040325     C     *IN30         IFEQ      *OFF
043000040325     C     W001A         ORLT      '0'
043100040325     C                   SETON                                        31
043200040325     C                   Z-ADD     *zeros        VATNSP
043300040325     C                   EVAL      vinmsg = %trimr(vinmsg)
043400040325     C                             + ' ' + 'VATNSP'
043500040325     C                   ELSE
043600040325     C                   MOVE      a_vatnsp      VATNSP
043700040325     C                   ENDIF
043800040325     C*
043900040325     C*
044000040325     C* ALFABETICI
044100040325     C                   MOVE      a_vatatb      VATATB
044200040325     C                   MOVE      a_vattrc      VATTRC
044300040325     C                   EVAL      VATNOT = %trim(a_vatnot)
044400040325     C*
044500040325     C                   ENDSR
044600040325     C*----------------------------------------------------*
044700990921
044800010531
044900010531
045000010531
045100010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045200010531     C     repfil        BEGSR
045300010531     C*
045400010531     C                   if        invfil = *zeros and
045500010531     C                             depfil > *zeros and
045600010613     C                             (vinflg = *blanks or
045700010613     C                              vinflg = *zeros)
045800010531     C
045900010531     C                   eval      invfil = depfil
046000010531     C                   endif
046100010531     C*
046200010531     C                   if        depfil <> invfil and
046300010531     C                             invfil > *zeros
046400010531     C                   eval      flgMulti = '1'
046500010531     C                   if        vinflg = *blanks
046600010531     C                   add       1             cntNonEl
046700010531     C                   endif
046800010531     C                   endif
046900010531     C*
047000010531     C                   if        vinflg = '2'
047100010531     C                   eval      flgStato = '2'
047200010531     C                   endif
047300010531     C*
047400010531     C                   ENDSR
047500010531     C***
047600010531
047700010531
047800010531
047900010531
048000010531
048100990921      /TITLE Invio dei dati al punto operativo.
048200000613     C     invio         BEGSR
048300990921     C*
048400990921     C                   reset                   dscmz
048500030618     C                   move      invfil        cmzdst
048600030618     C***                move      '789'         cmzdst
048700990921     C                   eval      cmzfld = vlrfou
048800990921     C                   eval      cmzmbd = vlrhdl
048900990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
049000000710     C                   if        prmfir = *blanks
049100040325     C                   eval      cmzfla = 'EDIVAT0F'
049200040325     C                   eval      cmzmba = 'EDIVAT0F'
049300000710     C                   else
049400000710     C                   eval      cmzfla = prmfir
049500000710     C                   eval      cmzmba = prmfir
049600000710     C                   endif
049700990921     C                   eval      cmznrr = *zeros
049800990921     C                   move      §ctdok        cmznrr
049900021003     C                   eval      cmzlba = vlrfl1
050000990921     C                   call(e)   'TIS711C'
050100990921     C                   parm                    dscmz
050200990921     C                   parm      *blanks       esito
050300990921     C                   if        %error
050400990921     C                             or cmzerr = '1'
050500990921     C                             or esito  = '1'
050600000710     C                   eval      wrkesito = '3'
050700990921     C                   endif
050800990921     C*
050900000613     C                   ENDSR
051000000613     C***
051100990914
051200010531
051300010531
051400010531
051500010531      /TITLE Invio dei dati al punto operativo.
051600010531     C     opeini        BEGSR
051700010531     C*
051800010531     C* Inizializzo flag e contatori operativi
051900010531     C                   movel     '0'           flgGiro           1
052000010531     C                   movel     '0'           flgMulti          1
052100010531     C                   movel     '1'           flgStato          1
052200010615     C                   movel     '0'           flgOk             1
052300010531     C                   z-add     *zeros        cntNonEl         10 0
052400010531     C                   z-add     *zeros        depfil            3 0
052500010531     C                   z-add     *zeros        invfil            3 0
052600030407     C*
052700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
052800030407     C                   if        %subst(vlrppt:1:1) = 'S'
052900030407     C                   seton                                        50
053000030407     C                   else
053100030407     C                   setoff                                       50
053200030407     C                   endif
053300010531     C*
053400010531     C                   ENDSR
053500010531     C***
053600010531
053700081202
053800081202
053900081202
054000081202     C*----------------------------------------------------*
054100081202     C*  CONTROLLO NUMERICITA' CAMPI
054200081202     C*----------------------------------------------------*
054300081202     C     CHKNUM        BEGSR
054400081202     C*
054500081202     C                   IF        PiDecChr = *blanks
054600081202     C                   EVAL      PiDecChr = '.'
054700081202     C                   ENDIF
054800081202     C*
054900081202     C                   callp(e)  UBISNUM_Check(PiStr
055000081202     C                                          :PiDecChr
055100081202     C                                          :PiVal
055200081202     C                                          :PiNum
055300081202     C                                          :PiInt)
055400081202     C*
055500081202     C                   IF        %error
055600081202     C                   EVAL      PiInt=*off
055700081202     C                   ENDIF
055800081202     C*
055900081202     C                   ENDSR
056000081202     C***
056100081202
056200010531
056300010531
056400010531
056500000613     C     *inzsr        BEGSR
056600990914     C*
056700990914     C     *entry        plist
056800990921     C                   parm                    tivlrds
056900990921     C                   parm      wrkesito      esito
057000000724     C                   parm                    prmlit
057100000710     C                   parm                    prmfir
057200030710     C*
057300030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
057400030710     C                   move(p)   vlrMSG        wNomeFile
057500030710     C*
057600030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
057700030710     C* del codice CMR dal nome del file importato.
057800030710     C                   if        wNomeFile <> *blanks AND
057900030710     C                             vlrppt<>*blanks
058000030710     C                   if        %subst(vlrppt:1:1) = 'S'
058100030710     C                   seton                                        50
058200030710     C                   else
058300030710     C                   setoff                                       50
058400030710     C                   endif
058500081202     C                   if        %subst(vlrppt:2:4) > *zeros
058600030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
058700030710     C                   move(p)   wPosDaA       wPosDa
058800030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
058900030710     C                   move(p)   wLungA        wLung
059000030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
059100081202     C                   else
059200081202     C                   eval      wCMR = 'CEVA - TIM'
059300081202     C                   endif
059400030710     C                   seton                                        60
059500030710     C                   else
059600030710     C                   setoff                                       60
059700030710     C                   endif
059800010606     C*
059900010606     C* CALCOLA LA DATA CORRENTE
060000010606     C                   time                    wn14             14 0
060100010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060200010606     C                   z-add     wn8           g08dat
060300010606     C                   z-add     *zeros        g08inv
060400010606     C                   movel     '0'           g08err
060500010606     C                   call      'XSRDA8'
060600010606     C                   parm                    wlbda8
060700010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060800000613     C*
060900000613     C                   ENDSR
061000000613     C***
061100990914
061200040325     Otis7tetp  E            riepilogo         2
061300990915     O                                              'Upload via Internet'
061400990915     O                                           +1 'Traduzione TIVIN00R -'
061500040325     O                                           +1 'EDIVATWR'
061600990915     O                                           +1 'Dettaglio colli'
061700990915     O          E            riepilogo   2
061800990915     O                       wrkdata
061900990915     O                       wrkora              +1
062000990915     O                       procname            +1
062100990915     O          E            riepilogo   2
062200990915     O                                              'Cliente..................:'
062300040325     O                       VATCCM        z     +1
062400030618     O          E            riepilogo   2
062500030618     O                                              'Numero CMR...............:'
062600040325     O                       VATCMR              +1
062700990915     O          E            riepilogo   2
062800990921     O                                              'Riferimento strategi.....:'
062900990921     O                       vlrhdl              +1
063000990915     O          E            riepilogo   2
063100990915     O                                              'Giusti...................:'
063200990915     O                       §CTDOK        2   +  1
063300990915     O          E            riepilogo   2
063400010601     O                                              'Sbagliati ...............:'
063500990915     O                       §CTDNO        2   +  1
063600000613
063700040325     Otis7tetps E            testdett          2
063800000613     O                                              'Upload via Internet'
063900000613     O                                           +1 'Traduzione TIVIN00R -'
064000040325     O                                           +1 'EDIVATWR'
064100000613     O                                           +1 'Dettaglio colli'
064200000616     O          E            testdett    3
064300000613     O                                           +2 'N° rec'
064400000613     O                                           +3 'Anteprima contenuto'
064500000616     O          E            rigadett    2
064600000613     O                       rrnum               +2
064700000621     O                       recko               +3
064800000616     O          E            findett     2
064900000613     O                       wrkdata
065000000613     O                       wrkora              +1
065100000613     O                       procname            +1
065200000616     O          E            findett     2
065300000613     O                                              'Cliente..................:'
065400040325     O                       VATCCM        z     +1
065500030618     O          E            findett     2
065600030618     O                                              'Numero CMR...............:'
065700040325     O                       VATCMR              +1
065800000616     O          E            findett     2
065900000613     O                                              'Riferimento strategi.....:'
066000000613     O                       vlrhdl              +1
066100000616     O          E            findett     2
066200000613     O                                              'Giusti...................:'
066300000613     O                       §CTDOK        2   +  1
066400000616     O          E            findett     2
066500010601     O                                              'Sbagliati ...............:'
066600000613     O                       §CTDNO        2   +  1
066700000613** CMD - COMANDI CL
066800040325OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TETP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
066900040325OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TETPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
067000040325DLTOVR FILE(TIS7TETP TIS7TETPS) LVL(*)
067100000613
067200000613
