000100040325      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200990914     H dftactgrp(*yes)
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
005400010606
005500010606
005600010606
005700990914
005800990915     C                   time                    wrkdata
005900990915     C                   time                    wrkora
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300010531     C*
006400010531     C                   exsr      opeini
006500040325     C                   exsr      rwvat
006600010531     C*
006700010531     C                   seton                                        lr
006800010531
006900010531
007000010531
007100010531
007200010531     C*--------------------------------------------------------
007300010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007400010531     C*--------------------------------------------------------
007500010531     C     PREELA        BEGSR
007600000724     C*
007700000724     C* SE OCCORRE SPEDIRE IN FILIALE
007800010531     C                   if        invfil <> *zeros and
007900010531     C                             flgGiro = '0'
008000010531     C*
008100010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008200010531     C                   eval      flgGiro = '1'
008300000724     C*
008400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600040325     C     VATFGS        chain     azorg01l
008700000724     C                   if        %found
008800000724     C                   movel(p)  CMD(1)        depcmd
008900020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009000000724     C*
009100000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009200011123     C                   Z-ADD     150           LENGH            15 5
009300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000724     C                   PARM                    depcmd
009500000724     C                   PARM                    LENGH
009600000724     C*
009700000724     C                   endif
009800000616     C*
009900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010000000616     C                   movel(p)  CMD(2)        depcmd
010100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010200000616     C*
010300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010400011123     C                   Z-ADD     150           LENGH            15 5
010500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010600000616     C                   PARM                    depcmd
010700000616     C                   PARM                    LENGH
010800990914     C*
010900040325     C                   if        not %open(tis7tetps)
011000040325     C                   open      tis7tetps
011100030407     C   50              except    testdett
011200000613     C                   endif
011300010531     C*
011400010531     C                   endif
011500010531     C*
011600010531     C                   ENDSR
011700010531     C***
011800010531
011900010531
012000010531
012100010531
012200010531
012300010531
012400010531
012500010531     C*--------------------------------------------------------
012600010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012700010531     C*--------------------------------------------------------
012800010531     C     ENDELA        BEGSR
012900010531     C*
013000010531     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013100000613     C*
013200040325     C                   if        %open(tis7tetps)
013300030407     C   50              except    findett
013400040325     C                   close     tis7tetps
013500000613     C                   endif
013600000616     C*
013700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013800011123     C                   Z-ADD     150           LENGH            15 5
013900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014000000616     C                   PARM                    CMD(3)
014100000616     C                   PARM                    LENGH
014200000616     C*
014300010531     C                   ENDSR
014400010531     C***
014500010531
014600010531
014700000613
014800000613
014900000613
015000000613     C*--------------------------------------------------------
015100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015200000613     C*--------------------------------------------------------
015300000621     C     STPR          BEGSR
015400000613     C*
015500040325     C                   if        not %open(tis7tetp)
015600040325     C                   open      tis7tetp
015700990915     C                   endif
015800990915     C*
015900030407     C   50              except    riepilogo
016000990915     C*
016100040325     C                   if        %open(tis7tetp)
016200040325     C                   close     tis7tetp
016300990914     C                   endif
016400000613     C*
016500000613     C                   ENDSR
016600000613     C***
016700990914
016800010531
016900010531
017000010531
017100010531
017200990914     C*--------------------------------------------------------
017300040325     C* RWVAT  LEGGE TIVIN00R E SCRIVE EDIVAT0F              *
017400990914     C*--------------------------------------------------------
017500040325     C     RWVAT         BEGSR
017600030710     C*
017700030710     C                   clear                   §CTDOK
017800030710     C                   clear                   §CTDNO
017900990914     C*
018000030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
018100030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
018200030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
018300030710     C                   if        *in60 = *on
018400990914     C                   if        not %open(tivin00r)
018500990914     C                   open      tivin00r
018600990914     C                   endif
018700040325     C                   if        not %open(edivatwr)
018800040325     C                   open      edivatwr
018900990914     C                   endif
019000990914     C*
019100990914     C                   DO        *HIVAL
019200010618     C*
019300990914     C                   READ      tivin00r                               70
019400010618     C*
019500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019600010618     C                   if        vinflg = '1'
019700010618     C                   eval      flgOk = '1'
019800010618     C                   endif
019900010618     C*
020000000905     C                   if        vindta > *blanks
020100000613     C                   add       1             rrnum
020200990914     C*
020300010601     C                   if        *in70 = *off and
020400990914     C                             (vinflg = *blanks
020500010711     C                              or vinflg = '0'
020600010711     C                              or vinflg = '2')
020700990914     C*
020800040325     C                   clear                   EDIVAT00
020900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021000010711     C                   if        vinflg = *blanks or vinflg = '0'
021100010711     C                   clear                   vinmsg
021200010711     C                   endif
021300010711     C*
021400040325     C                   eval      fnvataar = vindta
021500040325     C*
021600040325     C                   testb     x'3F'         a_vatatb                 02
021700040325     C                   if        not *in02
021800991022     C*
021900040325     C                   exsr      impvat
022000010531     C*
022100010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022200040325     C                   eval      depfil = VATLNP
022300010531     C                   exsr      repfil
022400010601     C                   if        depfil = invfil
022500021014     C                   if        vlrpoi = 999
022600040325     C                   MOVE(p)   invfil        VATFGS
022700021014     C                   else
022800040325     C                   MOVE(p)   vlrpoi        VATFGS
022900021014     C                   endif
023000010531     C*
023100010531     C                   exsr      PREELA
023200010531     C*
023300030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
023400100301     C                   EVAL      VATCMR = 'MONDADORI'                         * forzatura x CMR
023500040325     C                   EVAL      VATCNT = 1
023600010601     C*
023700010604     C  N31              ADD       1             §CTDOK            7 0
023800010601     C   32
023900010604     COR 31              ADD       1             §CTDNO            7 0
024000040325     C  N31              WRITE     EDIVAT00
024100010601     C*
024200010601     C                   if        *in31 = *off and
024300010601     C                             *in32 = *off
024400990914     C                   eval      vinflg = '1'
024500990914     C                   else
024600000621     C                   eval      recko = vindta
024700040325     C                   if        %open(tis7tetps)
024800030407     C   50              except    rigadett
024900010612     C                   endif
025000990914     C                   eval      vinflg = '2'
025100990914     C                   endif
025200000905     C                   endif
025300000905     C                   endif
025400010601     C*
025500010601     C                   endif
025600000905     C*
025700000905     C                   else
025800000905     C                   eval      vinflg = '1'
025900000905     C                   endif
026000000905     C*
026100000905     C  N70              update    tivin000
026200991022     C*
026300991022     C  N70              ENDdo
026400010531     C
026500010531     C*
026600010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026700010531     C                   if        cntNonEl = *zeros or
026800010531     C                             flgMulti = '0'
026900990914     C* Se non ci sono record con errori ...
027000010531     C                   if        §ctdno = 0 and
027100010531     C                             flgStato <> '2'
027200990914     C* ... restituisco esito OK.
027300990921     C                   eval      wrkesito = '0'
027400990914     C                   else
027500000710     C                   if        §ctdok > 0
027600990921     C                   eval      wrkesito = '1'
027700000710     C                   else
027800010615     C                   if        flgOk = '0'
027900010615     C                   eval      wrkesito = '2'
028000010615     C                   else
028100010615     C                   eval      wrkesito = '6'
028200010615     C                   endif
028300990914     C                   endif
028400000710     C                   endif
028500010531     C                   else
028600010531     C                   eval      wrkesito = '9'
028700010531     C                   endif
028800030710     C*
028900030710 X  1C                   else
029000030710     C                   eval      wrkesito = '8'
029100030710     C                   eval      flgGiro = '1'
029200030710 E  1C                   endif
029300990914     C*
029400990914     C                   if        %open(tivin00r)
029500990914     C                   close     tivin00r
029600990914     C                   endif
029700040325     C                   if        %open(edivatwr)
029800040325     C                   close     edivatwr
029900990914     C                   endif
030000010531     C*
030100010531     C                   if        vlrpoi <> 999
030200010531     C                   eval      invfil = vlrpoi
030300010531     C                   endif
030400990914     C*
030500990921     C                   if        §ctdok > 0
030600010531     C                             and invfil > *zeros
030700990921     C                   exsr      invio
030800990921     C                   endif
030900010612     C*
031000010612     C                   if        flgGiro = '1'
031100010612     C                   exsr      endela
031200010612     C                   endif
031300990921     C*
031400990914     C                   ENDSR
031500000613     C***
031600040325
031700040325
031800040325
031900040325
032000040325     C*----------------------------------------------------*
032100040325     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
032200040325     C*----------------------------------------------------*
032300040325     C     IMPVAT        BEGSR
032400040325     C*
032500040325     C                   SETOFF                                       3132
032600040325     C*
032700040325     C                   SETOFF                                       303132
032800040325     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
032900040325     C                   TESTN                   a_vatccm             30
033000040325     C  N30              TESTN                   a_vatccm               30
033100040325     C  N30              TESTN                   a_vatccm                 30
033200040325     C                   MOVE      a_vatccm      W001A             1
033300040325     C     *IN30         IFEQ      *OFF
033400040325     C     W001A         ORLT      '0'
033500040325     C                   SETON                                        31
033600040325     C                   Z-ADD     *zeros        VATCCM
033700040325     C                   EVAL      vinmsg = 'VATCCM'
033800040325     C                   ELSE
033900040325     C                   MOVE      a_vatccm      VATCCM
034000040325     C     VATCCM        IFLE      *ZEROS
034100040325     C                   SETON                                        31
034200040325     C                   Z-ADD     *zeros        VATCCM
034300040325     C                   EVAL      vinmsg = 'VATCCM'
034400040325     C                   ENDIF
034500040325     C                   ENDIF
034600040325     C*
034700040325     C                   TESTN                   a_vatlnp             30
034800040325     C  N30              TESTN                   a_vatlnp               30
034900040325     C  N30              TESTN                   a_vatlnp                 30
035000040325     C                   MOVE      a_vatlnp      W001A             1
035100040325     C     *IN30         IFEQ      *OFF
035200040325     C     W001A         ORLT      '0'
035300040325     C                   SETON                                        31
035400040325     C                   Z-ADD     *zeros        VATLNP
035500040325     C                   EVAL      vinmsg = %trimr(vinmsg)
035600040325     C                             + ' ' + 'VATLNP'
035700040325     C                   ELSE
035800040325     C                   MOVE      a_vatlnp      VATLNP
035900040325     C     VATLNP        IFLE      *ZEROS
036000040325     C                   SETON                                        31
036100040325     C                   Z-ADD     *zeros        VATLNP
036200040325     C                   EVAL      vinmsg = %trimr(vinmsg)
036300040325     C                             + ' ' + 'VATLNP'
036400040325     C                   ENDIF
036500040325     C                   ENDIF
036600040325     C*
036700040325     C                   TESTN                   a_vataas             30
036800040325     C  N30              TESTN                   a_vataas               30
036900040325     C  N30              TESTN                   a_vataas                 30
037000040325     C                   MOVE      a_vataas      W001A             1
037100040325     C     *IN30         IFEQ      *OFF
037200040325     C     W001A         ORLT      '0'
037300040325     C                   SETON                                        31
037400040325     C                   MOVEL(P)  datcor        VATAAS
037500040325     C                   EVAL      vinmsg = %trimr(vinmsg)
037600040325     C                             + ' ' + 'VATAAS'
037700040325     C                   ELSE
037800040325     C                   MOVE      a_vataas      VATAAS
037900040325     C                   ENDIF
038000040325     C*
038100040325     C                   TESTN                   a_vatnrs             30
038200040325     C  N30              TESTN                   a_vatnrs               30
038300040325     C  N30              TESTN                   a_vatnrs                 30
038400040325     C                   MOVE      a_vatnrs      W001A             1
038500040325     C     *IN30         IFEQ      *OFF
038600040325     C     W001A         ORLT      '0'
038700040325     C     W001A         ANDNE     *BLANK
038800040325     C                   SETON                                        31
038900040325     C                   Z-ADD     *zeros        VATNRS
039000040325     C                   EVAL      vinmsg = %trimr(vinmsg)
039100040325     C                             + ' ' + 'VATNRS'
039200040325     C                   ELSE
039300040325     C                   MOVE      a_vatnrs      VATNRS
039400040325     C                   ENDIF
039500040325     C*
039600040325     C                   TESTN                   a_vatnsp             30
039700040325     C  N30              TESTN                   a_vatnsp               30
039800040325     C  N30              TESTN                   a_vatnsp                 30
039900040325     C                   MOVE      a_vatnsp      W001A             1
040000040325     C     *IN30         IFEQ      *OFF
040100040325     C     W001A         ORLT      '0'
040200040325     C                   SETON                                        31
040300040325     C                   Z-ADD     *zeros        VATNSP
040400040325     C                   EVAL      vinmsg = %trimr(vinmsg)
040500040325     C                             + ' ' + 'VATNSP'
040600040325     C                   ELSE
040700040325     C                   MOVE      a_vatnsp      VATNSP
040800040325     C                   ENDIF
040900040325     C*
041000040325     C*
041100040325     C* ALFABETICI
041200040325     C                   MOVE      a_vatatb      VATATB
041300040325     C                   MOVE      a_vattrc      VATTRC
041400040325     C                   EVAL      VATNOT = %trim(a_vatnot)
041500040325     C*
041600040325     C                   ENDSR
041700040325     C*----------------------------------------------------*
041800990921
041900010531
042000010531
042100010531
042200010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
042300010531     C     repfil        BEGSR
042400010531     C*
042500010531     C                   if        invfil = *zeros and
042600010531     C                             depfil > *zeros and
042700010613     C                             (vinflg = *blanks or
042800010613     C                              vinflg = *zeros)
042900010531     C
043000010531     C                   eval      invfil = depfil
043100010531     C                   endif
043200010531     C*
043300010531     C                   if        depfil <> invfil and
043400010531     C                             invfil > *zeros
043500010531     C                   eval      flgMulti = '1'
043600010531     C                   if        vinflg = *blanks
043700010531     C                   add       1             cntNonEl
043800010531     C                   endif
043900010531     C                   endif
044000010531     C*
044100010531     C                   if        vinflg = '2'
044200010531     C                   eval      flgStato = '2'
044300010531     C                   endif
044400010531     C*
044500010531     C                   ENDSR
044600010531     C***
044700010531
044800010531
044900010531
045000010531
045100010531
045200990921      /TITLE Invio dei dati al punto operativo.
045300000613     C     invio         BEGSR
045400990921     C*
045500990921     C                   reset                   dscmz
045600030618     C                   move      invfil        cmzdst
045700030618     C***                move      '789'         cmzdst
045800990921     C                   eval      cmzfld = vlrfou
045900990921     C                   eval      cmzmbd = vlrhdl
046000990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
046100000710     C                   if        prmfir = *blanks
046200040325     C                   eval      cmzfla = 'EDIVAT0F'
046300040325     C                   eval      cmzmba = 'EDIVAT0F'
046400000710     C                   else
046500000710     C                   eval      cmzfla = prmfir
046600000710     C                   eval      cmzmba = prmfir
046700000710     C                   endif
046800990921     C                   eval      cmznrr = *zeros
046900990921     C                   move      §ctdok        cmznrr
047000021003     C                   eval      cmzlba = vlrfl1
047100990921     C                   call(e)   'TIS711C'
047200990921     C                   parm                    dscmz
047300990921     C                   parm      *blanks       esito
047400990921     C                   if        %error
047500990921     C                             or cmzerr = '1'
047600990921     C                             or esito  = '1'
047700000710     C                   eval      wrkesito = '3'
047800990921     C                   endif
047900990921     C*
048000000613     C                   ENDSR
048100000613     C***
048200990914
048300010531
048400010531
048500010531
048600010531      /TITLE Invio dei dati al punto operativo.
048700010531     C     opeini        BEGSR
048800010531     C*
048900010531     C* Inizializzo flag e contatori operativi
049000010531     C                   movel     '0'           flgGiro           1
049100010531     C                   movel     '0'           flgMulti          1
049200010531     C                   movel     '1'           flgStato          1
049300010615     C                   movel     '0'           flgOk             1
049400010531     C                   z-add     *zeros        cntNonEl         10 0
049500010531     C                   z-add     *zeros        depfil            3 0
049600010531     C                   z-add     *zeros        invfil            3 0
049700030407     C*
049800030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
049900030407     C                   if        %subst(vlrppt:1:1) = 'S'
050000030407     C                   seton                                        50
050100030407     C                   else
050200030407     C                   setoff                                       50
050300030407     C                   endif
050400010531     C*
050500010531     C                   ENDSR
050600010531     C***
050700010531
050800010531
050900010531
051000010531
051100000613     C     *inzsr        BEGSR
051200990914     C*
051300990914     C     *entry        plist
051400990921     C                   parm                    tivlrds
051500990921     C                   parm      wrkesito      esito
051600000724     C                   parm                    prmlit
051700000710     C                   parm                    prmfir
051800030710     C*
051900030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
052000030710     C                   move(p)   vlrMSG        wNomeFile
052100030710     C*
052200030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
052300030710     C* del codice CMR dal nome del file importato.
052400030710     C                   if        wNomeFile <> *blanks AND
052500030710     C                             vlrppt<>*blanks
052600030710     C                   if        %subst(vlrppt:1:1) = 'S'
052700030710     C                   seton                                        50
052800030710     C                   else
052900030710     C                   setoff                                       50
053000030710     C                   endif
053100030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
053200030710     C                   move(p)   wPosDaA       wPosDa
053300030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
053400030710     C                   move(p)   wLungA        wLung
053500030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
053600030710     C                   seton                                        60
053700030710     C                   else
053800030710     C                   setoff                                       60
053900030710     C                   endif
054000010606     C*
054100010606     C* CALCOLA LA DATA CORRENTE
054200010606     C                   time                    wn14             14 0
054300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054400010606     C                   z-add     wn8           g08dat
054500010606     C                   z-add     *zeros        g08inv
054600010606     C                   movel     '0'           g08err
054700010606     C                   call      'XSRDA8'
054800010606     C                   parm                    wlbda8
054900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055000000613     C*
055100000613     C                   ENDSR
055200000613     C***
055300990914
055400040325     Otis7tetp  E            riepilogo         2
055500990915     O                                              'Upload via Internet'
055600990915     O                                           +1 'Traduzione TIVIN00R -'
055700040325     O                                           +1 'EDIVATWR'
055800990915     O                                           +1 'Dettaglio colli'
055900990915     O          E            riepilogo   2
056000990915     O                       wrkdata
056100990915     O                       wrkora              +1
056200990915     O                       procname            +1
056300990915     O          E            riepilogo   2
056400990915     O                                              'Cliente..................:'
056500040325     O                       VATCCM        z     +1
056600030618     O          E            riepilogo   2
056700030618     O                                              'Numero CMR...............:'
056800040325     O                       VATCMR              +1
056900990915     O          E            riepilogo   2
057000990921     O                                              'Riferimento strategi.....:'
057100990921     O                       vlrhdl              +1
057200990915     O          E            riepilogo   2
057300990915     O                                              'Giusti...................:'
057400990915     O                       §CTDOK        2   +  1
057500990915     O          E            riepilogo   2
057600010601     O                                              'Sbagliati ...............:'
057700990915     O                       §CTDNO        2   +  1
057800000613
057900040325     Otis7tetps E            testdett          2
058000000613     O                                              'Upload via Internet'
058100000613     O                                           +1 'Traduzione TIVIN00R -'
058200040325     O                                           +1 'EDIVATWR'
058300000613     O                                           +1 'Dettaglio colli'
058400000616     O          E            testdett    3
058500000613     O                                           +2 'N° rec'
058600000613     O                                           +3 'Anteprima contenuto'
058700000616     O          E            rigadett    2
058800000613     O                       rrnum               +2
058900000621     O                       recko               +3
059000000616     O          E            findett     2
059100000613     O                       wrkdata
059200000613     O                       wrkora              +1
059300000613     O                       procname            +1
059400000616     O          E            findett     2
059500000613     O                                              'Cliente..................:'
059600040325     O                       VATCCM        z     +1
059700030618     O          E            findett     2
059800030618     O                                              'Numero CMR...............:'
059900040325     O                       VATCMR              +1
060000000616     O          E            findett     2
060100000613     O                                              'Riferimento strategi.....:'
060200000613     O                       vlrhdl              +1
060300000616     O          E            findett     2
060400000613     O                                              'Giusti...................:'
060500000613     O                       §CTDOK        2   +  1
060600000616     O          E            findett     2
060700010601     O                                              'Sbagliati ...............:'
060800000613     O                       §CTDNO        2   +  1
060900000613** CMD - COMANDI CL
061000040325OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TETP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
061100040325OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TETPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
061200040325DLTOVR FILE(TIS7TETP TIS7TETPS) LVL(*)
061300000613
061400000613
