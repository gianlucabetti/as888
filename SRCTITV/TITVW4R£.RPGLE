000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020916     FFIVATwwr  O    E             DISK    usropn
000700010705     Ftis7t9p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010705     Ftis7t9ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011123     D depcmd          s            150    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000010612     D*
004100010612     D fnvataar        ds                  inz
004200010705     D  a_vatatb               1      1
004300010705     D  a_vatccm               2      8
004400010705     D  a_vatlnp               9     11
004500010705     D  a_vataas              12     15
004600010705     D  a_vatnrs              16     17
004700010705     D  a_vatnsp              18     24
004800010705     D  a_vattrc              25     25
004900010705     D  a_vatnot              26     60
005000010330
005100010330
005200010330
005300990915     C                   time                    wrkdata
005400990915     C                   time                    wrkora
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800010601     C*
005900010601     C                   exsr      opeini
006000010612     C                   exsr      rwvat
006100010601     C*
006200010601     C                   seton                                        lr
006300010601
006400010601
006500010601
006600010601
006700010601     C*--------------------------------------------------------
006800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006900010601     C*--------------------------------------------------------
007000010601     C     PREELA        BEGSR
007100010601     C*
007200010601     C* SE OCCORRE SPEDIRE IN FILIALE
007300010601     C                   if        invfil <> *zeros and
007400010601     C                             flgGiro = '0'
007500010601     C*
007600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007700010601     C                   eval      flgGiro = '1'
007800000724     C*
007900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100010601     C     invfil        chain     azorg01l
008200000724     C                   if        %found
008300000724     C                   movel(p)  CMD(1)        depcmd
008400020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008500000724     C*
008600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008700011123     C                   Z-ADD     150           LENGH            15 5
008800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000724     C                   PARM                    depcmd
009000000724     C                   PARM                    LENGH
009100000724     C*
009200000724     C                   endif
009300000616     C*
009400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009500000616     C                   movel(p)  CMD(2)        depcmd
009600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009700000616     C*
009800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009900011123     C                   Z-ADD     150           LENGH            15 5
010000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010100000616     C                   PARM                    depcmd
010200000616     C                   PARM                    LENGH
010300000613     C*
010400010705     C                   if        not %open(tis7t9ps)
010500010705     C                   open      tis7t9ps
010600030407     C   50              except    testdett
010700000613     C                   endif
010800010601     C*
010900010601     C                   endif
011000010601     C*
011100010601     C                   ENDSR
011200010601     C***
011300010601
011400010601
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012000010601     C*--------------------------------------------------------
012100010601     C     ENDELA        BEGSR
012200990915     C*
012300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012400000613     C*
012500010705     C                   if        %open(tis7t9ps)
012600030407     C   50              except    findett
012700010705     C                   close     tis7t9ps
012800000613     C                   endif
012900000616     C*
013000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013100011123     C                   Z-ADD     150           LENGH            15 5
013200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    CMD(3)
013400000616     C                   PARM                    LENGH
013500000616     C*
013600010601     C                   ENDSR
013700010601     C***
013800010601
013900010601
014000010601
014100010601
014200000613
014300000613     C*--------------------------------------------------------
014400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014500000613     C*--------------------------------------------------------
014600000621     C     STPR          BEGSR
014700000613     C*
014800010705     C                   if        not %open(tis7t9p)
014900010705     C                   open      tis7t9p
015000990915     C                   endif
015100990915     C*
015200030407     C   50              except    riepilogo
015300990915     C*
015400010705     C                   if        %open(tis7t9p)
015500010705     C                   close     tis7t9p
015600990914     C                   endif
015700000613     C*
015800000613     C                   ENDSR
015900000613     C***
016000000613
016100010330
016200010601
016300010601
016400010601
016500010330     C*--------------------------------------------------------
016600020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
016700010330     C*--------------------------------------------------------
016800010612     C     RWVAT         BEGSR
016900010330     C*
017000010330     C                   if        not %open(tivin00r)
017100010330     C                   open      tivin00r
017200010330     C                   endif
017300020916     C                   if        not %open(fivatwwr)
017400020916     C                   open      fivatwwr
017500010330     C                   endif
017600010330     C*
017700010604     C                   clear                   §CTROK
017800010604     C                   clear                   §CTRMO
017900010604     C                   clear                   §CTRNO
018000010330     C*
018100010330     C                   DO        *HIVAL
018200010330     C*
018300010330     C                   READ      tivin00r                               70
018400010618     C*
018500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018600010618     C                   if        vinflg = '1'
018700010618     C                   eval      flgOk = '1'
018800010618     C                   endif
018900010618     C*
019000010330     C                   if        vindta > *blanks
019100010330     C                   add       1             rrnum
019200010330     C*
019300010601     C                   if        *in70 = *off and
019400010330     C                             (vinflg = *blanks
019500010330     C                              or vinflg = '0'
019600010330     C                              or vinflg = '2')
019700010330     C*
019800010330     C                   clear                   vinmsg
019900010613     C                   eval      fnvataar = vindta
020000010601     C*
020100010330     C                   exsr      inzvar
020200010330     C                   exsr      defcam
020300010612     C                   exsr      impvat
020400010601     C*
020500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020600010612     C                   eval      depfil = VATLNP
020700010601     C                   exsr      repfil
020800010601     C                   if        depfil = invfil
020900021014     C                   if        vlrpoi = 999
021000021010     C                   MOVE(P)   invfil        VATFGS
021100021014     C                   else
021200021014     C                   MOVE(P)   vlrpoi        VATFGS
021300021014     C                   endif
021400010601     C*
021500010601     C                   exsr      PREELA
021600010601     C*
021700010604     C* Ebbene...
021800010604     C*
021900010604     C  N31              ADD       1             §CTROK            7 0
022000010604     C   32              ADD       1             §CTRMO            7 0
022100010604     C   31              ADD       1             §CTRNO            7 0
022200020916     C  N31              WRITE     FIVAT000
022300010604     C*
022400010604     C                   if        *in31 = *off and
022500010604     C                             *in32 = *off
022600010604     C                   eval      vinflg = '1'
022700010604     C                   else
022800010604     C                   eval      recko = vindta
022900010705     C                   if        %open(tis7t9ps)
023000030407     C   50              except    rigadett
023100010612     C                   endif
023200010604     C                   eval      vinflg = '2'
023300010604     C                   endif
023400010604     C                   endif
023500010601     C*
023600010604     C                   endif
023700010604     C*
023800010330     C                   else
023900010330     C                   eval      vinflg = '1'
024000010330     C                   endif
024100010601     C*
024200010601     C  N70              update    tivin000
024300010330     C*
024400010330     C  N70              ENDdo
024500010601     C
024600010601     C*
024700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024800010601     C                   if        cntNonEl = *zeros or
024900010601     C                             flgMulti = '0'
025000010330     C* Se non ci sono record con errori ...
025100010601     C                   if        §ctrno = 0 and
025200010604     C                             §ctrmo = 0 and
025300010601     C                             flgStato <> '2'
025400010330     C* ... restituisco esito OK.
025500010330     C                   eval      wrkesito = '0'
025600010330     C                   else
025700010330     C                   if        §ctrok > 0
025800010330     C                   eval      wrkesito = '1'
025900010330     C                   else
026000010615     C                   if        flgOk = '0'
026100010615     C                   eval      wrkesito = '2'
026200010615     C                   else
026300010615     C                   eval      wrkesito = '6'
026400010615     C                   endif
026500010330     C                   endif
026600010330     C                   endif
026700010601     C                   else
026800010601     C                   eval      wrkesito = '9'
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        %open(tivin00r)
027200010330     C                   close     tivin00r
027300010330     C                   endif
027400020916     C                   if        %open(fivatwwr)
027500020916     C                   close     fivatwwr
027600010330     C                   endif
027700010601     C*
027800010601     C                   if        vlrpoi <> 999
027900010601     C                   eval      invfil = vlrpoi
028000010601     C                   endif
028100010330     C*
028200010330     C                   if        §ctrok > 0
028300010601     C                             and invfil > *zeros
028400010330     C                   exsr      invio
028500010330     C                   endif
028600010601     C*
028700010612     C                   if        flgGiro = '1'
028800010601     C                   exsr      endela
028900010612     C                   endif
029000010330     C*
029100010330     C                   ENDSR
029200010330     C***
029300010330
029400010601
029500010601
029600010601
029700010601
029800010330     C*----------------------------------------------------*
029900010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030000010330     C*----------------------------------------------------*
030100010330     C     INZVAR        BEGSR
030200010330     C*
030300010330     C*
030400010330     C                   ENDSR
030500010330     C*----------------------------------------------------*
030600010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030700010330     C*----------------------------------------------------*
030800010330     C     DEFCAM        BEGSR
030900010330     C*
031000020916     C                   CLEAR                   FIVAT000
031001070125     C* Imposto i valori di default...
031002070125     C                   EVAL      VATCCM = 1160951
031003070125     C                   EVAL      VATLNP = 116
031006070125     C* ... e poi verifico se sono stati passati come parametri
031007070125     C                   IF        vlrppt > *blanks
031008070125     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031009070125     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031010070125     C                   EXSR      CHKNUM
031011070125     C                   IF        PiInt=*on
031012070125     C                   Z-ADD     PiVal         VATCCM
031013070125     C                   ENDIF
031014070125     C                   ENDIF
031015070125     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031016070125     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031017070125     C                   EXSR      CHKNUM
031018070125     C                   IF        PiInt=*on
031019070125     C                   Z-ADD     PiVal         VATLNP
031020070125     C                   ENDIF
031021070125     C                   ENDIF
031029070125     C                   ENDIF
031100010330     C*
031200010330     C                   ENDSR
031300010705     C*----------------------------------------------------*
031400010705
031500010705
031600010705
031700010705
031800010705     C*----------------------------------------------------*
031900020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
032000010705     C*----------------------------------------------------*
032100010705     C     IMPVAT        BEGSR
032200010705     C*
032300010705     C                   SETOFF                                       3132
032400010705     C*
032500010705     C* Reperimento campi ALFA
032600010705     C                   EVAL      VATATB=a_vatatb
032700010705     C                   EVAL      VATTRC=a_vattrc
032800020725     C                   EVAL      VATNOT=%trim(a_vatnot)
032900010705     C*
033000070125     C*                  EVAL      PiStr=%trim(a_vatccm)
033100070125     C*                  EXSR      CHKNUM
033200070125     C*                  IF        PiInt=*on
033300070125     C*                  Z-ADD     PiVal         VATCCM
033400070125     C*                  ELSE
033500070125     C*                  SETON                                        31
033600070125     C*                  EVAL      vinmsg = %trimr(vinmsg)
033700070125     C*                            + ' ' + 'VATCCM'
033800070125     C*                  ENDIF
033900070125     C*
034000070125     C*                  EVAL      PiStr=%trim(a_vatlnp)
034100070125     C*                  EXSR      CHKNUM
034200070125     C*                  IF        PiInt=*on
034300070125     C*                  Z-ADD     PiVal         VATLNP
034400070125     C*                  ELSE
034500070125     C*                  SETON                                        31
034600070125     C*                  EVAL      vinmsg = %trimr(vinmsg)
034700070125     C*                            + ' ' + 'VATLNP'
034800070125     C*                  ENDIF
034900010705     C*
035000010705     C                   EVAL      PiStr=%trim(a_vataas)
035100010705     C                   EXSR      CHKNUM
035200010705     C                   IF        PiInt=*on
035300010705     C                   Z-ADD     PiVal         VATAAS
035400010705     C                   ELSE
035500010705     C                   SETON                                        31
035600010705     C                   EVAL      vinmsg = %trimr(vinmsg)
035700010705     C                             + ' ' + 'VATAAS'
035800010705     C                   ENDIF
035900010705     C*
036000010705     C                   EVAL      PiStr=%trim(a_vatnrs)
036100010705     C                   IF        PiStr = *blanks
036200010705     C                   EVAL      PiStr = *zeros
036300010705     C                   ENDIF
036400010705     C                   EXSR      CHKNUM
036500010705     C                   IF        PiInt=*on
036600010705     C                   Z-ADD     PiVal         VATNRS
036700010705     C                   ELSE
036800010705     C                   SETON                                        31
036900010705     C                   EVAL      vinmsg = %trimr(vinmsg)
037000010705     C                             + ' ' + 'VATNRS'
037100010705     C                   ENDIF
037200010705     C*
037300010705     C                   EVAL      PiStr=%trim(a_vatnsp)
037400010705     C                   EXSR      CHKNUM
037500010705     C                   IF        PiInt=*on
037600010705     C                   Z-ADD     PiVal         VATNSP
037700010705     C                   ELSE
037800010705     C                   SETON                                        31
037900010705     C                   EVAL      vinmsg = %trimr(vinmsg)
038000010705     C                             + ' ' + 'VATNSP'
038100010705     C                   ENDIF
038200010705     C*
038300010705     C                   ENDSR
038400010705     C*----------------------------------------------------*
038500010705
038600010705
038700010705
038800010705
038900010705
039000010705     C*----------------------------------------------------*
039100010705     C*  CONTROLLO NUMERICITA' CAMPI
039200010705     C*----------------------------------------------------*
039300010705     C     CHKNUM        BEGSR
039400010705     C*
039500010705     C                   call(e)   'ISNUMERIC'
039600010705     C                   PARM                    PiStr            30
039700010705     C                   PARM      ','           PiDecChr          1
039800010705     C                   PARM      *ZEROS        PiVal            30 9
039900010705     C                   PARM      '0'           PiInt             1
040000010705     C                   PARM      '0'           PiNum             1
040100010705     C                   IF        %error
040200010705     C                   EVAL      PiInt=*off
040300010705     C                   ENDIF
040400010705     C*
040500010705     C                   ENDSR
040600010705     C*----------------------------------------------------*
040700010705
040800010705
040900010705
041000010601
041100010601
041200010601
041300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
041400010601     C     repfil        BEGSR
041500010601     C*
041600010601     C                   if        invfil = *zeros and
041700010601     C                             depfil > *zeros and
041800010613     C                             (vinflg = *blanks or
041900010613     C                              vinflg = *zeros)
042000010601     C
042100010601     C                   eval      invfil = depfil
042200010601     C                   endif
042300010601     C*
042400010601     C                   if        depfil <> invfil and
042500010601     C                             invfil > *zeros
042600010601     C                   eval      flgMulti = '1'
042700010601     C                   if        vinflg = *blanks
042800010601     C                   add       1             cntNonEl
042900010601     C                   endif
043000010601     C                   endif
043100010601     C*
043200010601     C                   if        vinflg = '2'
043300010601     C                   eval      flgStato = '2'
043400010601     C                   endif
043500010601     C*
043600010601     C                   ENDSR
043700010601     C***
043800010601
043900010601
044000010601
044100010330
044200010330
044300010330
044400990920      /TITLE Invio dei dati al punto operativo.
044500000613     C     invio         BEGSR
044600990920     C*
044700990920     C                   reset                   dscmz
044800010601     C                   move      invfil        cmzdst
044900990920     C                   eval      cmzfld = vlrfou
045000990920     C                   eval      cmzmbd = vlrhdl
045100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
045200000710     C                   if        prmfir = *blanks
045300020916     C                   eval      cmzfla = 'FIVAT00F'
045400020916     C                   eval      cmzmba = 'FIVAT00F'
045500000710     C                   else
045600000710     C                   eval      cmzfla = prmfir
045700000710     C                   eval      cmzmba = prmfir
045800000710     C                   endif
045900990920     C                   eval      cmznrr = *zeros
046000990920     C                   move      §ctrok        cmznrr
046100021003     C                   eval      cmzlba = vlrfl1
046200990920     C                   call(e)   'TIS711C'
046300990920     C                   parm                    dscmz
046400990921     C                   parm      *blanks       esito
046500990920     C                   if        %error
046600990920     C                             or cmzerr = '1'
046700990921     C                             or esito  = '1'
046800000710     C                   eval      wrkesito = '3'
046900990920     C                   endif
047000990920     C*
047100000613     C                   ENDSR
047200990910
047300010601
047400010601
047500010601
047600010601
047700010601      /TITLE Invio dei dati al punto operativo.
047800010601     C     opeini        BEGSR
047900010601     C*
048000010601     C* Inizializzo flag e contatori operativi
048100010601     C                   movel     '0'           flgGiro           1
048200010601     C                   movel     '0'           flgMulti          1
048300010601     C                   movel     '1'           flgStato          1
048400010615     C                   movel     '0'           flgOk             1
048500010601     C                   z-add     *zeros        cntNonEl         10 0
048600010601     C                   z-add     *zeros        depfil            3 0
048700010601     C                   z-add     *zeros        invfil            3 0
048800030407     C*
048900030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
049000030407     C                   if        %subst(vlrppt:1:1) = 'S'
049100030407     C                   seton                                        50
049200030407     C                   else
049300030407     C                   setoff                                       50
049400030407     C                   endif
049500010601     C*
049600010601     C                   ENDSR
049700010601     C***
049800010601
049900010601
050000010601
050100010330
050200010330
050300000613     C     *inzsr        BEGSR
050400990910     C*
050500990910     C     *entry        plist
050600990920     C                   parm                    tivlrds
050700990921     C                   parm      wrkesito      esito
050800000724     C                   parm                    prmlit
050900000710     C                   parm                    prmfir
051000010330     C*
051100010330     C* CALCOLA LA DATA CORRENTE
051200010330     C                   time                    wn14             14 0
051300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
051400010330     C                   z-add     wn8           g08dat
051500010330     C                   z-add     *zeros        g08inv
051600010330     C                   movel     '0'           g08err
051700010330     C                   call      'XSRDA8'
051800010330     C                   parm                    wlbda8
051900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
052000000613     C*
052100000613     C                   ENDSR
052200000613     C***
052300990908
052400010705     Otis7t9p   E            riepilogo         2
052500990915     O                                              'Upload via Internet'
052600990915     O                                           +1 'Traduzione TIVIN00R -'
052700020916     O                                           +1 'FIVATWWR'
052800990915     O                                           +1 'Testate bolle'
052900990915     O          E            riepilogo   2
053000990915     O                       wrkdata
053100990915     O                       wrkora              +1
053200990915     O                       procname            +1
053300990915     O          E            riepilogo   2
053400990915     O                                              'Cliente..................:'
053500010612     O                       VATCCM        z     +1
053600990915     O          E            riepilogo   2
053700990920     O                                              'Riferimento Strategi.....:'
053800990920     O                       vlrhdl              +1
053900990915     O          E            riepilogo   2
054000990915     O                                              'Giusti...................:'
054100971022     O                       §CTROK        2   +  1
054200990915     O          E            riepilogo   2
054300990915     O                                              'Sbagliati e corretti.....:'
054400971022     O                       §CTRMO        2   +  1
054500990915     O          E            riepilogo   2
054600990915     O                                              'Sbagliati e scartati.....:'
054700971022     O                       §CTRNO        2   +  1
054800000613
054900010705     Otis7t9ps  E            testdett          2
055000000613     O                                              'Upload via Internet'
055100000613     O                                           +1 'Traduzione TIVIN00R -'
055200020916     O                                           +1 'FIVATWWR'
055300000613     O                                           +1 'Testate bolle'
055400000616     O          E            testdett    3
055500000613     O                                           +2 'N° rec'
055600000613     O                                           +3 'Anteprima contenuto'
055700000616     O          E            rigadett    2
055800000613     O                       rrnum               +2
055900000621     O                       recko               +3
056000000616     O          E            findett     2
056100000613     O                       wrkdata
056200000613     O                       wrkora              +1
056300000613     O                       procname            +1
056400000616     O          E            findett     2
056500000613     O                                              'Cliente..................:'
056600010612     O                       VATCCM        z     +1
056700000616     O          E            findett     2
056800000613     O                                              'Riferimento Strategi.....:'
056900000613     O                       vlrhdl              +1
057000000616     O          E            findett     2
057100000613     O                                              'Giusti...................:'
057200000613     O                       §CTROK        2   +  1
057300000616     O          E            findett     2
057400000613     O                                              'Sbagliati e corretti.....:'
057500000613     O                       §CTRMO        2   +  1
057600000616     O          E            findett     2
057700000613     O                                              'Sbagliati e scartati.....:'
057800000613     O                       §CTRNO        2   +  1
057900000613** CMD - COMANDI CL
058000011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
058100011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
058200010705DLTOVR FILE(TIS7T9P TIS7T9PS) LVL(*)
058300010606
058400000613
