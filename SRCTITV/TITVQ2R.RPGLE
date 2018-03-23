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
004200060109     D  a_vatatb                      1
004300060109     D  a_vatccm                      7
004400060109     D  a_vatlnp                      3
004500060109     D  a_vataas                      4
004600060109     D  a_vatmgs                      4
004700060109     D  a_vatnrs                      2
004800060109     D  a_vatnsp                      7
004900070420     D  a_vatldv              18     28
005000060109     D  a_vattrc                      1
005100080327     D  a_vatnot                     20
005200010330
005300010330
005400010330
005500990915     C                   time                    wrkdata
005600990915     C                   time                    wrkora
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000010601     C*
006100010601     C                   exsr      opeini
006200010612     C                   exsr      rwvat
006300010601     C*
006400010601     C                   seton                                        lr
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007100010601     C*--------------------------------------------------------
007200010601     C     PREELA        BEGSR
007300010601     C*
007400010601     C* SE OCCORRE SPEDIRE IN FILIALE
007500010601     C                   if        invfil <> *zeros and
007600010601     C                             flgGiro = '0'
007700010601     C*
007800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007900010601     C                   eval      flgGiro = '1'
008000000724     C*
008100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300010601     C     invfil        chain     azorg01l
008400000724     C                   if        %found
008500000724     C                   movel(p)  CMD(1)        depcmd
008600020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008700000724     C*
008800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008900011123     C                   Z-ADD     150           LENGH            15 5
009000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000724     C                   PARM                    depcmd
009200000724     C                   PARM                    LENGH
009300000724     C*
009400000724     C                   endif
009500000616     C*
009600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009700000616     C                   movel(p)  CMD(2)        depcmd
009800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009900000616     C*
010000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010100011123     C                   Z-ADD     150           LENGH            15 5
010200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010300000616     C                   PARM                    depcmd
010400000616     C                   PARM                    LENGH
010500000613     C*
010600010705     C                   if        not %open(tis7t9ps)
010700010705     C                   open      tis7t9ps
010800030407     C   50              except    testdett
010900000613     C                   endif
011000010601     C*
011100010601     C                   endif
011200010601     C*
011300010601     C                   ENDSR
011400010601     C***
011500010601
011600010601
011700010601
011800010601
011900010601
012000010601     C*--------------------------------------------------------
012100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012200010601     C*--------------------------------------------------------
012300010601     C     ENDELA        BEGSR
012400990915     C*
012500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012600000613     C*
012700010705     C                   if        %open(tis7t9ps)
012800030407     C   50              except    findett
012900010705     C                   close     tis7t9ps
013000000613     C                   endif
013100000616     C*
013200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013300011123     C                   Z-ADD     150           LENGH            15 5
013400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    CMD(3)
013600000616     C                   PARM                    LENGH
013700000616     C*
013800010601     C                   ENDSR
013900010601     C***
014000010601
014100010601
014200010601
014300010601
014400000613
014500000613     C*--------------------------------------------------------
014600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014700000613     C*--------------------------------------------------------
014800000621     C     STPR          BEGSR
014900000613     C*
015000010705     C                   if        not %open(tis7t9p)
015100010705     C                   open      tis7t9p
015200990915     C                   endif
015300990915     C*
015400030407     C   50              except    riepilogo
015500990915     C*
015600010705     C                   if        %open(tis7t9p)
015700010705     C                   close     tis7t9p
015800990914     C                   endif
015900000613     C*
016000000613     C                   ENDSR
016100000613     C***
016200000613
016300010330
016400010601
016500010601
016600010601
016700010330     C*--------------------------------------------------------
016800020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
016900010330     C*--------------------------------------------------------
017000010612     C     RWVAT         BEGSR
017100010330     C*
017200010330     C                   if        not %open(tivin00r)
017300010330     C                   open      tivin00r
017400010330     C                   endif
017500020916     C                   if        not %open(fivatwwr)
017600020916     C                   open      fivatwwr
017700010330     C                   endif
017800010330     C*
017900010604     C                   clear                   §CTROK
018000010604     C                   clear                   §CTRMO
018100010604     C                   clear                   §CTRNO
018200010330     C*
018300010330     C                   DO        *HIVAL
018400010330     C*
018500010330     C                   READ      tivin00r                               70
018600010618     C*
018700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018800010618     C                   if        vinflg = '1'
018900010618     C                   eval      flgOk = '1'
019000010618     C                   endif
019100010618     C*
019200010330     C                   if        vindta > *blanks
019300010330     C                   add       1             rrnum
019400010330     C*
019500010601     C                   if        *in70 = *off and
019600010330     C                             (vinflg = *blanks
019700010330     C                              or vinflg = '0'
019800010330     C                              or vinflg = '2')
019900010330     C*
020000010330     C                   clear                   vinmsg
020100010613     C                   eval      fnvataar = vindta
020200010601     C*
020300010330     C                   exsr      inzvar
020400010330     C                   exsr      defcam
020500010612     C                   exsr      impvat
020600010601     C*
020700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020800010612     C                   eval      depfil = VATLNP
020900010601     C                   exsr      repfil
021000010601     C                   if        depfil = invfil
021100021014     C                   if        vlrpoi = 999
021200021010     C                   MOVE(P)   invfil        VATFGS
021300021014     C                   else
021400021014     C                   MOVE(P)   vlrpoi        VATFGS
021500021014     C                   endif
021600010601     C*
021700010601     C                   exsr      PREELA
021800010601     C*
021900010604     C* Ebbene...
022000010604     C*
022100010604     C  N31              ADD       1             §CTROK            7 0
022200010604     C   32              ADD       1             §CTRMO            7 0
022300010604     C   31              ADD       1             §CTRNO            7 0
022400020916     C  N31              WRITE     FIVAT000
022500010604     C*
022600010604     C                   if        *in31 = *off and
022700010604     C                             *in32 = *off
022800010604     C                   eval      vinflg = '1'
022900010604     C                   else
023000010604     C                   eval      recko = vindta
023100010705     C                   if        %open(tis7t9ps)
023200030407     C   50              except    rigadett
023300010612     C                   endif
023400010604     C                   eval      vinflg = '2'
023500010604     C                   endif
023600010604     C                   endif
023700010601     C*
023800010604     C                   endif
023900010604     C*
024000010330     C                   else
024100010330     C                   eval      vinflg = '1'
024200010330     C                   endif
024300010601     C*
024400010601     C  N70              update    tivin000
024500010330     C*
024600010330     C  N70              ENDdo
024700010601     C
024800010601     C*
024900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025000010601     C                   if        cntNonEl = *zeros or
025100010601     C                             flgMulti = '0'
025200010330     C* Se non ci sono record con errori ...
025300010601     C                   if        §ctrno = 0 and
025400010604     C                             §ctrmo = 0 and
025500010601     C                             flgStato <> '2'
025600010330     C* ... restituisco esito OK.
025700010330     C                   eval      wrkesito = '0'
025800010330     C                   else
025900010330     C                   if        §ctrok > 0
026000010330     C                   eval      wrkesito = '1'
026100010330     C                   else
026200010615     C                   if        flgOk = '0'
026300010615     C                   eval      wrkesito = '2'
026400010615     C                   else
026500010615     C                   eval      wrkesito = '6'
026600010615     C                   endif
026700010330     C                   endif
026800010330     C                   endif
026900010601     C                   else
027000010601     C                   eval      wrkesito = '9'
027100010601     C                   endif
027200010330     C*
027300010330     C                   if        %open(tivin00r)
027400010330     C                   close     tivin00r
027500010330     C                   endif
027600020916     C                   if        %open(fivatwwr)
027700020916     C                   close     fivatwwr
027800010330     C                   endif
027900010601     C*
028000010601     C                   if        vlrpoi <> 999
028100010601     C                   eval      invfil = vlrpoi
028200010601     C                   endif
028300010330     C*
028400010330     C                   if        §ctrok > 0
028500010601     C                             and invfil > *zeros
028600010330     C                   exsr      invio
028700010330     C                   endif
028800010601     C*
028900010612     C                   if        flgGiro = '1'
029000010601     C                   exsr      endela
029100010612     C                   endif
029200010330     C*
029300010330     C                   ENDSR
029400010330     C***
029500010330
029600010601
029700010601
029800010601
029900010601
030000010330     C*----------------------------------------------------*
030100010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030200010330     C*----------------------------------------------------*
030300010330     C     INZVAR        BEGSR
030400010330     C*
030500010330     C*
030600010330     C                   ENDSR
030700010330     C*----------------------------------------------------*
030800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030900010330     C*----------------------------------------------------*
031000010330     C     DEFCAM        BEGSR
031100010330     C*
031200020916     C                   CLEAR                   FIVAT000
031300010330     C*
031400010330     C                   ENDSR
031500010705     C*----------------------------------------------------*
031600010705
031700010705
031800010705
031900010705
032000010705     C*----------------------------------------------------*
032100020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
032200010705     C*----------------------------------------------------*
032300010705     C     IMPVAT        BEGSR
032400010705     C*
032500010705     C                   SETOFF                                       3132
032600010705     C*
032700010705     C* Reperimento campi ALFA
032800010705     C                   EVAL      VATATB=a_vatatb
032900010705     C                   EVAL      VATTRC=a_vattrc
033000020725     C                   EVAL      VATNOT=%trim(a_vatnot)
033100010705     C*
033200010705     C                   EVAL      PiStr=%trim(a_vatccm)
033300010705     C                   EXSR      CHKNUM
033400010705     C                   IF        PiInt=*on
033500010705     C                   Z-ADD     PiVal         VATCCM
033600010705     C                   ELSE
033700010705     C                   SETON                                        31
033800010705     C                   EVAL      vinmsg = %trimr(vinmsg)
033900010705     C                             + ' ' + 'VATCCM'
034000010705     C                   ENDIF
034100010705     C*
034200010705     C                   EVAL      PiStr=%trim(a_vatlnp)
034300010705     C                   EXSR      CHKNUM
034400010705     C                   IF        PiInt=*on
034500010705     C                   Z-ADD     PiVal         VATLNP
034600010705     C                   ELSE
034700010705     C                   SETON                                        31
034800010705     C                   EVAL      vinmsg = %trimr(vinmsg)
034900021010     C                             + ' ' + 'VATLNP'
035000010705     C                   ENDIF
035100010705     C*
035200060109     C                   EVAL      PiStr=%trim(a_vataas)
035300060109     C                   EXSR      CHKNUM
035400060109     C                   IF        PiInt=*on
035500060109     C                   Z-ADD     PiVal         VATAAS
035600060109     C                   ELSE
035700060109     C                   SETON                                        32
035800060109     C                   MOVEL     datcor        VATAAS
035900060109     C                   EVAL      vinmsg = %trimr(vinmsg)
036000060109     C                             + ' ' + 'VATAAS'
036100060109     C                   ENDIF
036200010705     C*
036300070420     C*                  EVAL      PiStr=%trim(a_vatnrs)
036400070420     C*                  IF        PiStr = *blanks
036500070420     C*                  EVAL      PiStr = *zeros
036600070420     C*                  ENDIF
036700070420     C*                  EXSR      CHKNUM
036800070420     C*                  IF        PiInt=*on
036900070420     C*                  Z-ADD     PiVal         VATNRS
037000070420     C*                  ELSE
037100070420     C*                  SETON                                        31
037200070420     C*                  EVAL      vinmsg = %trimr(vinmsg)
037300070420     C*                            + ' ' + 'VATNRS'
037400070420     C*                  ENDIF
037500010705     C*
037600010705     C                   EVAL      PiStr=%trim(a_vatnsp)
037700010705     C                   EXSR      CHKNUM
037800010705     C                   IF        PiInt=*on
037900010705     C                   Z-ADD     PiVal         VATNSP
038000010705     C                   ELSE
038100010705     C                   SETON                                        31
038200010705     C                   EVAL      vinmsg = %trimr(vinmsg)
038300010705     C                             + ' ' + 'VATNSP'
038400010705     C                   ENDIF
038500010705     C*
038600010705     C                   ENDSR
038700010705     C*----------------------------------------------------*
038800010705
038900010705
039000010705
039100010705
039200010705
039300010705     C*----------------------------------------------------*
039400010705     C*  CONTROLLO NUMERICITA' CAMPI
039500010705     C*----------------------------------------------------*
039600010705     C     CHKNUM        BEGSR
039700010705     C*
039800010705     C                   call(e)   'ISNUMERIC'
039900010705     C                   PARM                    PiStr            30
040000010705     C                   PARM      ','           PiDecChr          1
040100010705     C                   PARM      *ZEROS        PiVal            30 9
040200010705     C                   PARM      '0'           PiInt             1
040300010705     C                   PARM      '0'           PiNum             1
040400010705     C                   IF        %error
040500010705     C                   EVAL      PiInt=*off
040600010705     C                   ENDIF
040700010705     C*
040800010705     C                   ENDSR
040900010705     C*----------------------------------------------------*
041000010705
041100010705
041200010705
041300010601
041400010601
041500010601
041600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
041700010601     C     repfil        BEGSR
041800010601     C*
041900010601     C                   if        invfil = *zeros and
042000010601     C                             depfil > *zeros and
042100010613     C                             (vinflg = *blanks or
042200010613     C                              vinflg = *zeros)
042300010601     C
042400010601     C                   eval      invfil = depfil
042500010601     C                   endif
042600010601     C*
042700010601     C                   if        depfil <> invfil and
042800010601     C                             invfil > *zeros
042900010601     C                   eval      flgMulti = '1'
043000010601     C                   if        vinflg = *blanks
043100010601     C                   add       1             cntNonEl
043200010601     C                   endif
043300010601     C                   endif
043400010601     C*
043500010601     C                   if        vinflg = '2'
043600010601     C                   eval      flgStato = '2'
043700010601     C                   endif
043800010601     C*
043900010601     C                   ENDSR
044000010601     C***
044100010601
044200010601
044300010601
044400010330
044500010330
044600010330
044700990920      /TITLE Invio dei dati al punto operativo.
044800000613     C     invio         BEGSR
044900990920     C*
045000990920     C                   reset                   dscmz
045100010601     C                   move      invfil        cmzdst
045200990920     C                   eval      cmzfld = vlrfou
045300990920     C                   eval      cmzmbd = vlrhdl
045400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
045500000710     C                   if        prmfir = *blanks
045600020916     C                   eval      cmzfla = 'FIVAT00F'
045700020916     C                   eval      cmzmba = 'FIVAT00F'
045800000710     C                   else
045900000710     C                   eval      cmzfla = prmfir
046000000710     C                   eval      cmzmba = prmfir
046100000710     C                   endif
046200990920     C                   eval      cmznrr = *zeros
046300990920     C                   move      §ctrok        cmznrr
046400021003     C                   eval      cmzlba = vlrfl1
046500990920     C                   call(e)   'TIS711C'
046600990920     C                   parm                    dscmz
046700990921     C                   parm      *blanks       esito
046800990920     C                   if        %error
046900990920     C                             or cmzerr = '1'
047000990921     C                             or esito  = '1'
047100000710     C                   eval      wrkesito = '3'
047200990920     C                   endif
047300990920     C*
047400000613     C                   ENDSR
047500990910
047600010601
047700010601
047800010601
047900010601
048000010601      /TITLE Invio dei dati al punto operativo.
048100010601     C     opeini        BEGSR
048200010601     C*
048300010601     C* Inizializzo flag e contatori operativi
048400010601     C                   movel     '0'           flgGiro           1
048500010601     C                   movel     '0'           flgMulti          1
048600010601     C                   movel     '1'           flgStato          1
048700010615     C                   movel     '0'           flgOk             1
048800010601     C                   z-add     *zeros        cntNonEl         10 0
048900010601     C                   z-add     *zeros        depfil            3 0
049000010601     C                   z-add     *zeros        invfil            3 0
049100030407     C*
049200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
049300030407     C                   if        %subst(vlrppt:1:1) = 'S'
049400030407     C                   seton                                        50
049500030407     C                   else
049600030407     C                   setoff                                       50
049700030407     C                   endif
049800010601     C*
049900010601     C                   ENDSR
050000010601     C***
050100010601
050200010601
050300010601
050400010330
050500010330
050600000613     C     *inzsr        BEGSR
050700990910     C*
050800990910     C     *entry        plist
050900990920     C                   parm                    tivlrds
051000990921     C                   parm      wrkesito      esito
051100000724     C                   parm                    prmlit
051200000710     C                   parm                    prmfir
051300010330     C*
051400010330     C* CALCOLA LA DATA CORRENTE
051500010330     C                   time                    wn14             14 0
051600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
051700010330     C                   z-add     wn8           g08dat
051800010330     C                   z-add     *zeros        g08inv
051900010330     C                   movel     '0'           g08err
052000010330     C                   call      'XSRDA8'
052100010330     C                   parm                    wlbda8
052200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
052300000613     C*
052400000613     C                   ENDSR
052500000613     C***
052600990908
052700010705     Otis7t9p   E            riepilogo         2
052800990915     O                                              'Upload via Internet'
052900990915     O                                           +1 'Traduzione TIVIN00R -'
053000020916     O                                           +1 'FIVATWWR'
053100990915     O                                           +1 'Testate bolle'
053200990915     O          E            riepilogo   2
053300990915     O                       wrkdata
053400990915     O                       wrkora              +1
053500990915     O                       procname            +1
053600990915     O          E            riepilogo   2
053700990915     O                                              'Cliente..................:'
053800010612     O                       VATCCM        z     +1
053900990915     O          E            riepilogo   2
054000990920     O                                              'Riferimento Strategi.....:'
054100990920     O                       vlrhdl              +1
054200990915     O          E            riepilogo   2
054300990915     O                                              'Giusti...................:'
054400971022     O                       §CTROK        2   +  1
054500990915     O          E            riepilogo   2
054600990915     O                                              'Sbagliati e corretti.....:'
054700971022     O                       §CTRMO        2   +  1
054800990915     O          E            riepilogo   2
054900990915     O                                              'Sbagliati e scartati.....:'
055000971022     O                       §CTRNO        2   +  1
055100000613
055200010705     Otis7t9ps  E            testdett          2
055300000613     O                                              'Upload via Internet'
055400000613     O                                           +1 'Traduzione TIVIN00R -'
055500020916     O                                           +1 'FIVATWWR'
055600000613     O                                           +1 'Testate bolle'
055700000616     O          E            testdett    3
055800000613     O                                           +2 'N° rec'
055900000613     O                                           +3 'Anteprima contenuto'
056000000616     O          E            rigadett    2
056100000613     O                       rrnum               +2
056200000621     O                       recko               +3
056300000616     O          E            findett     2
056400000613     O                       wrkdata
056500000613     O                       wrkora              +1
056600000613     O                       procname            +1
056700000616     O          E            findett     2
056800000613     O                                              'Cliente..................:'
056900010612     O                       VATCCM        z     +1
057000000616     O          E            findett     2
057100000613     O                                              'Riferimento Strategi.....:'
057200000613     O                       vlrhdl              +1
057300000616     O          E            findett     2
057400000613     O                                              'Giusti...................:'
057500000613     O                       §CTROK        2   +  1
057600000616     O          E            findett     2
057700000613     O                                              'Sbagliati e corretti.....:'
057800000613     O                       §CTRMO        2   +  1
057900000616     O          E            findett     2
058000000613     O                                              'Sbagliati e scartati.....:'
058100000613     O                       §CTRNO        2   +  1
058200000613** CMD - COMANDI CL
058300011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
058400011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
058500010705DLTOVR FILE(TIS7T9P TIS7T9PS) LVL(*)
058600010606
058700000613
