000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020916     FFIVATwwr  O    E             DISK    usropn
000700040316     Ftitva5p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040316     Ftitva5ps  O    f  198        PRINTER usropn
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
004000010330
004100010330
004200010330
004300990915     C                   time                    wrkdata
004400990915     C                   time                    wrkora
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900010601     C                   exsr      opeini
005000010612     C                   exsr      rwvat
005100010601     C*
005200010601     C                   seton                                        lr
005300010601
005400010601
005500010601
005600010601
005700010601     C*--------------------------------------------------------
005800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005900010601     C*--------------------------------------------------------
006000010601     C     PREELA        BEGSR
006100010601     C*
006200010601     C* SE OCCORRE SPEDIRE IN FILIALE
006300010601     C                   if        invfil <> *zeros and
006400010601     C                             flgGiro = '0'
006500010601     C*
006600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006700010601     C                   eval      flgGiro = '1'
006800000724     C*
006900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007100010601     C     invfil        chain     azorg01l
007200000724     C                   if        %found
007300000724     C                   movel(p)  CMD(1)        depcmd
007400020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007500000724     C*
007600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007700011123     C                   Z-ADD     150           LENGH            15 5
007800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007900000724     C                   PARM                    depcmd
008000000724     C                   PARM                    LENGH
008100000724     C*
008200000724     C                   endif
008300000616     C*
008400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008500000616     C                   movel(p)  CMD(2)        depcmd
008600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008700000616     C*
008800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008900011123     C                   Z-ADD     150           LENGH            15 5
009000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000616     C                   PARM                    depcmd
009200000616     C                   PARM                    LENGH
009300000613     C*
009400040316     C                   if        not %open(titva5ps)
009500040316     C                   open      titva5ps
009600030407     C   50              except    testdett
009700000613     C                   endif
009800010601     C*
009900010601     C                   endif
010000010601     C*
010100010601     C                   ENDSR
010200010601     C***
010300010601
010400010601
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011200010601     C*--------------------------------------------------------
011300010601     C     ENDELA        BEGSR
011400990915     C*
011500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011600000613     C*
011700040316     C                   if        %open(titva5ps)
011800030407     C   50              except    findett
011900040316     C                   close     titva5ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011123     C                   Z-ADD     150           LENGH            15 5
012400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800010601     C                   ENDSR
012900010601     C***
013000010601
013100010601
013200010601
013300010601
013400000613
013500000613     C*--------------------------------------------------------
013600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013700000613     C*--------------------------------------------------------
013800000621     C     STPR          BEGSR
013900000613     C*
014000040316     C                   if        not %open(titva5p)
014100040316     C                   open      titva5p
014200990915     C                   endif
014300990915     C*
014400030407     C   50              except    riepilogo
014500990915     C*
014600040316     C                   if        %open(titva5p)
014700040316     C                   close     titva5p
014800990914     C                   endif
014900000613     C*
015000000613     C                   ENDSR
015100000613     C***
015200000613
015300010330
015400010601
015500010601
015600010601
015700010330     C*--------------------------------------------------------
015800020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
015900010330     C*--------------------------------------------------------
016000010612     C     RWVAT         BEGSR
016100010330     C*
016200010330     C                   if        not %open(tivin00r)
016300010330     C                   open      tivin00r
016400010330     C                   endif
016500020916     C                   if        not %open(fivatwwr)
016600020916     C                   open      fivatwwr
016700010330     C                   endif
016800010330     C*
016900010604     C                   clear                   §CTROK
017000010604     C                   clear                   §CTRMO
017100010604     C                   clear                   §CTRNO
017200010330     C*
017300010330     C                   DO        *HIVAL
017400010330     C*
017500010330     C                   READ      tivin00r                               70
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100010618     C*
018200010330     C                   if        vindta > *blanks
018300010330     C                   add       1             rrnum
018400010330     C*
018500010601     C                   if        *in70 = *off and
018600010330     C                             (vinflg = *blanks
018700010330     C                              or vinflg = '0'
018800010330     C                              or vinflg = '2')
018900010711     C*
019000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019100010711     C                   if        vinflg = *blanks or vinflg = '0'
019200010711     C                   clear                   vinmsg
019300010711     C                   endif
019400010601     C*
019500010330     C                   exsr      inzvar
019600040316     C                   exsr      defcam
019700010601     C*
019800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019900010612     C                   eval      depfil = VATLNP
020000010601     C                   exsr      repfil
020100021010     C                   if        depfil = invfil
020200021014     C                   if        vlrpoi = 999
020300021010     C                   MOVE(P)   invfil        VATFGS
020400021014     C                   else
020500021014     C                   MOVE(P)   vlrpoi        VATFGS
020600021014     C                   endif
020700040316     C*
020800040316     C* Imposto i campi del file e scarico il buffer
020900040316     C                   exsr      impvat
021000010601     C*
021100010601     C                   exsr      PREELA
021200010601     C*
021300010604     C* Ebbene...
021400010604     C*
021500010604     C  N31              ADD       1             §CTROK            7 0
021600010604     C   32              ADD       1             §CTRMO            7 0
021700010604     C   31              ADD       1             §CTRNO            7 0
021800010604     C*
021900010604     C                   if        *in31 = *off and
022000010604     C                             *in32 = *off
022100010604     C                   eval      vinflg = '1'
022200010604     C                   else
022300010604     C                   eval      recko = vindta
022400040316     C                   if        %open(titva5ps)
022500030407     C   50              except    rigadett
022600010612     C                   endif
022700010604     C                   eval      vinflg = '2'
022800010604     C                   endif
022900010604     C                   endif
023000010601     C*
023100010604     C                   endif
023200010604     C*
023300010330     C                   else
023400010330     C                   eval      vinflg = '1'
023500010330     C                   endif
023600010601     C*
023700010601     C  N70              update    tivin000
023800010330     C*
023900010330     C  N70              ENDdo
024000010601     C
024100010601     C*
024200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024300010601     C                   if        cntNonEl = *zeros or
024400010601     C                             flgMulti = '0'
024500010330     C* Se non ci sono record con errori ...
024600010601     C                   if        §ctrno = 0 and
024700010604     C                             §ctrmo = 0 and
024800010601     C                             flgStato <> '2'
024900010330     C* ... restituisco esito OK.
025000010330     C                   eval      wrkesito = '0'
025100010330     C                   else
025200010330     C                   if        §ctrok > 0
025300010330     C                   eval      wrkesito = '1'
025400010330     C                   else
025500010615     C                   if        flgOk = '0'
025600010615     C                   eval      wrkesito = '2'
025700010615     C                   else
025800010615     C                   eval      wrkesito = '6'
025900010615     C                   endif
026000010330     C                   endif
026100010330     C                   endif
026200010601     C                   else
026300010601     C                   eval      wrkesito = '9'
026400010601     C                   endif
026500010330     C*
026600010330     C                   if        %open(tivin00r)
026700010330     C                   close     tivin00r
026800010330     C                   endif
026900020916     C                   if        %open(fivatwwr)
027000020916     C                   close     fivatwwr
027100010330     C                   endif
027200010601     C*
027300010601     C                   if        vlrpoi <> 999
027400010601     C                   eval      invfil = vlrpoi
027500010601     C                   endif
027600010330     C*
027700010330     C                   if        §ctrok > 0
027800010601     C                             and invfil > *zeros
027900010330     C                   exsr      invio
028000010330     C                   endif
028100010601     C*
028200010612     C                   if        flgGiro = '1'
028300010601     C                   exsr      endela
028400010612     C                   endif
028500010330     C*
028600010330     C                   ENDSR
028700010330     C***
028800010330
028900010601
029000010601
029100010601
029200010601
029300010330     C*----------------------------------------------------*
029400010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029500010330     C*----------------------------------------------------*
029600010330     C     INZVAR        BEGSR
029700010330     C*
029800010330     C*
029900010330     C                   ENDSR
030000010330     C*----------------------------------------------------*
030100010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030200010330     C*----------------------------------------------------*
030300010330     C     DEFCAM        BEGSR
030400010330     C*
030500040316     C* Inizializzo il buffer del record da scrivere
030600040316     C                   CLEAR                   FIVAT000
030700040316     C* Imposto i valori di default...
030800040316     C                   EVAL      VATCCM = 0892729
030900040316     C                   EVAL      VATLNP = 089
031000040316     C                   EVAL      VATTRC = 'E'
031100040316     C* ... e poi verifico se sono stati passati come parametri
031200040316     C                   IF        vlrppt > *blanks
031300040324     C                   IF        %trim(%subst(vlrppt:2:7)) <> *blanks
031400040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:2:7))
031500040316     C                   EXSR      CHKNUM
031600040316     C                   IF        PiInt=*on
031700040316     C                   Z-ADD     PiVal         VATCCM
031800040316     C                   ENDIF
031900040316     C                   ENDIF
032000040324     C                   IF        %trim(%subst(vlrppt:9:3)) <> *blanks
032100040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:9:3))
032200040316     C                   EXSR      CHKNUM
032300040316     C                   IF        PiInt=*on
032400040316     C                   Z-ADD     PiVal         VATLNP
032500040316     C                   ENDIF
032600040316     C                   ENDIF
032700040316     C                   ENDIF
032800010330     C*
032900010330     C                   ENDSR
033000040316     C*----------------------------------------------------*
033100040316
033200040316
033300040316
033400040316     C*----------------------------------------------------*
033500040316     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
033600040316     C*----------------------------------------------------*
033700040316     C     IMPVAT        BEGSR
033800040316     C*
033900040316     C                   SETOFF                                       3132
034000040316     C*
034100040316     C* Reperimento campi NUMERICI
034200040316     C                   EVAL      PiStr=%trim(%subst(vindta:15:4))
034300040316     C                   EXSR      CHKNUM
034400040316     C                   IF        PiInt=*on
034500040316     C                   Z-ADD     PiVal         VATAAS
034600040316     C                   ELSE
034700040316     C                   SETON                                        31
034800040316     C                   Z-ADD     *zeros        VATAAS
034900040316     C                   EVAL      vinmsg = %trimr(vinmsg)
035000040316     C                             + ' ' + 'VATAAS'
035100040316     C                   ENDIF
035200040316     C*
035300040316     C                   EVAL      PiStr=%trim(%subst(vindta:23:7))
035400040316     C                   EXSR      CHKNUM
035500040316     C                   IF        PiInt=*on
035600040316     C                   Z-ADD     PiVal         VATNSP
035700040316     C                   ELSE
035800040316     C                   SETON                                        31
035900040316     C                   Z-ADD     *zeros        VATNSP
036000040316     C                   EVAL      vinmsg = %trimr(vinmsg)
036100040316     C                             + ' ' + 'VATNSP'
036200040316     C                   ENDIF
036300040316     C*
036400040316     C* Reperimento campi ALFA
036500040316     C                   EVAL      VATNOT = %subst(vindta:31:35)
036600040316     C*
036700040316     C* Scarico il buffer del FIVAT
036800040316     C  N31              WRITE     FIVAT000
036900040316     C*
037000040316     C                   ENDSR
037100040316     C*----------------------------------------------------*
037200040316
037300040316
037400040316
037500040316     C*----------------------------------------------------*
037600040316     C*  CONTROLLO NUMERICITA' CAMPI
037700040316     C*----------------------------------------------------*
037800040316     C     CHKNUM        BEGSR
037900040316     C*
038000040316     C                   IF        PiDecChr = *blanks
038100040316     C                   EVAL      PiDecChr = ','
038200040316     C                   ENDIF
038300040316     C*
038400040316     C                   CALL(e)   'ISNUMERIC'
038500040316     C                   PARM                    PiStr            30
038600040316     C                   PARM                    PiDecChr          1
038700040316     C                   PARM      *ZEROS        PiVal            30 9
038800040316     C                   PARM      '0'           PiInt             1
038900040316     C                   PARM      '0'           PiNum             1
039000040316     C                   IF        %error
039100040316     C                   EVAL      PiNum=*off
039200040316     C                   ENDIF
039300040316     C*
039400040316     C                   ENDSR
039500040316     C***
039600990920
039700010601
039800010601
039900010601
040000010601
040100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
040200010601     C     repfil        BEGSR
040300010601     C*
040400010601     C                   if        invfil = *zeros and
040500010601     C                             depfil > *zeros and
040600010613     C                             (vinflg = *blanks or
040700010613     C                              vinflg = *zeros)
040800010601     C
040900010601     C                   eval      invfil = depfil
041000010601     C                   endif
041100010601     C*
041200010601     C                   if        depfil <> invfil and
041300010601     C                             invfil > *zeros
041400010601     C                   eval      flgMulti = '1'
041500010601     C                   if        vinflg = *blanks
041600010601     C                   add       1             cntNonEl
041700010601     C                   endif
041800010601     C                   endif
041900010601     C*
042000010601     C                   if        vinflg = '2'
042100010601     C                   eval      flgStato = '2'
042200010601     C                   endif
042300010601     C*
042400010601     C                   ENDSR
042500010601     C***
042600010601
042700010601
042800010601
042900010330
043000010330
043100010330
043200990920      /TITLE Invio dei dati al punto operativo.
043300000613     C     invio         BEGSR
043400990920     C*
043500990920     C                   reset                   dscmz
043600010601     C                   move      invfil        cmzdst
043700990920     C                   eval      cmzfld = vlrfou
043800990920     C                   eval      cmzmbd = vlrhdl
043900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
044000000710     C                   if        prmfir = *blanks
044100020916     C                   eval      cmzfla = 'FIVAT00F'
044200020916     C                   eval      cmzmba = 'FIVAT00F'
044300000710     C                   else
044400000710     C                   eval      cmzfla = prmfir
044500000710     C                   eval      cmzmba = prmfir
044600000710     C                   endif
044700990920     C                   eval      cmznrr = *zeros
044800990920     C                   move      §ctrok        cmznrr
044900021003     C                   eval      cmzlba = vlrfl1
045000990920     C                   call(e)   'TIS711C'
045100990920     C                   parm                    dscmz
045200990921     C                   parm      *blanks       esito
045300990920     C                   if        %error
045400990920     C                             or cmzerr = '1'
045500990921     C                             or esito  = '1'
045600000710     C                   eval      wrkesito = '3'
045700990920     C                   endif
045800990920     C*
045900000613     C                   ENDSR
046000990910
046100010601
046200010601
046300010601
046400010601
046500010601      /TITLE Invio dei dati al punto operativo.
046600010601     C     opeini        BEGSR
046700010601     C*
046800010601     C* Inizializzo flag e contatori operativi
046900010601     C                   movel     '0'           flgGiro           1
047000010601     C                   movel     '0'           flgMulti          1
047100010601     C                   movel     '1'           flgStato          1
047200010615     C                   movel     '0'           flgOk             1
047300010601     C                   z-add     *zeros        cntNonEl         10 0
047400010601     C                   z-add     *zeros        depfil            3 0
047500010601     C                   z-add     *zeros        invfil            3 0
047600030407     C*
047700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
047800030407     C                   if        %subst(vlrppt:1:1) = 'S'
047900030407     C                   seton                                        50
048000030407     C                   else
048100030407     C                   setoff                                       50
048200030407     C                   endif
048300010601     C*
048400010601     C                   ENDSR
048500010601     C***
048600010601
048700010601
048800010601
048900010330
049000010330
049100000613     C     *inzsr        BEGSR
049200990910     C*
049300990910     C     *entry        plist
049400990920     C                   parm                    tivlrds
049500990921     C                   parm      wrkesito      esito
049600000724     C                   parm                    prmlit
049700000710     C                   parm                    prmfir
049800010330     C*
049900010330     C* CALCOLA LA DATA CORRENTE
050000010330     C                   time                    wn14             14 0
050100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
050200010330     C                   z-add     wn8           g08dat
050300010330     C                   z-add     *zeros        g08inv
050400010330     C                   movel     '0'           g08err
050500010330     C                   call      'XSRDA8'
050600010330     C                   parm                    wlbda8
050700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
050800000613     C*
050900000613     C                   ENDSR
051000000613     C***
051100990908
051200040316     Otitva5p   E            riepilogo         2
051300990915     O                                              'Upload via Internet'
051400990915     O                                           +1 'Traduzione TIVIN00R -'
051500020916     O                                           +1 'FIVATWWR'
051600990915     O                                           +1 'Testate bolle'
051700990915     O          E            riepilogo   2
051800990915     O                       wrkdata
051900990915     O                       wrkora              +1
052000990915     O                       procname            +1
052100990915     O          E            riepilogo   2
052200990915     O                                              'Cliente..................:'
052300010612     O                       VATCCM        z     +1
052400990915     O          E            riepilogo   2
052500990920     O                                              'Riferimento Strategi.....:'
052600990920     O                       vlrhdl              +1
052700990915     O          E            riepilogo   2
052800990915     O                                              'Giusti...................:'
052900971022     O                       §CTROK        2   +  1
053000990915     O          E            riepilogo   2
053100990915     O                                              'Sbagliati e corretti.....:'
053200971022     O                       §CTRMO        2   +  1
053300990915     O          E            riepilogo   2
053400990915     O                                              'Sbagliati e scartati.....:'
053500971022     O                       §CTRNO        2   +  1
053600000613
053700040316     Otitva5ps  E            testdett          2
053800000613     O                                              'Upload via Internet'
053900000613     O                                           +1 'Traduzione TIVIN00R -'
054000020916     O                                           +1 'FIVATWWR'
054100000613     O                                           +1 'Testate bolle'
054200000616     O          E            testdett    3
054300000613     O                                           +2 'N° rec'
054400000613     O                                           +3 'Anteprima contenuto'
054500000616     O          E            rigadett    2
054600000613     O                       rrnum               +2
054700000621     O                       recko               +3
054800000616     O          E            findett     2
054900000613     O                       wrkdata
055000000613     O                       wrkora              +1
055100000613     O                       procname            +1
055200000616     O          E            findett     2
055300000613     O                                              'Cliente..................:'
055400010612     O                       VATCCM        z     +1
055500000616     O          E            findett     2
055600000613     O                                              'Riferimento Strategi.....:'
055700000613     O                       vlrhdl              +1
055800000616     O          E            findett     2
055900000613     O                                              'Giusti...................:'
056000000613     O                       §CTROK        2   +  1
056100000616     O          E            findett     2
056200000613     O                                              'Sbagliati e corretti.....:'
056300000613     O                       §CTRMO        2   +  1
056400000616     O          E            findett     2
056500000613     O                                              'Sbagliati e scartati.....:'
056600000613     O                       §CTRNO        2   +  1
056700000613** CMD - COMANDI CL
056800040316OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
056900040316OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
057000040316DLTOVR FILE(TITVA5P TITVA5PS) LVL(*)
057100010606
057200000613
