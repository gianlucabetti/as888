000100010720      /TITLE Upload via Internet: traduzione in FNVABWWR. (x EMMEGI - Cod. 0010051)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600010605     FFNVABwwr  O    E             DISK    usropn
000700010720     Ftis7mgp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010720     Ftis7mgps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500000613     D cmd             S             80    DIM(5) CTDATA PERRCD(1)
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
003100010330     D depcmd          s             80    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000000313     D*
004100010330
004200010330
004300010330
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010605     C                   exsr      rwvab
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010601
005700010601
005800010601     C*--------------------------------------------------------
005900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006000010601     C*--------------------------------------------------------
006100010601     C     PREELA        BEGSR
006200010601     C*
006300010601     C* SE OCCORRE SPEDIRE IN FILIALE
006400010601     C                   if        invfil <> *zeros and
006500010601     C                             flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200010601     C     invfil        chain     azorg01l
007300000724     C                   if        %found
007400000724     C                   movel(p)  CMD(1)        depcmd
007500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800000724     C                   Z-ADD     80            LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000000616     C                   Z-ADD     80            LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500010720     C                   if        not %open(tis7mgps)
009600010720     C                   open      tis7mgps
009700000616     C                   except    testdett
009800000613     C                   endif
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011300010601     C*--------------------------------------------------------
011400010601     C     ENDELA        BEGSR
011500990915     C*
011600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011700000613     C*
011800010720     C                   if        %open(tis7mgps)
011900000616     C                   except    findett
012000010720     C                   close     tis7mgps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400000616     C                   Z-ADD     80            LENGH            15 5
012500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    CMD(3)
012700000616     C                   PARM                    LENGH
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500000613
013600000613     C*--------------------------------------------------------
013700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013800000613     C*--------------------------------------------------------
013900000621     C     STPR          BEGSR
014000000613     C*
014100010720     C                   if        not %open(tis7mgp)
014200010720     C                   open      tis7mgp
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700010720     C                   if        %open(tis7mgp)
014800010720     C                   close     tis7mgp
014900990914     C                   endif
015000000613     C*
015100000613     C                   ENDSR
015200000613     C***
015300000613
015400010330
015500010601
015600010601
015700010601
015800010330     C*--------------------------------------------------------
015900010605     C* RWVAB   LEGGE TIVIN00R E SCRIVE FNVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600010605     C                   if        not %open(fnvabwwr)
016700010605     C                   open      fnvabwwr
016800010330     C                   endif
016900010330     C*
017000010604     C                   clear                   §CTROK
017100010604     C                   clear                   §CTRMO
017200010604     C                   clear                   §CTRNO
017300010330     C*
017400010330     C                   DO        *HIVAL
017500010330     C*
017600010330     C                   READ      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200010618     C*
018300010330     C                   if        vindta > *blanks
018400010330     C                   add       1             rrnum
018500010330     C*
018600010601     C                   if        *in70 = *off and
018700010330     C                             (vinflg = *blanks
018800010330     C                              or vinflg = '0'
018900010330     C                              or vinflg = '2')
019000010330     C*
019100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019200010711     C                   if        vinflg = *blanks or vinflg = '0'
019300010711     C                   clear                   vinmsg
019400010711     C                   endif
019500010601     C*
019600010330     C                   exsr      inzvar
019700010330     C                   exsr      defcam
019800010605     C                   exsr      impvab
019900010601     C*
020000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020100010605     C                   eval      depfil = VABLNP
020200010601     C                   exsr      repfil
020300010601     C                   if        depfil = invfil
020400010601     C*
020500010601     C                   exsr      PREELA
020600010601     C*
020700010604     C* Ebbene...
020800010604     C*
020900010604     C  N31              ADD       1             §CTROK            7 0
021000010604     C   32              ADD       1             §CTRMO            7 0
021100010604     C   31              ADD       1             §CTRNO            7 0
021200010605     C  N31              WRITE     FNVAB000
021300010604     C*
021400010604     C                   if        *in31 = *off and
021500010604     C                             *in32 = *off
021600010604     C                   eval      vinflg = '1'
021700010604     C                   else
021800010604     C                   eval      recko = vindta
021900010604     C                   except    rigadett
022000010604     C                   eval      vinflg = '2'
022100010604     C                   endif
022200010604     C                   endif
022300010601     C*
022400010604     C                   endif
022500010604     C*
022600010330     C                   else
022700010330     C                   eval      vinflg = '1'
022800010330     C                   endif
022900010601     C*
023000010601     C  N70              update    tivin000
023100010330     C*
023200010330     C  N70              ENDdo
023300010601     C
023400010601     C*
023500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023600010601     C                   if        cntNonEl = *zeros or
023700010601     C                             flgMulti = '0'
023800010330     C* Se non ci sono record con errori ...
023900010601     C                   if        §ctrno = 0 and
024000010604     C                             §ctrmo = 0 and
024100010601     C                             flgStato <> '2'
024200010330     C* ... restituisco esito OK.
024300010330     C                   eval      wrkesito = '0'
024400010330     C                   else
024500010330     C                   if        §ctrok > 0
024600010330     C                   eval      wrkesito = '1'
024700010330     C                   else
024800010615     C                   if        flgOk = '0'
024900010615     C                   eval      wrkesito = '2'
025000010615     C                   else
025100010615     C                   eval      wrkesito = '6'
025200010615     C                   endif
025300010330     C                   endif
025400010330     C                   endif
025500010601     C                   else
025600010601     C                   eval      wrkesito = '9'
025700010601     C                   endif
025800010330     C*
025900010330     C                   if        %open(tivin00r)
026000010330     C                   close     tivin00r
026100010330     C                   endif
026200010605     C                   if        %open(fnvabwwr)
026300010605     C                   close     fnvabwwr
026400010330     C                   endif
026500010601     C*
026600010601     C                   if        vlrpoi <> 999
026700010601     C                   eval      invfil = vlrpoi
026800010601     C                   endif
026900010330     C*
027000010330     C                   if        §ctrok > 0
027100010601     C                             and invfil > *zeros
027200010330     C                   exsr      invio
027300010330     C                   endif
027400010601     C*
027500010618     C                   if        flgGiro = '1'
027600010618     C                   exsr      endela
027700010618     C                   endif
027800010330     C*
027900010330     C                   ENDSR
028000010330     C***
028100010330
028200010601
028300010601
028400010601
028500010601
028600010330     C*----------------------------------------------------*
028700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028800010330     C*----------------------------------------------------*
028900010330     C     INZVAR        BEGSR
029000010330     C*
029100010330     C*
029200010330     C                   ENDSR
029300010330     C*----------------------------------------------------*
029400010330     C*  IMPOSTAZIONE CAMPI COSTANTI
029500010330     C*----------------------------------------------------*
029600010330     C     DEFCAM        BEGSR
029700010330     C*
029800010605     C                   CLEAR                   FNVAB000
029900010720     C                   EVAL      VABCCM = 0010051
030000010720     C                   EVAL      VABLNP = 001
030100010605     C                   EVAL      VABCTR = 000
030200010330     C*
030300010330     C                   ENDSR
030400010330     C*----------------------------------------------------*
030500010605     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAB)
030600010330     C*----------------------------------------------------*
030700010605     C     IMPVAB        BEGSR
030800010330     C*
030900010604     C                   SETOFF                                       3132
031000010605     C                   MOVEL     datcor        VABAAS
031100010607     C                   MOVE      datcor        VABMGS
031200010330     C*
031300010330     C* Reperimento campi ALFA
031400010720     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
031500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031700020117     C* ==
031800010720     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
031900010720     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
032000010720     C                   EVAL      VABCAD=%trim(%subst(vindta:101:5))
032100010720     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
032200010807     C                   EVAL      VABRMA=%trim(%subst(vindta:109:15))
032300010330     C*
032400010720     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
032500010330     C                   EXSR      CHKNUM
032600010330     C                   IF        PiInt=*on
032700010605     C                   Z-ADD     PiVal         VABNCL
032800010330     C                   ELSE
032900010605     C                   SETON                                        32
033000010605     C                   Z-ADD     *zeros        VABNCL
033100010330     C                   EVAL      vinmsg = %trimr(vinmsg)
033200010605     C                             + ' ' + 'VABNCL'
033300010330     C                   ENDIF
033400010605     C*
033500010720     C                   EVAL      PiStr=%trim(%subst(vindta:129:8))
033600010605     C                   EXSR      CHKNUM
033700010605     C                   IF        PiNum=*on
033800010606     C                   Z-ADD(H)  PiVal         VABPKB
033900010605     C                   ELSE
034000010605     C                   SETON                                        32
034100010605     C                   Z-ADD     *zeros        VABPKB
034200010605     C                   EVAL      vinmsg = %trimr(vinmsg)
034300010605     C                             + ' ' + 'VABPKB'
034400010605     C                   ENDIF
034500010605     C*
034600010807     C                   EVAL      PiStr=%trim(%subst(vindta:109:15))
034700010605     C                   EXSR      CHKNUM
034800010605     C                   IF        PiInt=*on
034900010606     C                   Z-ADD     PiVal         VABRMN
035000010605     C                   ELSE
035100010605     C                   SETON                                        32
035200010605     C                   Z-ADD     *zeros        VABRMN
035300010605     C                   EVAL      vinmsg = %trimr(vinmsg)
035400010720     C                             + ' ' + 'VABRMN'
035500010605     C                   ENDIF
035600010605     C*
035700010807     C                   EVAL      PiStr=%trim(%subst(vindta:137:16))
035800010605     C                   EXSR      CHKNUM
035900010605     C                   IF        PiNum=*on
036000010605     C                   Z-ADD     PiVal         VABCAS
036100010605     C                   ELSE
036200010605     C                   SETON                                        32
036300010605     C                   Z-ADD     *zeros        VABCAS
036400010605     C                   EVAL      vinmsg = %trimr(vinmsg)
036500010605     C                             + ' ' + 'VABCAS'
036600010605     C                   ENDIF
036700010330     C*
036800010605     C* Considerazioni finali sui valori sopra ricavati
036900010720     C                   IF        (VABCAS = *zeros) AND
037000010720     C                             (%subst(vindta:207:1) = '1')
037100010720     C                   EVAL      VABCBO = '1 '
037200010720     C                   ENDIF
037300010720     C                   IF        (VABCAS = *zeros) AND
037400010720     C                             (%subst(vindta:207:1) = '2')
037500010720     C                   EVAL      VABCBO = '2 '
037600010720     C                   ENDIF
037700010720     C                   IF        (VABCAS > *zeros) AND
037800010720     C                             (%subst(vindta:207:1) = '1')
037900010720     C                   EVAL      VABCBO = '4 '
038000010720     C                   ENDIF
038100010720     C                   IF        (VABCAS > *zeros) AND
038200010720     C                             (%subst(vindta:207:1) = '2')
038300010720     C                   EVAL      VABCBO = '6 '
038400010720     C                   ENDIF
038500010330     C*
038600010330     C                   ENDSR
038700990920
038800010601
038900010601
039000010330
039100010330
039200010330     C*----------------------------------------------------*
039300010330     C*  CONTROLLO NUMERICITA' CAMPI
039400010330     C*----------------------------------------------------*
039500010330     C     CHKNUM        BEGSR
039600010330     C*
039700010606     C                   IF        PiDecChr = *blanks
039800010606     C                   EVAL      PiDecChr = ','
039900010606     C                   ENDIF
040000010606     C*
040100010606     C                   CALL(e)   'ISNUMERIC'
040200010330     C                   PARM                    PiStr            30
040300010606     C                   PARM                    PiDecChr          1
040400010330     C                   PARM      *ZEROS        PiVal            30 9
040500010330     C                   PARM      '0'           PiInt             1
040600010330     C                   PARM      '0'           PiNum             1
040700010330     C                   IF        %error
040800010606     C                   EVAL      PiNum=*off
040900010330     C                   ENDIF
041000010330     C*
041100010330     C                   ENDSR
041200010330     C***
041300010330
041400010601
041500010601
041600010601
041700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
041800010601     C     repfil        BEGSR
041900010601     C*
042000010601     C                   if        invfil = *zeros and
042100010601     C                             depfil > *zeros and
042200010628     C                             (vinflg = *blanks or
042300010628     C                              vinflg = *zeros)
042400010601     C
042500010601     C                   eval      invfil = depfil
042600010601     C                   endif
042700010601     C*
042800010601     C                   if        depfil <> invfil and
042900010601     C                             invfil > *zeros
043000010601     C                   eval      flgMulti = '1'
043100010601     C                   if        vinflg = *blanks
043200010601     C                   add       1             cntNonEl
043300010601     C                   endif
043400010601     C                   endif
043500010601     C*
043600010601     C                   if        vinflg = '2'
043700010601     C                   eval      flgStato = '2'
043800010601     C                   endif
043900010601     C*
044000010601     C                   ENDSR
044100010601     C***
044200010601
044300010601
044400010601
044500010330
044600010330
044700010330
044800990920      /TITLE Invio dei dati al punto operativo.
044900000613     C     invio         BEGSR
045000990920     C*
045100990920     C                   reset                   dscmz
045200010601     C                   move      invfil        cmzdst
045300990920     C                   eval      cmzfld = vlrfou
045400990920     C                   eval      cmzmbd = vlrhdl
045500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
045600000710     C                   if        prmfir = *blanks
045700010605     C                   eval      cmzfla = 'FNVAB00F'
045800010605     C                   eval      cmzmba = 'FNVAB00F'
045900000710     C                   else
046000000710     C                   eval      cmzfla = prmfir
046100000710     C                   eval      cmzmba = prmfir
046200000710     C                   endif
046300990920     C                   eval      cmznrr = *zeros
046400990920     C                   move      §ctrok        cmznrr
046401021111     C                   eval      cmzlba = vlrfl1
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
049100010601     C*
049200010601     C                   ENDSR
049300010601     C***
049400010601
049500010601
049600010601
049700010330
049800010330
049900000613     C     *inzsr        BEGSR
050000990910     C*
050100990910     C     *entry        plist
050200990920     C                   parm                    tivlrds
050300990921     C                   parm      wrkesito      esito
050400000724     C                   parm                    prmlit
050500000710     C                   parm                    prmfir
050600010330     C*
050700010330     C* CALCOLA LA DATA CORRENTE
050800010330     C                   time                    wn14             14 0
050900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
051000010330     C                   z-add     wn8           g08dat
051100010330     C                   z-add     *zeros        g08inv
051200010330     C                   movel     '0'           g08err
051300010330     C                   call      'XSRDA8'
051400010330     C                   parm                    wlbda8
051500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
051600000613     C*
051700000613     C                   ENDSR
051800000613     C***
051900990908
052000010720     Otis7mgp   E            riepilogo         2
052100990915     O                                              'Upload via Internet'
052200990915     O                                           +1 'Traduzione TIVIN00R -'
052300010605     O                                           +1 'FNVABWWR'
052400990915     O                                           +1 'Testate bolle'
052500990915     O          E            riepilogo   2
052600990915     O                       wrkdata
052700990915     O                       wrkora              +1
052800990915     O                       procname            +1
052900990915     O          E            riepilogo   2
053000990915     O                                              'Cliente..................:'
053100010605     O                       VABCCM        z     +1
053200990915     O          E            riepilogo   2
053300990920     O                                              'Riferimento Strategi.....:'
053400990920     O                       vlrhdl              +1
053500990915     O          E            riepilogo   2
053600990915     O                                              'Giusti...................:'
053700971022     O                       §CTROK        2   +  1
053800990915     O          E            riepilogo   2
053900990915     O                                              'Sbagliati e corretti.....:'
054000971022     O                       §CTRMO        2   +  1
054100990915     O          E            riepilogo   2
054200990915     O                                              'Sbagliati e scartati.....:'
054300971022     O                       §CTRNO        2   +  1
054400000613
054500010720     Otis7mgps  E            testdett          2
054600000613     O                                              'Upload via Internet'
054700000613     O                                           +1 'Traduzione TIVIN00R -'
054800010605     O                                           +1 'FNVABWWR'
054900000613     O                                           +1 'Testate bolle'
055000000616     O          E            testdett    3
055100000613     O                                           +2 'N° rec'
055200000613     O                                           +3 'Anteprima contenuto'
055300000616     O          E            rigadett    2
055400000613     O                       rrnum               +2
055500000621     O                       recko               +3
055600000616     O          E            findett     2
055700000613     O                       wrkdata
055800000613     O                       wrkora              +1
055900000613     O                       procname            +1
056000000616     O          E            findett     2
056100000613     O                                              'Cliente..................:'
056200010605     O                       VABCCM        z     +1
056300000616     O          E            findett     2
056400000613     O                                              'Riferimento Strategi.....:'
056500000613     O                       vlrhdl              +1
056600000616     O          E            findett     2
056700000613     O                                              'Giusti...................:'
056800000613     O                       §CTROK        2   +  1
056900000616     O          E            findett     2
057000000613     O                                              'Sbagliati e corretti.....:'
057100000613     O                       §CTRMO        2   +  1
057200000616     O          E            findett     2
057300000613     O                                              'Sbagliati e scartati.....:'
057400000613     O                       §CTRNO        2   +  1
057500000613** CMD - COMANDI CL
057600010720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7MGP)  TOFILE(TIS7T1P)  OUTQ(
057700010720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7MGPS) TOFILE(TIS7T1PS) OUTQ(
057800010720DLTOVR FILE(TIS7MGP TIS7MGPS) LVL(*)
057900010606
058000000613
