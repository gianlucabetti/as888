000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700010319     Ftis7bbbj  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010319     Ftis7bbbjs O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011115     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000801     D*----------------------------------------------------
001700010215     D* DICHIARAZIOINE VARIABILI DI WRK
001800000801     D*----------------------------------------------------
001900990920     D dscmz         e ds                  inz
002000990910     D psds           sds
002100990910     D  procname         *PROC
002200990920     D tivlrds       e ds                  extname(tivlr00f)
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700990915     D wrkdata         s               d
002800990915     D wrkora          s               t
002900000613     D rrnum           s              6  0 INZ(*zeros)
003000000621     D recko           s            150    INZ(*blanks)
003100011115     D depcmd          s            150    INZ(*blanks)
003200010319     D depColl         s              5  0 INZ(*zeros)
003300010319     D depPall         s              5  0 INZ(*zeros)
003400020201     D depCart         s              5  0 INZ(*zeros)
003500000801
003600010213
003700000801     D  Num5_0         s              5  0
003800000830
003900000830     D*------------------
004000000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004100000830     D*------------------
004200000830     D WLBDA8          DS                  INZ
004300000830     D  G08DAT                 1      8  0
004400000830     D  G08INV                 9     16  0
004500000830     D  G08ERR                17     17
004600000830     D  G08TGI                18     22  0
004700000830     D*
004800990908
004900010201
005000010201
005100990915     C                   time                    wrkdata
005200990915     C                   time                    wrkora
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600000724     C*
005700000724     C* SE OCCORRE SPEDIRE IN FILIALE
005800000724     C                   if        vlrpoi <> *zeros
005900000724     C*
006000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006200000724     C     vlrpoi        chain     azorg01l
006300000724     C                   if        %found
006400000616     C                   movel(p)  CMD(1)        depcmd
006500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006600000616     C*
006700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006800011115     C                   Z-ADD     150           LENGH            15 5
006900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007000000616     C                   PARM                    depcmd
007100000616     C                   PARM                    LENGH
007200000724     C*
007300000724     C                   endif
007400000724     C                   endif
007500000616     C*
007600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700000616     C                   movel(p)  CMD(2)        depcmd
007800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
007900000616     C*
008000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008100011115     C                   Z-ADD     150           LENGH            15 5
008200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000616     C                   PARM                    depcmd
008400000616     C                   PARM                    LENGH
008500000616     C*
008600010319     C                   if        not %open(tis7bbbjs)
008700010319     C                   open      tis7bbbjs
008800000616     C                   except    testdett
008900000613     C                   endif
009000000613     C*
009100010215     C                   EXSR      RWVAB                                        LETT/SCR. VAB
009200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009300010202     C*
009400010319     C                   if        %open(tis7bbbjs)
009500000616     C                   except    findett
009600010319     C                   close     tis7bbbjs
009700000613     C                   endif
009800000616     C*
009900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
010000011115     C                   Z-ADD     150           LENGH            15 5
010100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010200000616     C                   PARM                    CMD(3)
010300000616     C                   PARM                    LENGH
010400000616     C*
010500000801     C
010600010201     C                   seton                                        LR
010700000613
010800000613
010900000613     C*--------------------------------------------------------
011000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
011100000613     C*--------------------------------------------------------
011200000621     C     STPR          BEGSR
011300000613     C*
011400010319     C                   if        not %open(tis7bbbj)
011500010319     C                   open      tis7bbbj
011600990915     C                   endif
011700990915     C*
011800990915     C                   except    riepilogo
011900990915     C*
012000010319     C                   if        %open(tis7bbbj)
012100010319     C                   close     tis7bbbj
012200990914     C                   endif
012300990910     C*
012400000613     C                   ENDSR
012500000613     C***
012600990908
012700000801
012800910830     C*--------------------------------------------------------
012900021113     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWR              *
013000910830     C*--------------------------------------------------------
013100010215     C     RWVAB         BEGSR
013200990910     C*
013300990914     C                   if        not %open(tivin00r)
013400990908     C                   open      tivin00r
013500990914     C                   endif
013600021113     C                   if        not %open(fivabwwr)
013700021113     C                   open      fivabwwr
013800990914     C                   endif
013900990910     C*
014000010215     C                   clear                   §CTROK            5 0
014100000801     C                   clear                   §CTRMO            5 0
014200000801     C                   clear                   §CTRNO            5 0
014300990910     C*
014400921023     C                   DO        *HIVAL
014500990913     C*
014600990915     C                   READ      tivin00r                               70
014700000905     C                   if        vindta > *blanks
014800000613     C                   add       1             rrnum
014900000801     C*
015000000801     C                   if        *in70 = *off
015100000801     C                             and
015200000801     C                             (vinflg = *blanks
015300000801     C                              or vinflg = '0'
015400000801     C                              or vinflg = '2')
015500000801     C*
015600000801     C                   clear                   vinmsg
015700000801     C                   eval      vinflg = '1'
015800010201     C*
015900000801     C                   exsr      inzvar
016000000801     C                   exsr      defcam
016100010201     C                   exsr      impvab
016200021113     C                   write     fivab000
016300010201     C                   endif
016400000905     C*
016500000905     C                   else
016600000905     C                   eval      vinflg = '1'
016700000905     C                   endif
016800000905     C*
016900000905     C  N70              update    tivin000
017000000905     C*
017100991022     C  N70              ENDdo
017200010202     C*
017300990910
017400990910     C* Se non ci sono record con errori ...
017500000710     C                   if        §ctrno = 0
017600990910     C* ... restituisco esito OK.
017700990921     C                   eval      wrkesito = '0'
017800990910     C                   else
017900010215     C                   if        §ctrok   > 0
018000990921     C                   eval      wrkesito = '1'
018100000710     C                   else
018200000710     C                   eval      wrkesito = '2'
018300990910     C                   endif
018400000710     C                   endif
018500990910     C*
018600990914     C                   if        %open(tivin00r)
018700990908     C                   close     tivin00r
018800990914     C                   endif
018900021113     C                   if        %open(fivabwwr)
019000021113     C                   close     fivabwwr
019100990914     C                   endif
019200990910     C*
019300010215     C                   if        §ctrok > 0
019400000724     C                             and vlrpoi <> *zeros
019500010202     C                   exsr      invio
019600990920     C                   endif
019700990920     C*
019800910830     C                   ENDSR
019900000613     C***
020000990920
020100000801     C*----------------------------------------------------*
020200010215     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020300000801     C*----------------------------------------------------*
020400010201     C     INZVAR        BEGSR
020500000801     C*
020600010319     C                   Z-ADD     *zeros        depColl
020700010319     C                   Z-ADD     *zeros        depPall
020800020201     C                   Z-ADD     *zeros        depCart
020900000801     C*
021000000801     C                   ENDSR
021100000801     C*----------------------------------------------------*
021200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021300000801     C*----------------------------------------------------*
021400000801     C     DEFCAM        BEGSR
021500000801     C*
021600021113     C                   CLEAR                   FIVAB000
021700020201     C                   IF        %subst(vindta:464:1) = 'K'
021800020204     C                   Z-ADD     0431898       VABCCM
021900020201     C                   ELSE
022000020204     C                   Z-ADD     0431893       VABCCM
022100020201     C                   ENDIF
022200020201     C                   Z-ADD     043           VABLNP
022300020516     C                   Z-ADD     200           VABCTR
022400000801     C*
022500000801     C                   ENDSR
022600000801     C*----------------------------------------------------*
022700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
022800000801     C*----------------------------------------------------*
022900010201     C     IMPVAB        BEGSR
023000000801     C*
023100000801     C                   Z-ADD     *zeros        errore            1 0
023200000830     C                   MOVEL     datcor        VABAAS
023300000830     C                   MOVE      datcor        VABMGS
023301021113     C                   MOVE(P)   vlrpoi        VABFGS
023400010215     C*
023500010215     C* Reperimento campi ALFA
023600020201     C                   MOVEL     *blanks       WrkRSD           70
023700020201     C                   EVAL      WrkRSD=%trim(WrkRSD)+' '+
023800020201     C                                     %trim(%subst(vindta:205:30))
023900020201     C                   EVAL      WrkRSD=%trim(WrkRSD)+' '+
024000020201     C                                     %trim(%subst(vindta:235:30))
024100020201     C                   EVAL      WrkRSD=%trim(WrkRSD)+' '+
024200020201     C                                     %trim(%subst(vindta:265:30))
024300020201     C                   EVAL      WrkRSD=%trim(WrkRSD)+' '+
024400020201     C                                     %trim(%subst(vindta:295:30))
024500020201     C*
024600020201     C                   EVAL      VABRSD=%subst(WrkRSD:1:35)
024700020201     C                   EVAL      VABRD2=%subst(WrkRSD:36:35)
024800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025000020117     C* ==
025100020201     C                   EVAL      VABIND=%trim(%subst(vindta:398:30))
025200020201     C                   EVAL      VABLOD=%trim(%subst(vindta:368:30))
025300020201     C                   EVAL      VABRMA=%trim(%subst(vindta:175:10))
025400020201     C*
025500020201     C* Costruzione campi NOTE
025600020201     C                   MOVEL     *blanks       WrkNOT           70
025700020201     C                   EVAL      WrkNOT=%trim(WrkNOT)+' '+
025800020201     C                                     %trim(%subst(vindta:465:40))
025900020201     C                   EVAL      WrkNOT=%trim(WrkNOT)+' '+
026000020201     C                                     %trim(%subst(vindta:505:40))
026100020201     C                   EVAL      WrkNOT=%trim(WrkNOT)+' '+
026200020201     C                                     %trim(%subst(vindta:545:40))
026300020201     C                   EVAL      WrkNOT=%trim(WrkNOT)+' '+
026400020201     C                                     %trim(%subst(vindta:585:40))
026500020201     C*
026600020201     C                   EVAL      VABNOT=%subst(WrkNOT:1:35)
026700020201     C                   EVAL      VABNT2=%subst(WrkNOT:36:35)
026800010319     C*
026900020201     C                   EVAL      PiStr=%trim(%subst(vindta:175:10))
027000010319     C                   EXSR      CHKNUM
027100010319     C                   IF        PiInt=*on
027200020201     C                   Z-ADD     PiVal         VABRMN
027300010319     C                   ELSE
027400010319     C                   ADD       1             errore
027500010319     C                   EVAL      vinmsg = %trimr(vinmsg)
027600010319     C                             + ' ' + 'VABRMN'
027700010319     C                   ENDIF
027800010319     C*
027900020201     C                   EVAL      PiStr=%trim(%subst(vindta:355:13))
028000010319     C                   EXSR      CHKNUM
028100010319     C                   IF        PiInt=*on
028200010319     C                   Z-ADD     PiVal         Num5_0
028300010319     C                   MOVEL(p)  Num5_0        VABCAD
028400010319     C                   ELSE
028500010319     C                   ADD       1             errore
028600010319     C                   EVAL      vinmsg = %trimr(vinmsg)
028700010319     C                             + ' ' + 'VABCAD'
028800010319     C                   ENDIF
028900010319     C*
029000010215     C* Reperimento campi NUMERICI e CALCOLATI
029100020201     C                   EVAL      PiStr=%trim(%subst(vindta:448:5))
029200010201     C                   EXSR      CHKNUM
029300010201     C                   IF        PiInt=*on
029400010319     C                   Z-ADD     PiVal         depColl
029500010201     C                   ENDIF
029600010215     C*
029700020201     C                   EVAL      PiStr=%trim(%subst(vindta:453:5))
029800010201     C                   EXSR      CHKNUM
029900020201     C                   IF        PiInt=*on
030000020201     C                   Z-ADD     PiVal         depCart
030100010201     C                   ENDIF
030200020201     C*
030300020201     C                   EVAL      PiStr=%trim(%subst(vindta:458:5))
030400020201     C                   EXSR      CHKNUM
030500020201     C                   IF        PiInt=*on
030600020201     C                   Z-ADD     PiVal         depPall
030700020201     C                   ENDIF
030800010215     C*
030900020201     C                   EVAL      VABNCL = depColl + depPall + depCart
031000010319     C*
031100020201     C                   EVAL      PiStr=%trim(%subst(vindta:438:10))
031200010215     C                   EXSR      CHKNUM
031300010215     C                   IF        PiNum=*on
031400020201     C                   EVAL      PiVal = PiVal / 1000
031500010319     C                   Z-ADD     PiVal         VABPKB
031600010215     C                   ENDIF
031700010319     C*
031800020201     C                   EVAL      PiStr=%trim(%subst(vindta:428:10))
031900010201     C                   EXSR      CHKNUM
032000010405     C                   IF        PiNum=*on
032100020201     C                   EVAL      PiVal = PiVal / 1000000
032200010405     C                   Z-ADD(H)  PiVal         VABVLB
032300010201     C                   ENDIF
032400010205     C*
032500010215     C                   EVAL      VABCBO = '1'
032600010205     C*
032700010202     C*
032800000801     C* Ebbene...
032900000801     C                   ADD       1             §CTRMO
033000010201     C                   IF        errore <> *zeros
033100000801     C                   ADD       1             §CTRNO
033200000801     C                   EVAL      recko = vindta
033300000801     C                   EXCEPT    rigadett
033400000801     C                   EVAL      vinflg = '2'
033500000801     C                   ELSE
033600010215     C                   ADD       1             §CTROK
033700000801     C                   ENDIF
033800000801     C*
033900000801     C                   ENDSR
034000000801     C*----------------------------------------------------*
034100000801     C*  CONTROLLO NUMERICITA' CAMPI
034200000801     C*----------------------------------------------------*
034300000801     C     CHKNUM        BEGSR
034400000801     C*
034500000801     C                   call(e)   'ISNUMERIC'
034600000801     C                   PARM                    PiStr            30
034700000801     C                   PARM      ','           PiDecChr          1
034800000801     C                   PARM      *ZEROS        PiVal            30 9
034900000801     C                   PARM      '0'           PiInt             1
035000000801     C                   PARM      '0'           PiNum             1
035100000801     C                   IF        %error
035200000801     C                   EVAL      PiInt=*off
035300000801     C                   ENDIF
035400000801     C*
035500000801     C                   ENDSR
035600000801     C***
035700000801
035800000801
035900000801
036000990920      /TITLE Invio dei dati al punto operativo.
036100010202     C     invio         BEGSR
036200010201     C*
036300021113     C* Invio FIVAB
036400010201     C                   reset                   dscmz
036500010201     C                   move      vlrpoi        cmzdst
036600010201     C                   eval      cmzfld = vlrfou
036700010201     C                   eval      cmzmbd = vlrhdl
036800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
036900010201     C                   if        prmfir = *blanks
037000021113     C                   eval      cmzfla = 'FIVAB00F'
037100021113     C                   eval      cmzmba = 'FIVAB00F'
037200010201     C                   else
037300010201     C                   eval      cmzfla = prmfir
037400010201     C                   eval      cmzmba = prmfir
037500010201     C                   endif
037600010201     C                   eval      cmznrr = *zeros
037700010215     C                   move      §ctrok        cmznrr
037800021025     C                   eval      cmzlba = vlrfl1
037900010201     C                   call(e)   'TIS711C'
038000010201     C                   parm                    dscmz
038100010201     C                   parm      *blanks       esito
038200010201     C                   if        %error
038300010201     C                             or cmzerr = '1'
038400010201     C                             or esito  = '1'
038500010201     C                   eval      wrkesito = '3'
038600010205     C                   endif
038700990920     C*
038800000613     C                   ENDSR
038900000613     C***
039000990910
039100000613     C     *inzsr        BEGSR
039200990910     C*
039300990910     C     *entry        plist
039400990920     C                   parm                    tivlrds
039500990921     C                   parm      wrkesito      esito
039600000724     C                   parm                    prmlit
039700000710     C                   parm                    prmfir
039800000613     C*
039900000830     C* CALCOLA LA DATA CORRENTE
040000000830     C                   time                    wn14             14 0
040100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
040200000830     C                   z-add     wn8           g08dat
040300000830     C                   z-add     *zeros        g08inv
040400000830     C                   movel     '0'           g08err
040500000830     C                   call      'XSRDA8'
040600000830     C                   parm                    wlbda8
040700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
040800000830     C*
040900000613     C                   ENDSR
041000000613     C***
041100990908
041200010319     Otis7bbbj  E            riepilogo         2
041300990915     O                                              'Upload via Internet'
041400990915     O                                           +1 'Traduzione TIVIN00R -'
041500021113     O                                           +1 'FIVABWWR'
041600010201     O                                           +1 'Report testate bolle'
041700990915     O          E            riepilogo   2
041800990915     O                       wrkdata
041900990915     O                       wrkora              +1
042000990915     O                       procname            +1
042100990915     O          E            riepilogo   2
042200990915     O                                              'Cliente..................:'
042300990915     O                       VABCCM        z     +1
042400990915     O          E            riepilogo   2
042500990920     O                                              'Riferimento Strategi.....:'
042600990920     O                       vlrhdl              +1
042700990915     O          E            riepilogo   2
042800990915     O                                              'Giusti...................:'
042900010215     O                       §CTROK        2   +  1
043000990915     O          E            riepilogo   2
043100990915     O                                              'Sbagliati e corretti.....:'
043200971022     O                       §CTRMO        2   +  1
043300990915     O          E            riepilogo   2
043400990915     O                                              'Sbagliati e scartati.....:'
043500971022     O                       §CTRNO        2   +  1
043600000613
043700010319     Otis7bbbjs E            testdett          2
043800000613     O                                              'Upload via Internet'
043900000613     O                                           +1 'Traduzione TIVIN00R -'
044000021113     O                                           +1 'FIVABWWR'
044100010201     O                                           +1 'Report testate bolle'
044200000616     O          E            testdett    3
044300000613     O                                           +2 'N° rec'
044400000613     O                                           +3 'Anteprima contenuto'
044500000616     O          E            rigadett    2
044600000613     O                       rrnum               +2
044700000621     O                       recko               +3
044800000616     O          E            findett     2
044900000613     O                       wrkdata
045000000613     O                       wrkora              +1
045100000613     O                       procname            +1
045200000616     O          E            findett     2
045300000613     O                                              'Cliente..................:'
045400000613     O                       VABCCM        z     +1
045500000616     O          E            findett     2
045600000613     O                                              'Riferimento Strategi.....:'
045700000613     O                       vlrhdl              +1
045800000616     O          E            findett     2
045900000613     O                                              'Giusti...................:'
046000010215     O                       §CTROK        2   +  1
046100000616     O          E            findett     2
046200000613     O                                              'Sbagliati e corretti.....:'
046300000613     O                       §CTRMO        2   +  1
046400000616     O          E            findett     2
046500000613     O                                              'Sbagliati e scartati.....:'
046600000613     O                       §CTRNO        2   +  1
046700000512** CMD - COMANDI CL
046800011115OVRPRTF FILE(TIS7BBBJ) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
046900011115OVRPRTF FILE(TIS7BBBJS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
047000010319DLTOVR FILE(TIS7BBBJ TIS7BBBJS) LVL(*)
047100000512
047200000512
