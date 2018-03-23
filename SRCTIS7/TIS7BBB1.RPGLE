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
003400000801
003500010213
003600000801     D  Num5_0         s              5  0
003700000830
003800000830     D*------------------
003900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004000000830     D*------------------
004100000830     D WLBDA8          DS                  INZ
004200000830     D  G08DAT                 1      8  0
004300000830     D  G08INV                 9     16  0
004400000830     D  G08ERR                17     17
004500000830     D  G08TGI                18     22  0
004600000830     D*
004700990908
004800010201
004900010201
005000990915     C                   time                    wrkdata
005100990915     C                   time                    wrkora
005200000913     C                   reset                   rrnum
005300990921     C                   reset                   esito
005400990921     C                   reset                   wrkesito
005500000724     C*
005600000724     C* SE OCCORRE SPEDIRE IN FILIALE
005700000724     C                   if        vlrpoi <> *zeros
005800000724     C*
005900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006100000724     C     vlrpoi        chain     azorg01l
006200000724     C                   if        %found
006300000616     C                   movel(p)  CMD(1)        depcmd
006400020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006500000616     C*
006600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006700011115     C                   Z-ADD     150           LENGH            15 5
006800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
006900000616     C                   PARM                    depcmd
007000000616     C                   PARM                    LENGH
007100000724     C*
007200000724     C                   endif
007300020829     C                   endif
007400000616     C*
007500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007600000616     C                   movel(p)  CMD(2)        depcmd
007700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
007800000616     C*
007900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008000011115     C                   Z-ADD     150           LENGH            15 5
008100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000616     C                   PARM                    depcmd
008300000616     C                   PARM                    LENGH
008400000616     C*
008500010319     C                   if        not %open(tis7bbbjs)
008600010319     C                   open      tis7bbbjs
008700000616     C                   except    testdett
008800000613     C                   endif
008900000613     C*
009000010215     C                   EXSR      RWVAB                                        LETT/SCR. VAB
009100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009200010202     C*
009300010319     C                   if        %open(tis7bbbjs)
009400000616     C                   except    findett
009500010319     C                   close     tis7bbbjs
009600000613     C                   endif
009700000616     C*
009800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
009900011115     C                   Z-ADD     150           LENGH            15 5
010000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010100000616     C                   PARM                    CMD(3)
010200000616     C                   PARM                    LENGH
010300000616     C*
010400000801     C
010500010201     C                   seton                                        LR
010600000613
010700000613
010800000613     C*--------------------------------------------------------
010900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
011000000613     C*--------------------------------------------------------
011100000621     C     STPR          BEGSR
011200000613     C*
011300010319     C                   if        not %open(tis7bbbj)
011400010319     C                   open      tis7bbbj
011500990915     C                   endif
011600990915     C*
011700990915     C                   except    riepilogo
011800990915     C*
011900010319     C                   if        %open(tis7bbbj)
012000010319     C                   close     tis7bbbj
012100990914     C                   endif
012200990910     C*
012300000613     C                   ENDSR
012400000613     C***
012500990908
012600000801
012700910830     C*--------------------------------------------------------
012800021113     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWR              *
012900910830     C*--------------------------------------------------------
013000010215     C     RWVAB         BEGSR
013100990910     C*
013200990914     C                   if        not %open(tivin00r)
013300990908     C                   open      tivin00r
013400990914     C                   endif
013500021113     C                   if        not %open(fivabwwr)
013600021113     C                   open      fivabwwr
013700990914     C                   endif
013800990910     C*
013900010215     C                   clear                   §CTROK            5 0
014000000801     C                   clear                   §CTRMO            5 0
014100000801     C                   clear                   §CTRNO            5 0
014200990910     C*
014300921023     C                   DO        *HIVAL
014400990913     C*
014500990915     C                   READ      tivin00r                               70
014600000905     C                   if        vindta > *blanks
014700000613     C                   add       1             rrnum
014800000801     C*
014900000801     C                   if        *in70 = *off
015000000801     C                             and
015100000801     C                             (vinflg = *blanks
015200000801     C                              or vinflg = '0'
015300000801     C                              or vinflg = '2')
015400000801     C*
015500000801     C                   clear                   vinmsg
015600000801     C                   eval      vinflg = '1'
015700010201     C*
015800000801     C                   exsr      inzvar
015900000801     C                   exsr      defcam
016000010201     C                   exsr      impvab
016100021113     C                   write     fivab000
016200010201     C                   endif
016300000905     C*
016400000905     C                   else
016500000905     C                   eval      vinflg = '1'
016600000905     C                   endif
016700000905     C*
016800000905     C  N70              update    tivin000
016900000905     C*
017000991022     C  N70              ENDdo
017100010202     C*
017200990910
017300990910     C* Se non ci sono record con errori ...
017400000710     C                   if        §ctrno = 0
017500990910     C* ... restituisco esito OK.
017600990921     C                   eval      wrkesito = '0'
017700990910     C                   else
017800010215     C                   if        §ctrok   > 0
017900990921     C                   eval      wrkesito = '1'
018000000710     C                   else
018100000710     C                   eval      wrkesito = '2'
018200990910     C                   endif
018300000710     C                   endif
018400990910     C*
018500990914     C                   if        %open(tivin00r)
018600990908     C                   close     tivin00r
018700990914     C                   endif
018800021113     C                   if        %open(fivabwwr)
018900021113     C                   close     fivabwwr
019000990914     C                   endif
019100990910     C*
019200010215     C                   if        §ctrok > 0
019300000724     C                             and vlrpoi <> *zeros
019400010202     C                   exsr      invio
019500990920     C                   endif
019600990920     C*
019700910830     C                   ENDSR
019800000613     C***
019900990920
020000000801     C*----------------------------------------------------*
020100010215     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020200000801     C*----------------------------------------------------*
020300010201     C     INZVAR        BEGSR
020400000801     C*
020500010319     C                   Z-ADD     *zeros        depColl
020600010319     C                   Z-ADD     *zeros        depPall
020700000801     C*
020800000801     C                   ENDSR
020900000801     C*----------------------------------------------------*
021000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021100000801     C*----------------------------------------------------*
021200000801     C     DEFCAM        BEGSR
021300000801     C*
021400021113     C                   CLEAR                   FIVAB000
021500010319     C                   Z-ADD     1070067       VABCCM
021600010319     C                   Z-ADD     107           VABLNP
021700020516     C                   Z-ADD     200           VABCTR
021800000801     C*
021900000801     C                   ENDSR
022000000801     C*----------------------------------------------------*
022100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
022200000801     C*----------------------------------------------------*
022300010201     C     IMPVAB        BEGSR
022400000801     C*
022500000801     C                   Z-ADD     *zeros        errore            1 0
022600000830     C                   MOVEL     datcor        VABAAS
022700000830     C                   MOVE      datcor        VABMGS
022701021113     C                   MOVE(P)   vlrpoi        VABFGS
022800010215     C*
022900010215     C* Reperimento campi ALFA
023000010319     C                   EVAL      VABRSD=%trim(%subst(vindta:18:35))
023100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
023200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
023300020117     C* ==
023400010319     C                   EVAL      VABRD2=%trim(%subst(vindta:53:25))
023500010319     C                   EVAL      VABIND=%trim(%subst(vindta:78:30))
023600010319     C                   EVAL      VABRMA=%trim(%subst(vindta:146:8))
023700010319     C*
023800010319     C                   EVAL      PiStr=%trim(%subst(vindta:146:8))
023900010319     C                   EXSR      CHKNUM
024000010319     C                   IF        PiInt=*on
024100010319     C                   Z-ADD     PiVal         VABRMN
024200010319     C                   ELSE
024300010319     C                   ADD       1             errore
024400010319     C                   EVAL      vinmsg = %trimr(vinmsg)
024500010319     C                             + ' ' + 'VABRMN'
024600010319     C                   ENDIF
024700010319     C*
024800010319     C                   EVAL      PiStr=%trim(%subst(vindta:108:5))
024900010319     C                   EXSR      CHKNUM
025000010319     C                   IF        PiInt=*on
025100010319     C                   Z-ADD     PiVal         Num5_0
025200010319     C                   MOVEL(p)  Num5_0        VABCAD
025300010319     C                   EVAL      VABLOD=%trim(%subst(vindta:114:32))
025400010319     C                   ELSE
025500010319     C                   ADD       1             errore
025600010319     C                   EVAL      vinmsg = %trimr(vinmsg)
025700010319     C                             + ' ' + 'VABCAD'
025800010319     C                   EVAL      VABLOD=%trim(%subst(vindta:108:38))
025900010319     C                   ENDIF
026000010319     C*
026100010215     C* Reperimento campi NUMERICI e CALCOLATI
026200010319     C                   EVAL      PiStr=%trim(%subst(vindta:154:7))
026300010201     C                   EXSR      CHKNUM
026400010201     C                   IF        PiInt=*on
026500010319     C                   Z-ADD     PiVal         depColl
026600010201     C                   ENDIF
026700010215     C*
026800010319     C                   EVAL      PiStr=%trim(%subst(vindta:161:7))
026900010201     C                   EXSR      CHKNUM
027000010201     C                   IF        PiNum=*on
027100010319     C                   Z-ADD     PiVal         depPall
027200010201     C                   ENDIF
027300010215     C*
027400010319     C                   EVAL      VABNCL = depColl + depPall
027500010319     C*
027600010319     C                   EVAL      PiStr=%trim(%subst(vindta:168:16))
027700010215     C                   EXSR      CHKNUM
027800010215     C                   IF        PiNum=*on
027900010319     C                   Z-ADD     PiVal         VABPKB
028000010215     C                   ENDIF
028100010319     C*
028200010319     C                   EVAL      PiStr=%trim(%subst(vindta:184:18))
028300010201     C                   EXSR      CHKNUM
028400010405     C                   IF        PiNum=*on
028500010405     C                   EVAL      PiVal = PiVal / 1000
028600010405     C                   Z-ADD(H)  PiVal         VABVLB
028700010201     C                   ENDIF
028800010205     C*
028900010215     C                   EVAL      VABCBO = '1'
029000010205     C*
029100010202     C*
029200000801     C* Ebbene...
029300000801     C                   ADD       1             §CTRMO
029400010201     C                   IF        errore <> *zeros
029500000801     C                   ADD       1             §CTRNO
029600000801     C                   EVAL      recko = vindta
029700000801     C                   EXCEPT    rigadett
029800000801     C                   EVAL      vinflg = '2'
029900000801     C                   ELSE
030000010215     C                   ADD       1             §CTROK
030100000801     C                   ENDIF
030200000801     C*
030300000801     C                   ENDSR
030400000801     C*----------------------------------------------------*
030500000801     C*  CONTROLLO NUMERICITA' CAMPI
030600000801     C*----------------------------------------------------*
030700000801     C     CHKNUM        BEGSR
030800000801     C*
030900000801     C                   call(e)   'ISNUMERIC'
031000000801     C                   PARM                    PiStr            30
031100000801     C                   PARM      ','           PiDecChr          1
031200000801     C                   PARM      *ZEROS        PiVal            30 9
031300000801     C                   PARM      '0'           PiInt             1
031400000801     C                   PARM      '0'           PiNum             1
031500000801     C                   IF        %error
031600000801     C                   EVAL      PiInt=*off
031700000801     C                   ENDIF
031800000801     C*
031900000801     C                   ENDSR
032000000801     C***
032100000801
032200000801
032300000801
032400990920      /TITLE Invio dei dati al punto operativo.
032500010202     C     invio         BEGSR
032600010201     C*
032700021113     C* Invio FIVAB
032800010201     C                   reset                   dscmz
032900010201     C                   move      vlrpoi        cmzdst
033000010201     C                   eval      cmzfld = vlrfou
033100010201     C                   eval      cmzmbd = vlrhdl
033200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
033300010201     C                   if        prmfir = *blanks
033400021113     C                   eval      cmzfla = 'FIVAB00F'
033500021113     C                   eval      cmzmba = 'FIVAB00F'
033600010201     C                   else
033700010201     C                   eval      cmzfla = prmfir
033800010201     C                   eval      cmzmba = prmfir
033900010201     C                   endif
034000010201     C                   eval      cmznrr = *zeros
034100010215     C                   move      §ctrok        cmznrr
034200021025     C                   eval      cmzlba = vlrfl1
034300010201     C                   call(e)   'TIS711C'
034400010201     C                   parm                    dscmz
034500010201     C                   parm      *blanks       esito
034600010201     C                   if        %error
034700010201     C                             or cmzerr = '1'
034800010201     C                             or esito  = '1'
034900010201     C                   eval      wrkesito = '3'
035000010205     C                   endif
035100990920     C*
035200000613     C                   ENDSR
035300000613     C***
035400990910
035500000613     C     *inzsr        BEGSR
035600990910     C*
035700990910     C     *entry        plist
035800990920     C                   parm                    tivlrds
035900990921     C                   parm      wrkesito      esito
036000000724     C                   parm                    prmlit
036100000710     C                   parm                    prmfir
036200000613     C*
036300000830     C* CALCOLA LA DATA CORRENTE
036400000830     C                   time                    wn14             14 0
036500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
036600000830     C                   z-add     wn8           g08dat
036700000830     C                   z-add     *zeros        g08inv
036800000830     C                   movel     '0'           g08err
036900000830     C                   call      'XSRDA8'
037000000830     C                   parm                    wlbda8
037100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
037200000830     C*
037300000613     C                   ENDSR
037400000613     C***
037500990908
037600010319     Otis7bbbj  E            riepilogo         2
037700990915     O                                              'Upload via Internet'
037800990915     O                                           +1 'Traduzione TIVIN00R -'
037900021113     O                                           +1 'FIVABWWR'
038000010201     O                                           +1 'Report testate bolle'
038100990915     O          E            riepilogo   2
038200990915     O                       wrkdata
038300990915     O                       wrkora              +1
038400990915     O                       procname            +1
038500990915     O          E            riepilogo   2
038600990915     O                                              'Cliente..................:'
038700990915     O                       VABCCM        z     +1
038800990915     O          E            riepilogo   2
038900990920     O                                              'Riferimento Strategi.....:'
039000990920     O                       vlrhdl              +1
039100990915     O          E            riepilogo   2
039200990915     O                                              'Giusti...................:'
039300010215     O                       §CTROK        2   +  1
039400990915     O          E            riepilogo   2
039500990915     O                                              'Sbagliati e corretti.....:'
039600971022     O                       §CTRMO        2   +  1
039700990915     O          E            riepilogo   2
039800990915     O                                              'Sbagliati e scartati.....:'
039900971022     O                       §CTRNO        2   +  1
040000000613
040100010319     Otis7bbbjs E            testdett          2
040200000613     O                                              'Upload via Internet'
040300000613     O                                           +1 'Traduzione TIVIN00R -'
040400021113     O                                           +1 'FIVABWWR'
040500010201     O                                           +1 'Report testate bolle'
040600000616     O          E            testdett    3
040700000613     O                                           +2 'N° rec'
040800000613     O                                           +3 'Anteprima contenuto'
040900000616     O          E            rigadett    2
041000000613     O                       rrnum               +2
041100000621     O                       recko               +3
041200000616     O          E            findett     2
041300000613     O                       wrkdata
041400000613     O                       wrkora              +1
041500000613     O                       procname            +1
041600000616     O          E            findett     2
041700000613     O                                              'Cliente..................:'
041800000613     O                       VABCCM        z     +1
041900000616     O          E            findett     2
042000000613     O                                              'Riferimento Strategi.....:'
042100000613     O                       vlrhdl              +1
042200000616     O          E            findett     2
042300000613     O                                              'Giusti...................:'
042400010215     O                       §CTROK        2   +  1
042500000616     O          E            findett     2
042600000613     O                                              'Sbagliati e corretti.....:'
042700000613     O                       §CTRMO        2   +  1
042800000616     O          E            findett     2
042900000613     O                                              'Sbagliati e scartati.....:'
043000000613     O                       §CTRNO        2   +  1
043100000512** CMD - COMANDI CL
043200011115OVRPRTF FILE(TIS7BBBJ) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
043300011115OVRPRTF FILE(TIS7BBBJS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
043400010319DLTOVR FILE(TIS7BBBJ TIS7BBBJS) LVL(*)
043500000512
043600000512
