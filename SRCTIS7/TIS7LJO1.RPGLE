000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700010215     Ftis7liuj  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010215     Ftis7liujs O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
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
003100011113     D depcmd          s            150    INZ(*blanks)
003200010215     D depScat         s              5  0 INZ(*zeros)
003300010215     D depBaul         s              5  0 INZ(*zeros)
003400010215     D depPeso         s              7  2 INZ(*zeros)
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
006800011113     C                   Z-ADD     150           LENGH            15 5
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
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000616     C                   PARM                    depcmd
008400000616     C                   PARM                    LENGH
008500000616     C*
008600010215     C                   if        not %open(tis7liujs)
008700010215     C                   open      tis7liujs
008800000616     C                   except    testdett
008900000613     C                   endif
009000000613     C*
009100010215     C                   EXSR      RWVAB                                        LETT/SCR. VAB
009200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009300010202     C*
009400010215     C                   if        %open(tis7liujs)
009500000616     C                   except    findett
009600010215     C                   close     tis7liujs
009700000613     C                   endif
009800000616     C*
009900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
010000011113     C                   Z-ADD     150           LENGH            15 5
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
011400010215     C                   if        not %open(tis7liuj)
011500010215     C                   open      tis7liuj
011600990915     C                   endif
011700990915     C*
011800990915     C                   except    riepilogo
011900990915     C*
012000010215     C                   if        %open(tis7liuj)
012100010215     C                   close     tis7liuj
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
020600010215     C                   Z-ADD     *zeros        depScat
020700010215     C                   Z-ADD     *zeros        depBaul
020800010215     C                   Z-ADD     *zeros        depPeso
020900000801     C*
021000000801     C                   ENDSR
021100000801     C*----------------------------------------------------*
021200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021300000801     C*----------------------------------------------------*
021400000801     C     DEFCAM        BEGSR
021500000801     C*
021600021113     C                   CLEAR                   FIVAB000
021700010215     C                   Z-ADD     0180555       VABCCM
021800010215     C                   Z-ADD     018           VABLNP
021900000801     C*
022000000801     C                   ENDSR
022100000801     C*----------------------------------------------------*
022200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
022300000801     C*----------------------------------------------------*
022400010201     C     IMPVAB        BEGSR
022500000801     C*
022600000801     C                   Z-ADD     *zeros        errore            1 0
022700000830     C                   MOVEL     datcor        VABAAS
022800000830     C                   MOVE      datcor        VABMGS
022801021113     C                   MOVE(P)   vlrpoi        VABFGS
022900010215     C*
023000010215     C* Reperimento campi ALFA
023100010215     C                   EVAL      VABRMA=%trim(%subst(vindta:107:12))
023200010215     C                   EVAL      VABRSD=%trim(%subst(vindta:119:26))
023300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
023400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
023500020117     C* ==
023600010215     C                   EVAL      VABIND=%trim(%subst(vindta:144:40))
023700010215     C                   EVAL      VABLOD=%trim(%subst(vindta:207:35))
023800010215     C                   EVAL      VABPRD=%trim(%subst(vindta:242:2))
023900010215     C                   EVAL      VABNOT='Tel. '+%trim(%subst(vindta:263:15))
024000010215     C*
024100010215     C                   EVAL      PiStr=%trim(%subst(vindta:200:7))
024200000801     C                   EXSR      CHKNUM
024300000801     C                   IF        PiInt=*on
024400000801     C                   Z-ADD     PiVal         Num5_0
024500010208     C                   MOVEL(p)  Num5_0        VABCAD
024600000801     C                   ELSE
024700000801     C                   ADD       1             errore
024800000801     C                   EVAL      vinmsg = %trimr(vinmsg)
024900000801     C                             + ' ' + 'VABCAD'
025000000801     C                   ENDIF
025100010215     C*
025200010215     C* Reperimento campi NUMERICI e CALCOLATI
025300010301     C                   EVAL      PiStr=%trim(%subst(vindta:40:5))
025400010201     C                   EXSR      CHKNUM
025500010201     C                   IF        PiInt=*on
025600010215     C                   Z-ADD     PiVal         depScat
025700010201     C                   ENDIF
025800010215     C*
025900010215     C                   EVAL      PiStr=%trim(%subst(vindta:50:5))
026000010201     C                   EXSR      CHKNUM
026100010201     C                   IF        PiNum=*on
026200010215     C                   Z-ADD     PiVal         depBaul
026300010201     C                   ENDIF
026400010215     C*
026500010215     C                   EVAL      PiStr=%trim(%subst(vindta:68:8))
026600010215     C                   EXSR      CHKNUM
026700010215     C                   IF        PiNum=*on
026800010215     C                   Z-ADD     PiVal         depPeso
026900010215     C                   ENDIF
027000010215     C* ==>
027100010215     C                   IF        depScat = *zeros and depBaul > *zeros
027200010215     C                   EVAL      VABPKB = 15 * depBaul
027300010215     C                   EVAL      VABNCL = depBaul
027400010215     C                   EVAL      VABCTR = 100
027500010215     C                   ENDIF
027600010301     C                   IF        depScat > *zeros and depBaul = *zeros
027700010215     C                   EVAL      VABPKB = depPeso
027800010215     C                   EVAL      VABNCL = depScat
027900010215     C                   EVAL      VABCTR = *zeros
028000010215     C                   ENDIF
028100010215     C                   IF        depBaul = *zeros and depScat = *zeros
028200010215     C                   EVAL      VABPKB = *zeros
028300010215     C                   EVAL      VABNCL = *zeros
028400010215     C                   EVAL      VABCTR = *zeros
028500010215     C                   ENDIF
028600010301     C                   IF        depBaul > *zeros and depScat > *zeros
028700010301     C                   EVAL      VABNCL = depScat + depBaul
028800010301     C                   EVAL      VABPKB = 15 * (depScat + depBaul)
028900010301     C                   EVAL      VABCTR = *zeros
029000010301     C                   ENDIF
029100010215     C* ===
029200010215     C                   EVAL      PiStr=%trim(%subst(vindta:87:10))
029300010201     C                   EXSR      CHKNUM
029400010201     C                   IF        PiInt=*on
029500010215     C                   Z-ADD     PiVal         VABNSP
029600010301     C                   Z-ADD     PiVal         VABRMN
029700010201     C                   ELSE
029800010201     C                   ADD       1             errore
029900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030000010301     C                             + ' ' + 'VABNSP VABRMN'
030100010201     C                   ENDIF
030200010205     C*
030300010215     C                   EVAL      VABCBO = '1'
030400010205     C*
030500010202     C*
030600000801     C* Ebbene...
030700000801     C                   ADD       1             §CTRMO
030800010201     C                   IF        errore <> *zeros
030900000801     C                   ADD       1             §CTRNO
031000000801     C                   EVAL      recko = vindta
031100000801     C                   EXCEPT    rigadett
031200000801     C                   EVAL      vinflg = '2'
031300000801     C                   ELSE
031400010215     C                   ADD       1             §CTROK
031500000801     C                   ENDIF
031600000801     C*
031700000801     C                   ENDSR
031800000801     C*----------------------------------------------------*
031900000801     C*  CONTROLLO NUMERICITA' CAMPI
032000000801     C*----------------------------------------------------*
032100000801     C     CHKNUM        BEGSR
032200000801     C*
032300000801     C                   call(e)   'ISNUMERIC'
032400000801     C                   PARM                    PiStr            30
032500000801     C                   PARM      ','           PiDecChr          1
032600000801     C                   PARM      *ZEROS        PiVal            30 9
032700000801     C                   PARM      '0'           PiInt             1
032800000801     C                   PARM      '0'           PiNum             1
032900000801     C                   IF        %error
033000000801     C                   EVAL      PiInt=*off
033100000801     C                   ENDIF
033200000801     C*
033300000801     C                   ENDSR
033400000801     C***
033500000801
033600000801
033700000801
033800990920      /TITLE Invio dei dati al punto operativo.
033900010202     C     invio         BEGSR
034000010201     C*
034100021113     C* Invio FIVAB
034200010201     C                   reset                   dscmz
034300010201     C                   move      vlrpoi        cmzdst
034400010201     C                   eval      cmzfld = vlrfou
034500010201     C                   eval      cmzmbd = vlrhdl
034600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
034700010201     C                   if        prmfir = *blanks
034800021113     C                   eval      cmzfla = 'FIVAB00F'
034900021113     C                   eval      cmzmba = 'FIVAB00F'
035000010201     C                   else
035100010201     C                   eval      cmzfla = prmfir
035200010201     C                   eval      cmzmba = prmfir
035300010201     C                   endif
035400010201     C                   eval      cmznrr = *zeros
035500010215     C                   move      §ctrok        cmznrr
035600021025     C                   eval      cmzlba = vlrfl1
035700010201     C                   call(e)   'TIS711C'
035800010201     C                   parm                    dscmz
035900010201     C                   parm      *blanks       esito
036000010201     C                   if        %error
036100010201     C                             or cmzerr = '1'
036200010201     C                             or esito  = '1'
036300010201     C                   eval      wrkesito = '3'
036400010205     C                   endif
036500990920     C*
036600000613     C                   ENDSR
036700000613     C***
036800990910
036900000613     C     *inzsr        BEGSR
037000990910     C*
037100990910     C     *entry        plist
037200990920     C                   parm                    tivlrds
037300990921     C                   parm      wrkesito      esito
037400000724     C                   parm                    prmlit
037500000710     C                   parm                    prmfir
037600000613     C*
037700000830     C* CALCOLA LA DATA CORRENTE
037800000830     C                   time                    wn14             14 0
037900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
038000000830     C                   z-add     wn8           g08dat
038100000830     C                   z-add     *zeros        g08inv
038200000830     C                   movel     '0'           g08err
038300000830     C                   call      'XSRDA8'
038400000830     C                   parm                    wlbda8
038500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
038600000830     C*
038700000613     C                   ENDSR
038800000613     C***
038900990908
039000010215     Otis7liuj  E            riepilogo         2
039100990915     O                                              'Upload via Internet'
039200990915     O                                           +1 'Traduzione TIVIN00R -'
039300021113     O                                           +1 'FIVABWWR'
039400010201     O                                           +1 'Report testate bolle'
039500990915     O          E            riepilogo   2
039600990915     O                       wrkdata
039700990915     O                       wrkora              +1
039800990915     O                       procname            +1
039900990915     O          E            riepilogo   2
040000990915     O                                              'Cliente..................:'
040100990915     O                       VABCCM        z     +1
040200990915     O          E            riepilogo   2
040300990920     O                                              'Riferimento Strategi.....:'
040400990920     O                       vlrhdl              +1
040500990915     O          E            riepilogo   2
040600990915     O                                              'Giusti...................:'
040700010215     O                       §CTROK        2   +  1
040800990915     O          E            riepilogo   2
040900990915     O                                              'Sbagliati e corretti.....:'
041000971022     O                       §CTRMO        2   +  1
041100990915     O          E            riepilogo   2
041200990915     O                                              'Sbagliati e scartati.....:'
041300971022     O                       §CTRNO        2   +  1
041400000613
041500010215     Otis7liujs E            testdett          2
041600000613     O                                              'Upload via Internet'
041700000613     O                                           +1 'Traduzione TIVIN00R -'
041800021113     O                                           +1 'FIVABWWR'
041900010201     O                                           +1 'Report testate bolle'
042000000616     O          E            testdett    3
042100000613     O                                           +2 'N° rec'
042200000613     O                                           +3 'Anteprima contenuto'
042300000616     O          E            rigadett    2
042400000613     O                       rrnum               +2
042500000621     O                       recko               +3
042600000616     O          E            findett     2
042700000613     O                       wrkdata
042800000613     O                       wrkora              +1
042900000613     O                       procname            +1
043000000616     O          E            findett     2
043100000613     O                                              'Cliente..................:'
043200000613     O                       VABCCM        z     +1
043300000616     O          E            findett     2
043400000613     O                                              'Riferimento Strategi.....:'
043500000613     O                       vlrhdl              +1
043600000616     O          E            findett     2
043700000613     O                                              'Giusti...................:'
043800010215     O                       §CTROK        2   +  1
043900000616     O          E            findett     2
044000000613     O                                              'Sbagliati e corretti.....:'
044100000613     O                       §CTRMO        2   +  1
044200000616     O          E            findett     2
044300000613     O                                              'Sbagliati e scartati.....:'
044400000613     O                       §CTRNO        2   +  1
044500000512** CMD - COMANDI CL
044600011113OVRPRTF FILE(TIS7LIUJ) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
044700011113OVRPRTF FILE(TIS7LIUJS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
044800010215DLTOVR FILE(TIS7LIUJ TIS7LIUJS) LVL(*)
044900000512
045000000512
