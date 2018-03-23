000100100301      /TITLE Upload via Internet: traduzione in EDIVAWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100301     FEDIVABwr  O    E             DISK    usropn
000700010322     Ftis7arnp  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010322     Ftis7arnps O    f  198        PRINTER usropn
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
003200000801
003300010213
003400000801     D  Num5_0         s              5  0
003500990908
003600010201
003700010201
003800990915     C                   time                    wrkdata
003900990915     C                   time                    wrkora
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300000724     C*
004400000724     C* SE OCCORRE SPEDIRE IN FILIALE
004500000724     C                   if        vlrpoi <> *zeros
004600000724     C*
004700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
004800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
004900000724     C     vlrpoi        chain     azorg01l
005000000724     C                   if        %found
005100000616     C                   movel(p)  CMD(1)        depcmd
005200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
005300000616     C*
005400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
005500011113     C                   Z-ADD     150           LENGH            15 5
005600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
005700000616     C                   PARM                    depcmd
005800000616     C                   PARM                    LENGH
005900000724     C*
006000000724     C                   endif
006100020829     C                   endif
006200000616     C*
006300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006400000616     C                   movel(p)  CMD(2)        depcmd
006500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
006600000616     C*
006700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
006800011113     C                   Z-ADD     150           LENGH            15 5
006900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007000000616     C                   PARM                    depcmd
007100000616     C                   PARM                    LENGH
007200000616     C*
007300010322     C                   if        not %open(tis7arnps)
007400010322     C                   open      tis7arnps
007500000616     C                   except    testdett
007600000613     C                   endif
007700000613     C*
007800010215     C                   EXSR      RWVAB                                        LETT/SCR. VAB
007900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
008000010202     C*
008100010322     C                   if        %open(tis7arnps)
008200000616     C                   except    findett
008300010322     C                   close     tis7arnps
008400000613     C                   endif
008500000616     C*
008600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
008700011113     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    CMD(3)
009000000616     C                   PARM                    LENGH
009100000616     C*
009200000801     C
009300010201     C                   seton                                        LR
009400000613
009500000613
009600000613     C*--------------------------------------------------------
009700000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
009800000613     C*--------------------------------------------------------
009900000621     C     STPR          BEGSR
010000000613     C*
010100010322     C                   if        not %open(tis7arnp)
010200010322     C                   open      tis7arnp
010300990915     C                   endif
010400990915     C*
010500990915     C                   except    riepilogo
010600990915     C*
010700010322     C                   if        %open(tis7arnp)
010800010322     C                   close     tis7arnp
010900990914     C                   endif
011000990910     C*
011100000613     C                   ENDSR
011200000613     C***
011300990908
011400000801
011500910830     C*--------------------------------------------------------
011600100301     C* RWVAB   LEGGE tivin00r E SCRIVE EDIVABWR              *
011700910830     C*--------------------------------------------------------
011800010215     C     RWVAB         BEGSR
011900990910     C*
012000990914     C                   if        not %open(tivin00r)
012100990908     C                   open      tivin00r
012200990914     C                   endif
012300100301     C                   if        not %open(edivabwr)
012400100301     C                   open      edivabwr
012500990914     C                   endif
012600990910     C*
012700010215     C                   clear                   §CTROK            5 0
012800000801     C                   clear                   §CTRMO            5 0
012900000801     C                   clear                   §CTRNO            5 0
013000990910     C*
013100921023     C                   DO        *HIVAL
013200990913     C*
013300990915     C                   READ      tivin00r                               70
013400000905     C                   if        vindta > *blanks
013500000613     C                   add       1             rrnum
013600000801     C*
013700000801     C                   if        *in70 = *off
013800000801     C                             and
013900000801     C                             (vinflg = *blanks
014000000801     C                              or vinflg = '0'
014100000801     C                              or vinflg = '2')
014200000801     C*
014300000801     C                   clear                   vinmsg
014400000801     C                   eval      vinflg = '1'
014500010201     C*
014600000801     C                   exsr      inzvar
014700000801     C                   exsr      defcam
014800010201     C                   exsr      impvab
014900100301     C*
015000100301     C* Imposto i campi x il CMR
015100100301     C                   eval      VABCMR = 'MONDADORI'
015200100301     C                   eval      VABDCM = datcor
015300100301     C                   eval      VABDTS = datcor
015400100301     C                   eval      VABHMS = oracor
015500100301     C                   eval      VABCNT = 1
015600100301     C*
015700100301     C                   write     edivab00
015800010201     C                   endif
015900000905     C*
016000000905     C                   else
016100000905     C                   eval      vinflg = '1'
016200000905     C                   endif
016300000905     C*
016400000905     C  N70              update    tivin000
016500000905     C*
016600991022     C  N70              ENDdo
016700010202     C*
016800990910
016900990910     C* Se non ci sono record con errori ...
017000000710     C                   if        §ctrno = 0
017100990910     C* ... restituisco esito OK.
017200990921     C                   eval      wrkesito = '0'
017300990910     C                   else
017400010215     C                   if        §ctrok   > 0
017500990921     C                   eval      wrkesito = '1'
017600000710     C                   else
017700000710     C                   eval      wrkesito = '2'
017800990910     C                   endif
017900000710     C                   endif
018000990910     C*
018100990914     C                   if        %open(tivin00r)
018200990908     C                   close     tivin00r
018300990914     C                   endif
018400100301     C                   if        %open(edivabwr)
018500100301     C                   close     edivabwr
018600990914     C                   endif
018700990910     C*
018800010215     C                   if        §ctrok > 0
018900000724     C                             and vlrpoi <> *zeros
019000010202     C                   exsr      invio
019100990920     C                   endif
019200990920     C*
019300910830     C                   ENDSR
019400000613     C***
019500990920
019600000801     C*----------------------------------------------------*
019700010322     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019800000801     C*----------------------------------------------------*
019900010201     C     INZVAR        BEGSR
020000000801     C*
020100010322     C                   Z-ADD     *zeros        Num5_0
020200000801     C*
020300000801     C                   ENDSR
020400000801     C*----------------------------------------------------*
020500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020600000801     C*----------------------------------------------------*
020700000801     C     DEFCAM        BEGSR
020800000801     C*
020900050103     C* Inizializzo il buffer del record da scrivere
021000100301     C                   CLEAR                   EDIVAB00
021100050103     C* Imposto i valori di default...
021200050103     C                   Z-ADD     0430001       VABCCM
021300010322     C                   Z-ADD     043           VABLNP
021400090930     C                   Z-ADD     301           VABCTR
021500010402     C                   MOVEL     '7Q'          VABCTM
021600050103     C* ... e poi verifico se sono stati passati come parametri
021700050103     C                   IF        vlrppt > *blanks
021800050103     C                   IF        %subst(vlrppt:1:7) <> *blanks
021900050103     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022000050103     C                   EXSR      CHKNUM
022100050103     C                   IF        PiInt=*on
022200050103     C                   Z-ADD     PiVal         VABCCM
022300050103     C                   ENDIF
022400050103     C                   ENDIF
022500050103     C                   IF        %subst(vlrppt:8:3) <> *blanks
022600050103     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022700050103     C                   EXSR      CHKNUM
022800050103     C                   IF        PiInt=*on
022900050103     C                   Z-ADD     PiVal         VABLNP
023000050103     C                   ENDIF
023100050103     C                   ENDIF
023200050103     C                   IF        %subst(vlrppt:11:3) <> *blanks
023300050103     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023400050103     C                   EXSR      CHKNUM
023500050103     C                   IF        PiInt=*on
023600050103     C                   Z-ADD     PiVal         VABCTR
023700050103     C                   ENDIF
023800050103     C                   ENDIF
023900050103     C                   ENDIF
024000000801     C*
024100000801     C                   ENDSR
024200000801     C*----------------------------------------------------*
024300100301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
024400000801     C*----------------------------------------------------*
024500010201     C     IMPVAB        BEGSR
024600000801     C*
024700000801     C                   Z-ADD     *zeros        errore            1 0
024800000830     C                   MOVEL     datcor        VABAAS
024900000830     C                   MOVE      datcor        VABMGS
025000021113     C                   MOVE(P)   vlrpoi        VABFGS
025100010215     C*
025200010215     C* Reperimento campi ALFA
025300010322     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
025400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025600020117     C* ==
025700010322     C                   EVAL      VABIND=%trim(%subst(vindta:57:50))
025800010322     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
025900010322     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
026000020104     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
026100020104     C                   EVAL      VABNT2=%trim(%subst(vindta:232:35))
026200010322     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
026300010322     C                   EVAL      VABRMA=%trim(%subst(vindta:189:6))
026400010319     C*
026500010322     C                   EVAL      PiStr=%trim(%subst(vindta:189:6))
026600010319     C                   EXSR      CHKNUM
026700010319     C                   IF        PiInt=*on
026800010319     C                   Z-ADD     PiVal         VABRMN
026900010322     C                   Z-ADD     PiVal         VABNSP
027000010319     C                   ELSE
027100010319     C                   ADD       1             errore
027200010319     C                   EVAL      vinmsg = %trimr(vinmsg)
027300010322     C                             + ' ' + 'VABRMN/VABNSP'
027400010319     C                   ENDIF
027500010319     C*
027600010322     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
027700010319     C                   EXSR      CHKNUM
027800010319     C                   IF        PiInt=*on
027900010319     C                   Z-ADD     PiVal         Num5_0
028000010319     C                   MOVEL(p)  Num5_0        VABCAD
028100010319     C                   ELSE
028200010319     C                   ADD       1             errore
028300010319     C                   EVAL      vinmsg = %trimr(vinmsg)
028400010319     C                             + ' ' + 'VABCAD'
028500010319     C                   ENDIF
028600010319     C*
028700010322     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
028800010201     C                   EXSR      CHKNUM
028900010201     C                   IF        PiInt=*on
029000010322     C                   Z-ADD     PiVal         VABNCL
029100010201     C                   ENDIF
029200110729     C*
029300110729     C                   MOVEL     *blanks       wVARIE           80
029400110729     C                   EVAL      wVARIE = %trim(%subst(vindta:197:80))
029500110729     C                   IF        %scan('CONSEGNA':wVARIE) > 0
029600110729     C                   Z-ADD     *zeros        wPos              4 0
029700110729     C                   EVAL      wPos = %scan('-201':wVARIE)
029800110729     C                   IF        wPos > *zeros
029900110729     C                   EVAL      wPos = wPos + 1
030000110729     C                   EVAL      PiStr=%subst(wVARIE:wPos:4)   +
030100110729     C                                   %subst(wVARIE:wPos-3:2) +
030200110729     C                                   %subst(wVARIE:wPos-3-3:2)
030300110729     C                   EXSR      CHKNUM
030400110729     C                   IF        PiInt=*on
030500110729     C                   Z-ADD     PiVal         VABDCR
030600110729     C                   ENDIF
030700110729     C                   ENDIF
030800110729     C                   ENDIF
030900010215     C*
031000010322     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
031100010201     C                   EXSR      CHKNUM
031200010201     C                   IF        PiNum=*on
031300010322     C                   EVAL      PiVal = PiVal / 1000
031400010322     C                   Z-ADD(H)  PiVal         VABPKB
031500010201     C                   ENDIF
031600010215     C*
031700020104     C                   EVAL      PiStr=%trim(%subst(vindta:277:12))
031800020104     C                   EXSR      CHKNUM
031900020104     C                   IF        PiNum=*on
032000020104     C                   Z-ADD     PiVal         VABCAS
032100020104     C                   ENDIF
032200010205     C*
032300010322     C                   EVAL      VABCBO = '1 '
032400010322     C*
032500010322     C                   IF        VABCAS > *zeros
032600020104     C                   EVAL      VABVCA=%trim(%subst(vindta:309:3))
032700010322     C                   EVAL      VABCBO = '4 '
032800110414     C                   EVAL      VABTIC = 'BM'
032900010322     C                   ENDIF
033000010202     C*
033100000801     C* Ebbene...
033200000801     C                   ADD       1             §CTRMO
033300010201     C                   IF        errore <> *zeros
033400000801     C                   ADD       1             §CTRNO
033500000801     C                   EVAL      recko = vindta
033600000801     C                   EXCEPT    rigadett
033700000801     C                   EVAL      vinflg = '2'
033800000801     C                   ELSE
033900010215     C                   ADD       1             §CTROK
034000000801     C                   ENDIF
034100000801     C*
034200000801     C                   ENDSR
034300000801     C*----------------------------------------------------*
034400000801     C*  CONTROLLO NUMERICITA' CAMPI
034500000801     C*----------------------------------------------------*
034600000801     C     CHKNUM        BEGSR
034700000801     C*
034800000801     C                   call(e)   'ISNUMERIC'
034900000801     C                   PARM                    PiStr            30
035000000801     C                   PARM      ','           PiDecChr          1
035100000801     C                   PARM      *ZEROS        PiVal            30 9
035200000801     C                   PARM      '0'           PiInt             1
035300000801     C                   PARM      '0'           PiNum             1
035400000801     C                   IF        %error
035500000801     C                   EVAL      PiInt=*off
035600000801     C                   ENDIF
035700000801     C*
035800000801     C                   ENDSR
035900000801     C***
036000000801
036100000801
036200000801
036300990920      /TITLE Invio dei dati al punto operativo.
036400010202     C     invio         BEGSR
036500010201     C*
036600100301     C* Invio EDIVAB
036700010201     C                   reset                   dscmz
036800010201     C                   move      vlrpoi        cmzdst
036900010201     C                   eval      cmzfld = vlrfou
037000010201     C                   eval      cmzmbd = vlrhdl
037100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
037200010201     C                   if        prmfir = *blanks
037300100301     C                   eval      cmzfla = 'EDIVAB0F'
037400100301     C                   eval      cmzmba = 'EDIVAB0F'
037500010201     C                   else
037600010201     C                   eval      cmzfla = prmfir
037700010201     C                   eval      cmzmba = prmfir
037800010201     C                   endif
037900010201     C                   eval      cmznrr = *zeros
038000010215     C                   move      §ctrok        cmznrr
038100021025     C                   eval      cmzlba = vlrfl1
038200010201     C                   call(e)   'TIS711C'
038300010201     C                   parm                    dscmz
038400010201     C                   parm      *blanks       esito
038500010201     C                   if        %error
038600010201     C                             or cmzerr = '1'
038700010201     C                             or esito  = '1'
038800010201     C                   eval      wrkesito = '3'
038900010205     C                   endif
039000990920     C*
039100000613     C                   ENDSR
039200000613     C***
039300990910
039400000613     C     *inzsr        BEGSR
039500990910     C*
039600990910     C     *entry        plist
039700990920     C                   parm                    tivlrds
039800990921     C                   parm      wrkesito      esito
039900000724     C                   parm                    prmlit
040000000710     C                   parm                    prmfir
040100000613     C*
040200000830     C* CALCOLA LA DATA CORRENTE
040300000830     C                   time                    wn14             14 0
040400100301     C                   movel     wn14          oracor            6 0          *ORA
040500100301     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
040600100301     C                   eval      datcor = %dec(%date() : *ISO)
040700000830     C*
040800000613     C                   ENDSR
040900000613     C***
041000990908
041100010322     Otis7arnp  E            riepilogo         2
041200990915     O                                              'Upload via Internet'
041300990915     O                                           +1 'Traduzione TIVIN00R -'
041400100301     O                                           +1 'EDIVABWR'
041500010201     O                                           +1 'Report testate bolle'
041600990915     O          E            riepilogo   2
041700990915     O                       wrkdata
041800990915     O                       wrkora              +1
041900990915     O                       procname            +1
042000990915     O          E            riepilogo   2
042100990915     O                                              'Cliente..................:'
042200990915     O                       VABCCM        z     +1
042300990915     O          E            riepilogo   2
042400990920     O                                              'Riferimento Strategi.....:'
042500990920     O                       vlrhdl              +1
042600990915     O          E            riepilogo   2
042700990915     O                                              'Giusti...................:'
042800010215     O                       §CTROK        2   +  1
042900990915     O          E            riepilogo   2
043000990915     O                                              'Sbagliati e corretti.....:'
043100971022     O                       §CTRMO        2   +  1
043200990915     O          E            riepilogo   2
043300990915     O                                              'Sbagliati e scartati.....:'
043400971022     O                       §CTRNO        2   +  1
043500000613
043600010322     Otis7arnps E            testdett          2
043700000613     O                                              'Upload via Internet'
043800000613     O                                           +1 'Traduzione TIVIN00R -'
043900100301     O                                           +1 'EDIVABWR'
044000010201     O                                           +1 'Report testate bolle'
044100000616     O          E            testdett    3
044200000613     O                                           +2 'N° rec'
044300000613     O                                           +3 'Anteprima contenuto'
044400000616     O          E            rigadett    2
044500000613     O                       rrnum               +2
044600000621     O                       recko               +3
044700000616     O          E            findett     2
044800000613     O                       wrkdata
044900000613     O                       wrkora              +1
045000000613     O                       procname            +1
045100000616     O          E            findett     2
045200000613     O                                              'Cliente..................:'
045300000613     O                       VABCCM        z     +1
045400000616     O          E            findett     2
045500000613     O                                              'Riferimento Strategi.....:'
045600000613     O                       vlrhdl              +1
045700000616     O          E            findett     2
045800000613     O                                              'Giusti...................:'
045900010215     O                       §CTROK        2   +  1
046000000616     O          E            findett     2
046100000613     O                                              'Sbagliati e corretti.....:'
046200000613     O                       §CTRMO        2   +  1
046300000616     O          E            findett     2
046400000613     O                                              'Sbagliati e scartati.....:'
046500000613     O                       §CTRNO        2   +  1
046600000512** CMD - COMANDI CL
046700011113OVRPRTF FILE(TIS7ARNP) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
046800011113OVRPRTF FILE(TIS7ARNPS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
046900010322DLTOVR FILE(TIS7ARNP TIS7ARNPS) LVL(*)
047000000512
047100000512
