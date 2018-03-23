000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700010329     Ftis7mdyp  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010329     Ftis7mdyps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000801     D*----------------------------------------------------
001700000801     D* DICHIARAZIOINE VARIABILI DI WRK
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
003300000801     D  Num5_0         s              5  0
003400000830
003500000830     D*------------------
003600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003700000830     D*------------------
003800000830     D WLBDA8          DS                  INZ
003900000830     D  G08DAT                 1      8  0
004000000830     D  G08INV                 9     16  0
004100000830     D  G08ERR                17     17
004200000830     D  G08TGI                18     22  0
004300000830     D*
004400990908
004500990915     C                   time                    wrkdata
004600990915     C                   time                    wrkora
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000000724     C*
005100000724     C* SE OCCORRE SPEDIRE IN FILIALE
005200000724     C                   if        vlrpoi <> *zeros
005300000724     C*
005400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
005500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
005600000724     C     vlrpoi        chain     azorg01l
005700000724     C                   if        %found
005800000616     C                   movel(p)  CMD(1)        depcmd
005900020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006000000616     C*
006100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006200011113     C                   Z-ADD     150           LENGH            15 5
006300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
006400000616     C                   PARM                    depcmd
006500000616     C                   PARM                    LENGH
006600000724     C*
006700000724     C                   endif
006800020829     C                   endif
006900000616     C*
007000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007100000616     C                   movel(p)  CMD(2)        depcmd
007200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
007300000616     C*
007400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
007500011113     C                   Z-ADD     150           LENGH            15 5
007600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007700000616     C                   PARM                    depcmd
007800000616     C                   PARM                    LENGH
007900000616     C*
008000010329     C                   if        not %open(tis7mdyps)
008100010329     C                   open      tis7mdyps
008200000616     C                   except    testdett
008300000613     C                   endif
008400000613     C*
008500941229     C                   EXSR      RWVAB                                        LETT/SCR. VAB
008600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
008700000613     C*
008800010329     C                   if        %open(tis7mdyps)
008900000616     C                   except    findett
009000010329     C                   close     tis7mdyps
009100000613     C                   endif
009200000616     C*
009300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
009400011113     C                   Z-ADD     150           LENGH            15 5
009500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009600000616     C                   PARM                    CMD(3)
009700000616     C                   PARM                    LENGH
009800000616     C*
009900000801     C
010000000613     C                   return
010100000613
010200000613
010300000613     C*--------------------------------------------------------
010400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
010500000613     C*--------------------------------------------------------
010600000621     C     STPR          BEGSR
010700000613     C*
010800010329     C                   if        not %open(tis7mdyp)
010900010329     C                   open      tis7mdyp
011000990915     C                   endif
011100990915     C*
011200990915     C                   except    riepilogo
011300990915     C*
011400010329     C                   if        %open(tis7mdyp)
011500010329     C                   close     tis7mdyp
011600990914     C                   endif
011700990910     C*
011800000613     C                   ENDSR
011900000613     C***
012000990908
012100000801
012200910830     C*--------------------------------------------------------
012300021113     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
012400910830     C*--------------------------------------------------------
012500941229     C     RWVAB         BEGSR
012600990910     C*
012700990914     C                   if        not %open(tivin00r)
012800990908     C                   open      tivin00r
012900990914     C                   endif
013000021113     C                   if        not %open(fivabwwr)
013100021113     C                   open      fivabwwr
013200990914     C                   endif
013300990910     C*
013400000801     C                   clear                   §CTROK            5 0
013500000801     C                   clear                   §CTRMO            5 0
013600000801     C                   clear                   §CTRNO            5 0
013700990910     C*
013800921023     C                   DO        *HIVAL
013900990913     C*
014000990915     C                   READ      tivin00r                               70
014100000905     C                   if        vindta > *blanks
014200000613     C                   add       1             rrnum
014300000801     C*
014400000801     C                   if        *in70 = *off
014500000801     C                             and
014600000801     C                             (vinflg = *blanks
014700000801     C                              or vinflg = '0'
014800000801     C                              or vinflg = '2')
014900000801     C*
015000000801     C                   clear                   vinmsg
015100000801     C                   eval      vinflg = '1'
015200010329     C                   if        %trim(%subst(vindta:1:2)) = '02'
015300000801     C                   exsr      inzvar
015400000801     C                   exsr      defcam
015500010329     C                   exsr      impvab
015600021113     C                   write     fivab000
015700000801     C                   endif
015800000905     C                   endif
015900000905     C*
016000000905     C                   else
016100000905     C                   eval      vinflg = '1'
016200000905     C                   endif
016300000905     C*
016400000905     C  N70              update    tivin000
016500000905     C*
016600991022     C  N70              ENDdo
016700990910
016800990910     C* Se non ci sono record con errori ...
016900000710     C                   if        §ctrno = 0
017000990910     C* ... restituisco esito OK.
017100990921     C                   eval      wrkesito = '0'
017200990910     C                   else
017300000710     C                   if        §ctrok > 0
017400990921     C                   eval      wrkesito = '1'
017500000710     C                   else
017600000710     C                   eval      wrkesito = '2'
017700990910     C                   endif
017800000710     C                   endif
017900990910     C*
018000990914     C                   if        %open(tivin00r)
018100990908     C                   close     tivin00r
018200990914     C                   endif
018300021113     C                   if        %open(fivabwwr)
018400021113     C                   close     fivabwwr
018500990914     C                   endif
018600990910     C*
018700990920     C                   if        §ctrok > 0
018800000724     C                             and vlrpoi <> *zeros
018900000613     C                   exsr      invio
019000990920     C                   endif
019100990920     C*
019200910830     C                   ENDSR
019300000613     C***
019400990920
019500000801     C*----------------------------------------------------*
019600010329     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019700000801     C*----------------------------------------------------*
019800000801     C     INZVAR        BEGSR
019900000801     C*
020000010329     C                   Z-ADD     *zeros        Num5_0
020100000801     C*
020200000801     C                   ENDSR
020300000801     C*----------------------------------------------------*
020400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020500000801     C*----------------------------------------------------*
020600000801     C     DEFCAM        BEGSR
020700000801     C*
020800021113     C                   CLEAR                   FIVAB000
020900010329     C                   MOVEL     *blanks       DEPCCM            7
021000010329     C                   EVAL      DEPCCM = %trim(VLRPPT)
021100010329     C                   MOVE(p)   DEPCCM        VABCCM
021200010329     C                   Z-ADD     043           VABLNP
021300010329     C                   Z-ADD     000           VABCTR
021400000801     C*
021500000801     C                   ENDSR
021600010329     C*----------------------------------------------------*
021700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
021800010329     C*----------------------------------------------------*
021900010329     C     IMPVAB        BEGSR
022000010329     C*
022100010329     C                   Z-ADD     *zeros        errore            1 0
022200010329     C                   MOVEL     datcor        VABAAS
022300010329     C                   MOVE      datcor        VABMGS
022400021113     C                   MOVE(P)   vlrpoi        VABFGS
022500010329     C*
022600010329     C* Reperimento campi ALFA
022700010618     C                   IF        %subst(vindta:125:30) > *blanks
022800010618     C                   EVAL      VABRSD=%trim(%subst(vindta:125:30))
022900010618     C                   EVAL      VABIND=%trim(%subst(vindta:155:30))
023000010618     C*
023100010618     C                   EVAL      PiStr=%trim(%subst(vindta:185:5))
023200010618     C                   EXSR      CHKNUM
023300010618     C                   IF        PiInt=*on
023400010618     C                   Z-ADD     PiVal         Num5_0
023500010618     C                   MOVEL(p)  Num5_0        VABCAD
023600010618     C                   ELSE
023700010618     C                   ADD       1             errore
023800010618     C                   EVAL      vinmsg = %trimr(vinmsg)
023900010618     C                             + ' ' + 'VABCAD'
024000010618     C                   ENDIF
024100010618     C*
024200010618     C                   EVAL      VABLOD=%trim(%subst(vindta:190:30))
024300010618     C                   EVAL      VABPRD=%trim(%subst(vindta:220:2))
024400010618     C*
024500010618     C                   ELSE
024600010618     C*
024700010618     C                   EVAL      VABRSD=%trim(%subst(vindta:28:30))
024800010618     C                   EVAL      VABIND=%trim(%subst(vindta:58:30))
024900010618     C*
025000010618     C                   EVAL      PiStr=%trim(%subst(vindta:88:5))
025100010618     C                   EXSR      CHKNUM
025200010618     C                   IF        PiInt=*on
025300010618     C                   Z-ADD     PiVal         Num5_0
025400010618     C                   MOVEL(p)  Num5_0        VABCAD
025500010618     C                   ELSE
025600010618     C                   ADD       1             errore
025700010618     C                   EVAL      vinmsg = %trimr(vinmsg)
025800010618     C                             + ' ' + 'VABCAD'
025900010618     C                   ENDIF
026000010618     C*
026100010618     C                   EVAL      VABLOD=%trim(%subst(vindta:93:30))
026200010618     C                   EVAL      VABPRD=%trim(%subst(vindta:123:2))
026300010618     C*
026400010618     C                   ENDIF
026500020117     C*
026600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026800020117     C* ==
026900010329     C                   EVAL      VABRMA=%trim(%subst(vindta:3:8))
027000010329     C*
027100010329     C                   EVAL      PiStr=%trim(%subst(vindta:3:8))
027200010329     C                   EXSR      CHKNUM
027300010329     C                   IF        PiInt=*on
027400010329     C                   Z-ADD     PiVal         VABRMN
027500010329     C                   ELSE
027600010329     C                   ADD       1             errore
027700010329     C                   EVAL      vinmsg = %trimr(vinmsg)
027800010329     C                             + ' ' + 'VABRMN'
027900010329     C                   ENDIF
028000010329     C*
028100010329     C                   EVAL      PiStr=%trim(%subst(vindta:19:3))
028200010329     C                   EXSR      CHKNUM
028300010329     C                   IF        PiInt=*on
028400010329     C                   Z-ADD     PiVal         VABNCL
028500010329     C                   ENDIF
028600010329     C*
028700010329     C                   EVAL      PiStr=%trim(%subst(vindta:22:6))
028800010329     C                   EXSR      CHKNUM
028900010329     C                   IF        PiNum=*on
029000010329     C                   Z-ADD(H)  PiVal         VABPKB
029100010329     C                   ENDIF
029200010329     C*
029300011005     C                   EVAL      PiStr=%trim(%subst(vindta:234:12))
029400010329     C                   EXSR      CHKNUM
029500010329     C                   IF        PiNum=*on
029600010329     C                   Z-ADD     PiVal         VABCAS
029700010329     C                   ENDIF
029800010329     C*
029900010329     C                   EVAL      VABCBO = '1 '
030000011026     C                   IF        %trim(%subst(vindta:250:1)) = '2'
030100011026     C                   EVAL      VABCBO = '2 '
030200011026     C                   ENDIF
030300010329     C*
030400010329     C                   IF        VABCAS > *zeros
030500011026     C                   EVAL      VABVCA=%trim(%subst(vindta:246:3))
030600011026     C                   IF        VABVCA = *blanks
030700011026     C                   EVAL      VABVCA = 'ITL'
030800011026     C                   ENDIF
030900010329     C                   EVAL      VABCBO = '4 '
031000011026     C                   IF        %trim(%subst(vindta:250:1)) = '6'
031100011026     C                   EVAL      VABCBO = '6 '
031200011026     C                   ENDIF
031300010329     C                   ENDIF
031400020506     C*
031500020506     C                   EVAL      PiStr=%trim((%subst(vindta:255:4))  +
031600020506     C                                         (%subst(vindta:253:2))  +
031700020506     C                                         (%subst(vindta:251:2)))
031800020506     C                   IF        PiStr<>*blanks
031900020506     C                   EXSR      CHKNUM
032000020506     C                   IF        PiInt=*on
032100020506     C                   Z-ADD     PiVal         VABDCR
032200020506     C                   ELSE
032300020506     C                   ADD       1             errore
032400020506     C                   EVAL      vinmsg = %trimr(vinmsg)
032500020506     C                             + ' ' + 'VABDCR'
032600020506     C                   ENDIF
032700020506     C                   ENDIF
032800020506     C*
032900020506     C                   EVAL      PiStr=%trim(%subst(vindta:259:4))
033000020506     C                   IF        PiStr<>*blanks
033100020506     C                   EXSR      CHKNUM
033200020506     C                   IF        PiInt=*on
033300020506     C                   Z-ADD     PiVal         VABHCR
033400020506     C                   ELSE
033500020506     C                   ADD       1             errore
033600020506     C                   EVAL      vinmsg = %trimr(vinmsg)
033700020506     C                             + ' ' + 'VABHCR'
033800020506     C                   ENDIF
033900020506     C                   ENDIF
034000030407     C*
034100021204     C                   EVAL      VABTC1=%trim(%subst(vindta:263:1))
034200030407     C                   EVAL      VABNOT=%trim(%subst(vindta:264:15))
034300011113     C*
034400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
034500011113     C                   EXSR      CHKIMPDIV
034600010329     C*
034700010329     C* Ebbene...
034800010329     C                   ADD       1             §CTRMO
034900010329     C                   IF        errore <> *zeros
035000010329     C                   ADD       1             §CTRNO
035100010329     C                   EVAL      recko = vindta
035200010329     C                   EXCEPT    rigadett
035300010329     C                   EVAL      vinflg = '2'
035400010329     C                   ELSE
035500010329     C                   ADD       1             §CTROK
035600010329     C                   ENDIF
035700010329     C*
035800010329     C                   ENDSR
035900000801     C*----------------------------------------------------*
036000000801     C*  CONTROLLO NUMERICITA' CAMPI
036100000801     C*----------------------------------------------------*
036200000801     C     CHKNUM        BEGSR
036300000801     C*
036400000801     C                   call(e)   'ISNUMERIC'
036500000801     C                   PARM                    PiStr            30
036600010329     C                   PARM      '.'           PiDecChr          1
036700000801     C                   PARM      *ZEROS        PiVal            30 9
036800000801     C                   PARM      '0'           PiInt             1
036900000801     C                   PARM      '0'           PiNum             1
037000000801     C                   IF        %error
037100000801     C                   EVAL      PiInt=*off
037200000801     C                   ENDIF
037300000801     C*
037400000801     C                   ENDSR
037500000801     C***
037600000801
037700011113
037800011113     C*----------------------------------------------------*
037900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
038000011113     C*----------------------------------------------------*
038100011113     C     CHKIMPDIV     BEGSR
038200011113     C*
038300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
038400011113     C                   Z-ADD     *zeros        wrkDec            9 9
038500011113     C*
038600011113     C* Come prima cosa effettuo considerazioni sulla divisa
038700011113     C                   IF        vabIAS > *zeros
038800011113     C                   IF        vabVAS <> 'EUR'
038900011113     C                   EVAL      vabVAS =  'ITL'
039000011113     C                   ENDIF
039100011113     C                   ENDIF
039200011113     C*
039300011113     C                   IF        vabCAS > *zeros
039400011113     C                   IF        vabVCA <> 'EUR'
039500011113     C                   EVAL      vabVCA =  'ITL'
039600011113     C                   ENDIF
039700011113     C                   ENDIF
039800011113     C*
039900011113     C                   IF        vabVMD > *zeros
040000011113     C                   IF        vabVAD <> 'EUR'
040100011113     C                   EVAL      vabVAD =  'ITL'
040200011113     C                   ENDIF
040300011113     C                   ENDIF
040400011113     C*
040500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
040600011113     C                   Z-ADD     vabIAS        wrkDec
040700011113     C                   IF        wrkDec > *zeros
040800011113     C                   IF        vabVAS = 'ITL'
040900011113     C                   EVAL      vabIAS = *zeros
041000011113     C                   ENDIF
041100011113     C                   ENDIF
041200011113     C*
041300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
041400011113     C                   Z-ADD     vabCAS        wrkDec
041500011113     C                   IF        wrkDec > *zeros
041600011113     C                   IF        vabVCA = 'ITL'
041700011113     C                   EVAL      vabCAS = *zeros
041800011113     C                   ENDIF
041900011113     C                   ENDIF
042000011113     C*
042100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
042200011113     C                   Z-ADD     vabVMD        wrkDec
042300011113     C                   IF        wrkDec > *zeros
042400011113     C                   IF        vabVAD = 'ITL'
042500011113     C                   EVAL      vabVMD = *zeros
042600011113     C                   ENDIF
042700011113     C                   ENDIF
042800011113     C*
042900011113     C                   ENDSR
043000011113     C***
043100000801
043200000801
043300990920      /TITLE Invio dei dati al punto operativo.
043400000613     C     invio         BEGSR
043500990920     C*
043600990920     C                   reset                   dscmz
043700990920     C                   move      vlrpoi        cmzdst
043800990920     C                   eval      cmzfld = vlrfou
043900990920     C                   eval      cmzmbd = vlrhdl
044000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
044100000710     C                   if        prmfir = *blanks
044200021113     C                   eval      cmzfla = 'FIVAB00F'
044300021113     C                   eval      cmzmba = 'FIVAB00F'
044400000710     C                   else
044500000710     C                   eval      cmzfla = prmfir
044600000710     C                   eval      cmzmba = prmfir
044700000710     C                   endif
044800990920     C                   eval      cmznrr = *zeros
044900990920     C                   move      §ctrok        cmznrr
045000021025     C                   eval      cmzlba = vlrfl1
045100990920     C                   call(e)   'TIS711C'
045200990920     C                   parm                    dscmz
045300990921     C                   parm      *blanks       esito
045400990920     C                   if        %error
045500990920     C                             or cmzerr = '1'
045600990921     C                             or esito  = '1'
045700000710     C                   eval      wrkesito = '3'
045800990920     C                   endif
045900990920     C*
046000000613     C                   ENDSR
046100000613     C***
046200990910
046300000613     C     *inzsr        BEGSR
046400990910     C*
046500990910     C     *entry        plist
046600990920     C                   parm                    tivlrds
046700990921     C                   parm      wrkesito      esito
046800000724     C                   parm                    prmlit
046900000710     C                   parm                    prmfir
047000000613     C*
047100000830     C* CALCOLA LA DATA CORRENTE
047200000830     C                   time                    wn14             14 0
047300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
047400000830     C                   z-add     wn8           g08dat
047500000830     C                   z-add     *zeros        g08inv
047600000830     C                   movel     '0'           g08err
047700000830     C                   call      'XSRDA8'
047800000830     C                   parm                    wlbda8
047900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
048000000830     C*
048100000613     C                   ENDSR
048200000613     C***
048300990908
048400010329     Otis7mdyp  E            riepilogo         2
048500990915     O                                              'Upload via Internet'
048600990915     O                                           +1 'Traduzione TIVIN00R -'
048700021113     O                                           +1 'FIVABWWR'
048800990915     O                                           +1 'Testate bolle'
048900990915     O          E            riepilogo   2
049000990915     O                       wrkdata
049100990915     O                       wrkora              +1
049200990915     O                       procname            +1
049300990915     O          E            riepilogo   2
049400990915     O                                              'Cliente..................:'
049500990915     O                       VABCCM        z     +1
049600990915     O          E            riepilogo   2
049700990920     O                                              'Riferimento Strategi.....:'
049800990920     O                       vlrhdl              +1
049900990915     O          E            riepilogo   2
050000990915     O                                              'Giusti...................:'
050100971022     O                       §CTROK        2   +  1
050200990915     O          E            riepilogo   2
050300990915     O                                              'Sbagliati e corretti.....:'
050400971022     O                       §CTRMO        2   +  1
050500990915     O          E            riepilogo   2
050600990915     O                                              'Sbagliati e scartati.....:'
050700971022     O                       §CTRNO        2   +  1
050800000613
050900010329     Otis7mdyps E            testdett          2
051000000613     O                                              'Upload via Internet'
051100000613     O                                           +1 'Traduzione TIVIN00R -'
051200021113     O                                           +1 'FIVABWWR'
051300000613     O                                           +1 'Testate bolle'
051400000616     O          E            testdett    3
051500000613     O                                           +2 'N° rec'
051600000613     O                                           +3 'Anteprima contenuto'
051700000616     O          E            rigadett    2
051800000613     O                       rrnum               +2
051900000621     O                       recko               +3
052000000616     O          E            findett     2
052100000613     O                       wrkdata
052200000613     O                       wrkora              +1
052300000613     O                       procname            +1
052400000616     O          E            findett     2
052500000613     O                                              'Cliente..................:'
052600000613     O                       VABCCM        z     +1
052700000616     O          E            findett     2
052800000613     O                                              'Riferimento Strategi.....:'
052900000613     O                       vlrhdl              +1
053000000616     O          E            findett     2
053100000613     O                                              'Giusti...................:'
053200000613     O                       §CTROK        2   +  1
053300000616     O          E            findett     2
053400000613     O                                              'Sbagliati e corretti.....:'
053500000613     O                       §CTRMO        2   +  1
053600000616     O          E            findett     2
053700000613     O                                              'Sbagliati e scartati.....:'
053800000613     O                       §CTRNO        2   +  1
053900000512** CMD - COMANDI CL
054000011113OVRPRTF FILE(TIS7MDYP) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
054100011113OVRPRTF FILE(TIS7MDYPS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
054200010329DLTOVR FILE(TIS7MDYP TIS7MDYPS) LVL(*)
054300000512
054400000512
