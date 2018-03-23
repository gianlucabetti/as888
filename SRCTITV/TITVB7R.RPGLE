000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800040506     Ftitvb7p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040506     Ftitvb7ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600040506     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600040506     D depspe          s             12    INZ(*blanks)
003700010213     D depdat          s              8  0 INZ(*zeros)
003800010213     D depaas          s              4    INZ(*zeros)
003900040506     D curspe          s             12    INZ(*blanks)
004000010202     D parccm          s              8    INZ(*blanks)
004100010202     D parmbr          s             10    INZ(*blanks)
004200010202     D paropz          s              1    INZ(*blanks)
004300010202     D chkcall         s              1    INZ(*blanks)
004400010213
004500000801     D  Num5_0         s              5  0
004600041025
004700041025     d wpos            s              5  0
004800041025     d wpos1           s              5  0
004900000830
005000000830     D*------------------
005100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200000830     D*------------------
005300000830     D WLBDA8          DS                  INZ
005400000830     D  G08DAT                 1      8  0
005500000830     D  G08INV                 9     16  0
005600000830     D  G08ERR                17     17
005700000830     D  G08TGI                18     22  0
005800000830     D*
005900990908
006000010201
006100010201
006200990915     C                   time                    wrkdata
006300990915     C                   time                    wrkora
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700000724     C*
006800000724     C* SE OCCORRE SPEDIRE IN FILIALE
006900000724     C                   if        vlrpoi <> *zeros
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300000724     C     vlrpoi        chain     azorg01l
007400000724     C                   if        %found
007500000616     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000616     C*
007800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000616     C                   PARM                    depcmd
008200000616     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000616     C*
009700040506     C                   if        not %open(titvb7ps)
009800040506     C                   open      titvb7ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100000613     C*
010200040506     C                   EXSR      RWLEGO                                       LETT/SCR. VAB
010300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010400000613     C*
010500010202     C* Effettuo la chiamata al CLLE preposto
010600040506     C                   call(e)   'TITVVTC'
010700010202     C                   parm                    parccm
010800010202     C                   parm                    parmbr
010900010202     C                   parm      '2'           paropz
011000050201     C*
011100050201     C* Effettuo lancio TISI95 solo x chiusura
011200050201     C                   CLEAR                   TISI95DS
011300050201     C                   EVAL      I95TLA = 'C'
011400050201     C                   CALL      'TISI95R'
011500050201     C                   PARM                    TISI95DS
011600010202     C*
011700040506     C                   if        %open(titvb7ps)
011800000616     C                   except    findett
011900040506     C                   close     titvb7ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800000801     C
012900010201     C                   seton                                        LR
013000000613
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700040506     C                   if        not %open(titvb7p)
013800040506     C                   open      titvb7p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300040506     C                   if        %open(titvb7p)
014400040506     C                   close     titvb7p
014500990914     C                   endif
014600990910     C*
014700000613     C                   ENDSR
014800000613     C***
014900990908
015000000801
015100910830     C*--------------------------------------------------------
015200040506     C* RWLEGO   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015300910830     C*--------------------------------------------------------
015400040506     C     RWLEGO        BEGSR
015500990910     C*
015600990914     C                   if        not %open(tivin00r)
015700990908     C                   open      tivin00r
015800990914     C                   endif
015900021113     C                   if        not %open(fivabwwr)
016000021113     C                   open      fivabwwr
016100990914     C                   endif
016200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300020305     C                   exsr      prevat
016400010201     C*
016500010202     C                   if        chkcall = '0'
016600010202     C*
016700021113     C                   if        not %open(fivatwwr)
016800021113     C                   open      fivatwwr
016900010201     C                   endif
017000990910     C*
017100010201     C                   clear                   §CTROKVB          5 0
017200020305     C                   clear                   §CTROKVT          5 0
017300000801     C                   clear                   §CTRMO            5 0
017400000801     C                   clear                   §CTRNO            5 0
017500990910     C*
017600921023     C                   DO        *HIVAL
017700990913     C*
017800990915     C                   READ      tivin00r                               70
017900000905     C                   if        vindta > *blanks
018000000613     C                   add       1             rrnum
018100000801     C*
018200000801     C                   if        *in70 = *off
018300000801     C                             and
018400000801     C                             (vinflg = *blanks
018500000801     C                              or vinflg = '0'
018600000801     C                              or vinflg = '2')
018700000801     C*
018800000801     C                   clear                   vinmsg
018900000801     C                   eval      vinflg = '1'
019000010305     C*
019100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
019200040506     C                   EVAL      PiStr=%trim(%subst(vindta:86:12))
019300020305     C                   MOVEL(p)  PiStr         curspe
019400010305     C*
019500010305     C                   if        depspe = *blanks                             => 1° giro
019600010305     C                   eval      depspe = curspe                              => memorizz. spediz
019700020305     C                   exsr      impvab
019800020305     C                   exsr      wrivat                                       => carico VAT
019900010305     C                   else
020000010305     C                   if        curspe <> depspe                             => rottura di spediz
020100010305     C                   eval      depspe = curspe                              => memorizz. spediz
020200020305     C                   exsr      wrivab
020300020305     C                   exsr      impvab
020400020305     C                   exsr      wrivat                                       => carico VAT
020500020305     C                   else                                                   => x stessa spediz
020600020305     C                   exsr      impvab
020700020305     C                   exsr      wrivat                                       => carico VAT
020800010305     C                   endif
020900010305     C                   endif
021000010305     C                   endif
021100000905     C*
021200000905     C                   else
021300000905     C                   eval      vinflg = '1'
021400000905     C                   endif
021500000905     C*
021600000905     C  N70              update    tivin000
021700000905     C*
021800991022     C  N70              ENDdo
021900010202     C*
022000020305     C* Scarico i VAB rimasti "in sospeso"
022100020305     C                   exsr      wrivab
022200010202     C*
022300010202     C                   endif
022400990910
022500990910     C* Se non ci sono record con errori ...
022600000710     C                   if        §ctrno = 0
022700990910     C* ... restituisco esito OK.
022800990921     C                   eval      wrkesito = '0'
022900990910     C                   else
023000010201     C                   if        §ctrokvb > 0
023100990921     C                   eval      wrkesito = '1'
023200000710     C                   else
023300000710     C                   eval      wrkesito = '2'
023400990910     C                   endif
023500000710     C                   endif
023600990910     C*
023700990914     C                   if        %open(tivin00r)
023800990908     C                   close     tivin00r
023900990914     C                   endif
024000021113     C                   if        %open(fivabwwr)
024100021113     C                   close     fivabwwr
024200990914     C                   endif
024300021113     C                   if        %open(fivatwwr)
024400021113     C                   close     fivatwwr
024500010201     C                   endif
024600990910     C*
024700010201     C                   if        §ctrokvb > 0
024800000724     C                             and vlrpoi <> *zeros
024900010202     C                   exsr      invio
025000990920     C                   endif
025100990920     C*
025200910830     C                   ENDSR
025300000613     C***
025400010305
025500010305     C*----------------------------------------------------*
025600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025700010305     C*----------------------------------------------------*
025800020305     C     WRIVAB        BEGSR
025900010305     C*
026000021113     C                   write     fivab000                                     => scarico il VAB
026100010305     C*
026200010305     C                   ENDSR
026300990920
026400000801     C*----------------------------------------------------*
026500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026600000801     C*----------------------------------------------------*
026700010201     C     INZVAR        BEGSR
026800000801     C*
026900010201     C                   Z-ADD     *zeros        Num5_0
027000000801     C*
027100000801     C                   ENDSR
027200000801     C*----------------------------------------------------*
027300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027400000801     C*----------------------------------------------------*
027500000801     C     DEFCAM        BEGSR
027600000801     C*
027700021113     C                   CLEAR                   FIVAB000
027800020619     C* Imposto i valori di default...
027900040506     C                   Z-ADD     0892696       VABCCM
028000040506     C                   Z-ADD     0892696       VATCCM
028100020619     C                   Z-ADD     089           VABLNP
028200020619     C                   Z-ADD     089           VATLNP
028300040506     C                   Z-ADD     001           VABCTR
028400040506     C                   MOVEL     'C'           VABTSP
028500020305     C                   MOVEL     '7Q'          VABCTM
028600050316     C                   EVAL      VABRMO='LEGO'
028700020305     C                   MOVEL     'E'           VATTRC
028800020619     C* ... e poi verifico se sono stati passati come parametri
028900020619     C                   IF        vlrppt > *blanks
029000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029200020619     C                   EXSR      CHKNUM
029300020619     C                   IF        PiInt=*on
029400020619     C                   Z-ADD     PiVal         VABCCM
029500020619     C                   Z-ADD     PiVal         VATCCM
029600020619     C                   ENDIF
029700040506     C                   ENDIF
029800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030000020619     C                   EXSR      CHKNUM
030100020619     C                   IF        PiInt=*on
030200020619     C                   Z-ADD     PiVal         VABLNP
030300020619     C                   Z-ADD     PiVal         VATLNP
030400040506     C                   ENDIF
030500020619     C                   ENDIF
030600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030800020619     C                   EXSR      CHKNUM
030900020619     C                   IF        PiInt=*on
031000020619     C                   Z-ADD     PiVal         VABCTR
031100040506     C                   ENDIF
031200020619     C                   ENDIF
031300020619     C                   ENDIF
031400000801     C*
031500000801     C                   ENDSR
031600000801     C*----------------------------------------------------*
031700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031800000801     C*----------------------------------------------------*
031900010201     C     IMPVAB        BEGSR
032000000801     C*
032100020305     C                   EXSR      INZVAR
032200020305     C                   EXSR      DEFCAM
032300010305     C*
032400000801     C                   Z-ADD     *zeros        errore            1 0
032500000830     C                   MOVEL     datcor        VABAAS
032600020305     C                   MOVEL     datcor        VATAAS
032700021113     C                   MOVE(P)   vlrpoi        VABFGS
032800021113     C                   MOVE(P)   vlrpoi        VATFGS
032900010213     C                   MOVEL     VABAAS        depaas
033000000830     C                   MOVE      datcor        VABMGS
033100040506     C                   MOVEL     *blanks       wRSD            200
033200040506     C                   EVAL      wRSD=%trim(%subst(vindta:098:50))+' '+
033300040506     C                                  %trim(%subst(vindta:248:50))+' '+
033400040506     C                                  %trim(%subst(vindta:298:50))+' '+
033500040506     C                                  %trim(%subst(vindta:348:50))
033600041025      * prima della ragione sociale del destinatario viene passato un codice.... è da togliere
033700041025     c                   If        %subst(wrsd:1:1) >= '0' and
033800041025     c                             %subst(wrsd:1:1) <= '9'
033900041025     c                   Eval      wpos = %scan(' ':wrsd)
034000041025     c                   Eval      wpos1 = wpos - 1
034100041025     c                   If        %subst(wrsd:wpos1:1) >= '0' and
034200041025     c                             %subst(wrsd:wpos1:1) <= '9'
034300041025     c                   Eval      wrsd = %subst(wrsd:wpos)
034400041025     c                   EndIf
034500041025     c                   EndIf
034600041025
034700040506     C                   EVAL      VABRSD=%trim(%subst(wRSD:1:35))
034800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035000020117     C* ==
035100040506     C                   EVAL      VABRD2=%trim(%subst(wRSD:1+35:35))
035200040506     C                   EVAL      VABIND=%trim(%subst(vindta:198:50))
035300040506     C                   EVAL      VABLOD=%trim(%subst(vindta:398:50))
035400040506     C                   EVAL      VABNZD=%trim(%subst(vindta:473:5))
035500020918     C                   IF        %trim(VABNZD) = 'IT'
035600020918     C                   EVAL      VABNZD=*blanks
035700020918     C                   ENDIF
035800040506     C                   EVAL      VABRMA=%trim(%subst(vindta:86:12))
035900040506     C                   EVAL      VABNAS=%trim(%subst(vindta:555:25))
036000040506     C                   EVAL      VABNOT=%trim(%subst(vindta:632:35))
036100040506     C                   EVAL      VABNT2=%trim(%subst(vindta:632+35:35))
036200040506     C*
036300040506     C* Verifico se richiesto appuntamento
036400040506     C                   IF        %subst(vindta:728:12) <> *blanks
036500040506     C                   EVAL      VABTC1 = 'A'
036600040506     C                   ENDIF
036700040506     C* CAD
036800040506     C                   EVAL      PiStr=%trim(%subst(vindta:463:10))
036900010201     C                   EXSR      CHKNUM
037000010201     C                   IF        PiInt=*on
037100010201     C                   Z-ADD     PiVal         Num5_0
037200040506     C                   MOVEL(P)  Num5_0        VABCAD
037300010201     C                   ELSE
037400040506     C                   ADD       1             errore
037500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037600040506     C                             + ' ' + 'VABCAD'
037700010201     C                   ENDIF
037800040506     C* Reperisco la provincia dal CAP e dalla località
037900040506     C                   IF        VABCAD <> *blanks
038000040506     C                   CLEAR                   TISI95DS
038100040506     C                   EVAL      I95TCN = '3'
038200040506     C                   Z-ADD     datcor        I95DAT
038300040506     C                   EVAL      I95CAP = VABCAD
038400040506     C                   EVAL      I95LOC = VABLOD
038500040506     C                   CALL      'TISI95R'
038600040506     C                   PARM                    TISI95DS
038700040506     C                   EVAL      VABPRD = O95PRV
038800040506     C                   ENDIF
038900040506     C* NCL
039000040506     C                   EVAL      PiStr=%trim(%subst(vindta:551:4))
039100040506     C                   EXSR      CHKNUM
039200040506     C                   IF        PiInt=*on
039300040506     C                   Z-ADD     PiVal         VABNCL
039400040506     C                   ELSE
039500040506     C                   ADD       1             errore
039600040506     C                   EVAL      vinmsg = %trimr(vinmsg)
039700040506     C                             + ' ' + 'VABNCL'
039800040506     C                   ENDIF
039900040506     C* PKB
040000040506     C                   EVAL      PiStr=%trim(%subst(vindta:580:10))
040100010201     C                   EXSR      CHKNUM
040200010201     C                   IF        PiNum=*on
040300011115     C                   Z-ADD     PiVal         VABPKB
040400010201     C                   ELSE
040500010201     C                   ADD       1             errore
040600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040700010201     C                             + ' ' + 'VABPKB'
040800010201     C                   ENDIF
040900040506     C* VLB
041000040506     C                   EVAL      PiStr=%trim(%subst(vindta:600:10))
041100040506     C                   EXSR      CHKNUM
041200040506     C                   IF        PiNum=*on
041300040506     C                   Z-ADD     PiVal         VABVLB
041400040506     C                   ELSE
041500040506     C                   ADD       1             errore
041600040506     C                   EVAL      vinmsg = %trimr(vinmsg)
041700040506     C                             + ' ' + 'VABVLB'
041800040506     C                   ENDIF
041900040506     C* NSP/RMN
042000040506     C                   EVAL      PiStr=%trim(%subst(vindta:86:12))
042100010201     C                   EXSR      CHKNUM
042200010201     C                   IF        PiInt=*on
042300040506     C                   Z-ADD     PiVal         VABNSP
042400040506     C                   Z-ADD     PiVal         VATNSP
042500040506     C                   Z-ADD     PiVal         VABRMN
042600010201     C                   ELSE
042700010201     C                   ADD       1             errore
042800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
042900040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
043000010201     C                   ENDIF
043100010205     C*
043200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043300010205     C                   IF        VABCAS > *zeros
043400010205     C                   EVAL      VABCBO = '4'
043500010205     C                   ELSE
043600010205     C                   EVAL      VABCBO = '1'
043700010205     C                   ENDIF
043800020305     C*
043900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044000011113     C                   EXSR      CHKIMPDIV
044100010202     C*
044200000801     C* Ebbene...
044300000801     C                   ADD       1             §CTRMO
044400010201     C                   IF        errore <> *zeros
044500000801     C                   ADD       1             §CTRNO
044600000801     C                   EVAL      recko = vindta
044700000801     C                   EXCEPT    rigadett
044800000801     C                   EVAL      vinflg = '2'
044900000801     C                   ELSE
045000010201     C                   ADD       1             §CTROKVB
045100000801     C                   ENDIF
045200000801     C*
045300000801     C                   ENDSR
045400010201     C*----------------------------------------------------*
045500021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
045600010201     C*----------------------------------------------------*
045700020305     C     WRIVAT        BEGSR
045800010201     C*
045900021113     C* Valorizzo l buffer di scrittura del FIVAT
046000040506     C                   EVAL      VATNOT = %subst(vindta:620:12)
046100021113     C                   write     FIVAT000
046200040506     C                   IF        %subst(vindta:728:12) <> *blanks
046300040506     C                   EVAL      VATTRC = 'B'
046400040506     C                   EVAL      VATNOT=%subst(vindta:728:12)
046500040224     C                   write     FIVAT000
046600040506     C                   ENDIF
046700010201     C*
046800010201     C                   ENDSR
046900010202     C*----------------------------------------------------*
047000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047100010202     C*----------------------------------------------------*
047200020305     C     PREVAT        BEGSR
047300010202     C*
047400021113     C* Compongo il nome del membro da dare al FIVATWWR
047500010202     C                   eval      parmbr = vlrhdl
047600010202     C                   movel     'M'           parmbr
047700040506     C                   eval      parccm = '00892696'
047800010202     C                   eval      paropz = '1'
047900010202     C* Effettuo la chiamata al CLLE preposto
048000040506     C                   call(e)   'TITVVTC'
048100010202     C                   parm                    parccm
048200010202     C                   parm                    parmbr
048300010202     C                   parm                    paropz
048400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048500010202     C                   if        %error
048600010202     C                   movel     '1'           chkcall
048700010202     C                   else
048800010202     C                   movel     '0'           chkcall
048900010202     C                   endif
049000010202     C*
049100010202     C                   ENDSR
049200000801     C*----------------------------------------------------*
049300000801     C*  CONTROLLO NUMERICITA' CAMPI
049400000801     C*----------------------------------------------------*
049500000801     C     CHKNUM        BEGSR
049600000801     C*
049700000801     C                   call(e)   'ISNUMERIC'
049800000801     C                   PARM                    PiStr            30
049900020619     C                   PARM      '.'           PiDecChr          1
050000000801     C                   PARM      *ZEROS        PiVal            30 9
050100000801     C                   PARM      '0'           PiInt             1
050200000801     C                   PARM      '0'           PiNum             1
050300000801     C                   IF        %error
050400000801     C                   EVAL      PiInt=*off
050500000801     C                   ENDIF
050600000801     C*
050700000801     C                   ENDSR
050800000801     C***
050900000801
051000011113
051100011113     C*----------------------------------------------------*
051200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051300011113     C*----------------------------------------------------*
051400011113     C     CHKIMPDIV     BEGSR
051500011113     C*
051600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051700011113     C                   Z-ADD     *zeros        wrkDec            9 9
051800011113     C*
051900011113     C* Come prima cosa effettuo considerazioni sulla divisa
052000011113     C                   IF        vabIAS > *zeros
052100011113     C                   IF        vabVAS <> 'EUR'
052200011113     C                   EVAL      vabVAS =  'ITL'
052300011113     C                   ENDIF
052400011113     C                   ENDIF
052500011113     C*
052600011113     C                   IF        vabCAS > *zeros
052700011113     C                   IF        vabVCA <> 'EUR'
052800011113     C                   EVAL      vabVCA =  'ITL'
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C                   IF        vabVMD > *zeros
053300020305     C                   IF        vabVAD <> 'EUR'
053400011113     C                   EVAL      vabVAD =  'ITL'
053500011113     C                   ENDIF
053600011113     C                   ENDIF
053700011113     C*
053800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053900011113     C                   Z-ADD     vabIAS        wrkDec
054000011113     C                   IF        wrkDec > *zeros
054100011113     C                   IF        vabVAS = 'ITL'
054200011113     C                   EVAL      vabIAS = *zeros
054300011113     C                   ENDIF
054400011113     C                   ENDIF
054500011113     C*
054600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054700011113     C                   Z-ADD     vabCAS        wrkDec
054800011113     C                   IF        wrkDec > *zeros
054900011113     C                   IF        vabVCA = 'ITL'
055000011113     C                   EVAL      vabCAS = *zeros
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055500011113     C                   Z-ADD     vabVMD        wrkDec
055600011113     C                   IF        wrkDec > *zeros
055700011113     C                   IF        vabVAD = 'ITL'
055800011113     C                   EVAL      vabVMD = *zeros
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C                   ENDSR
056300011113     C***
056400011113
056500011113
056600000801
056700000801
056800990920      /TITLE Invio dei dati al punto operativo.
056900010202     C     invio         BEGSR
057000990920     C*
057100021113     C* 1° invio FIVAT
057200010201     C                   reset                   dscmz
057300010201     C                   move      vlrpoi        cmzdst
057400021113     C                   eval      cmzfld = 'FIVATWWR'
057500010201     C                   eval      cmzmbd = vlrhdl
057600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057700021009     C***                if        prmfir = *blanks
057800021113     C                   eval      cmzfla = 'FIVAT00F'
057900021113     C                   eval      cmzmba = 'FIVAT00F'
058000021009     C***                else
058100021009     C***                eval      cmzfla = prmfir
058200021009     C***                eval      cmzmba = prmfir
058300021009     C***                endif
058400010201     C                   eval      cmznrr = *zeros
058500020305     C                   move      §ctrokvt      cmznrr
058600021018     C                   eval      cmzlba = vlrfl1
058700010201     C                   call(e)   'TIS711C'
058800010201     C                   parm                    dscmz
058900010201     C                   parm      *blanks       esito
059000010205     C                   if        %error
059100010205     C                             or cmzerr = '1'
059200010205     C                             or esito  = '1'
059300010205     C                   eval      wrkesito = '3'
059400010205     C                   else
059500010201     C*
059600021113     C* 2° invio FIVAB
059700010201     C                   reset                   dscmz
059800010201     C                   move      vlrpoi        cmzdst
059900010201     C                   eval      cmzfld = vlrfou
060000010201     C                   eval      cmzmbd = vlrhdl
060100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060200021009     C***                if        prmfir = *blanks
060300021113     C                   eval      cmzfla = 'FIVAB00F'
060400021113     C                   eval      cmzmba = 'FIVAB00F'
060500021009     C***                else
060600021009     C***                eval      cmzfla = prmfir
060700021009     C***                eval      cmzmba = prmfir
060800021009     C***                endif
060900010201     C                   eval      cmznrr = *zeros
061000010201     C                   move      §ctrokvb      cmznrr
061100021018     C                   eval      cmzlba = vlrfl1
061200010201     C                   call(e)   'TIS711C'
061300010201     C                   parm                    dscmz
061400010201     C                   parm      *blanks       esito
061500010201     C                   if        %error
061600010201     C                             or cmzerr = '1'
061700010201     C                             or esito  = '1'
061800010201     C                   eval      wrkesito = '3'
061900010201     C                   endif
062000010205     C                   endif
062100990920     C*
062200000613     C                   ENDSR
062300000613     C***
062400070411
062500070411     C     *pssr         BEGSR
062600070411     C*
062700070411     C                   if        %open(tivin00r)
062800070411     C                   close     tivin00r
062900070411     C                   endif
063000070411     C                   if        %open(fivabwwr)
063100070411     C                   close     fivabwwr
063200070411     C                   endif
063300070411     C                   if        %open(fivatwwr)
063400070411     C                   close     fivatwwr
063500070411     C                   endif
063600070411     C*
063700070411     C* Effettuo la chiamata al CLLE preposto
063800070411     C                   call(e)   'TITVVTC'
063900070411     C                   parm                    parccm
064000070411     C                   parm                    parmbr
064100070411     C                   parm      '2'           paropz
064200070411     C*
064300070411     C                   eval      wrkesito = '2'
064400070411     C*
064500070411     C                   seton                                        LR
064600070411     C*
064700070411     C                   ENDSR     '*CANCL'
064800070411     C***
064900070411
065000990910
065100000613     C     *inzsr        BEGSR
065200990910     C*
065300990910     C     *entry        plist
065400990920     C                   parm                    tivlrds
065500990921     C                   parm      wrkesito      esito
065600000724     C                   parm                    prmlit
065700000710     C                   parm                    prmfir
065800000613     C*
065900000830     C* CALCOLA LA DATA CORRENTE
066000000830     C                   time                    wn14             14 0
066100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066200000830     C                   z-add     wn8           g08dat
066300000830     C                   z-add     *zeros        g08inv
066400000830     C                   movel     '0'           g08err
066500000830     C                   call      'XSRDA8'
066600000830     C                   parm                    wlbda8
066700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066800000830     C*
066900000613     C                   ENDSR
067000000613     C***
067100990908
067200040506     Otitvb7p   E            riepilogo         2
067300990915     O                                              'Upload via Internet'
067400990915     O                                           +1 'Traduzione TIVIN00R -'
067500021113     O                                           +1 'FIVABWWR/FIVATWWR'
067600010201     O                                           +1 'Report testate bolle'
067700990915     O          E            riepilogo   2
067800990915     O                       wrkdata
067900990915     O                       wrkora              +1
068000990915     O                       procname            +1
068100990915     O          E            riepilogo   2
068200990915     O                                              'Cliente..................:'
068300990915     O                       VABCCM        z     +1
068400990915     O          E            riepilogo   2
068500990920     O                                              'Riferimento Strategi.....:'
068600990920     O                       vlrhdl              +1
068700990915     O          E            riepilogo   2
068800990915     O                                              'Giusti...................:'
068900010201     O                       §CTROKVB      2   +  1
069000990915     O          E            riepilogo   2
069100990915     O                                              'Sbagliati e corretti.....:'
069200971022     O                       §CTRMO        2   +  1
069300990915     O          E            riepilogo   2
069400990915     O                                              'Sbagliati e scartati.....:'
069500971022     O                       §CTRNO        2   +  1
069600000613
069700040506     Otitvb7ps  E            testdett          2
069800000613     O                                              'Upload via Internet'
069900000613     O                                           +1 'Traduzione TIVIN00R -'
070000021113     O                                           +1 'FIVABWWR/FIVATWWR'
070100010201     O                                           +1 'Report testate bolle'
070200000616     O          E            testdett    3
070300000613     O                                           +2 'N° rec'
070400000613     O                                           +3 'Anteprima contenuto'
070500000616     O          E            rigadett    2
070600000613     O                       rrnum               +2
070700000621     O                       recko               +3
070800000616     O          E            findett     2
070900000613     O                       wrkdata
071000000613     O                       wrkora              +1
071100000613     O                       procname            +1
071200000616     O          E            findett     2
071300000613     O                                              'Cliente..................:'
071400000613     O                       VABCCM        z     +1
071500000616     O          E            findett     2
071600000613     O                                              'Riferimento Strategi.....:'
071700000613     O                       vlrhdl              +1
071800000616     O          E            findett     2
071900000613     O                                              'Giusti...................:'
072000010201     O                       §CTROKVB      2   +  1
072100000616     O          E            findett     2
072200000613     O                                              'Sbagliati e corretti.....:'
072300000613     O                       §CTRMO        2   +  1
072400000616     O          E            findett     2
072500000613     O                                              'Sbagliati e scartati.....:'
072600000613     O                       §CTRNO        2   +  1
072700000512** CMD - COMANDI CL
072800040506OVRPRTF FILE(TITVB7P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
072900040506OVRPRTF FILE(TITVB7PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
073000040506DLTOVR FILE(TITVB7P TITVB7PS) LVL(*)
073100000512
073200000512
