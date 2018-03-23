000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070213     Ftitvw9p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070213     Ftitvw9ps  O    f  198        PRINTER usropn
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
002600060307     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000081114     D curSped         s              6    INZ(*blanks)
004100081114     D depSped         s              6    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100070103     D*------------------
005200070103     D* DS REPERIMENTO NUMERATORE
005300070103     D*------------------
005400070103     D trul33ds      e ds                  inz
005500070103     D*------------------
005600070103     D* DS ARCHITETTURA
005700070103     D*------------------
005800070103     D kpjba         e ds                  inz
005900000830     D*
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
009700070213     C                   if        not %open(titvw9ps)
009800070213     C                   open      titvw9ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100000613     C*
010200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
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
011700070213     C                   if        %open(titvw9ps)
011800000616     C                   except    findett
011900070213     C                   close     titvw9ps
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
013700070213     C                   if        not %open(titvw9p)
013800070213     C                   open      titvw9p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300070213     C                   if        %open(titvw9p)
014400070213     C                   close     titvw9p
014500990914     C                   endif
014600990910     C*
014700000613     C                   ENDSR
014800000613     C***
014900990908
015000000801
015100910830     C*--------------------------------------------------------
015200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015300910830     C*--------------------------------------------------------
015400040526     C     RWFILE        BEGSR
015500990910     C*
015600990914     C                   if        not %open(tivin00r)
015700990908     C                   open      tivin00r
015800990914     C                   endif
015900021113     C                   if        not %open(fivabwwr)
016000021113     C                   open      fivabwwr
016100990914     C                   endif
016200070103     C*
016300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016400020305     C                   exsr      prevat
016500010201     C*
016600010202     C                   if        chkcall = '0'
016700010202     C*
016800021113     C                   if        not %open(fivatwwr)
016900021113     C                   open      fivatwwr
017000010201     C                   endif
017100990910     C*
017200010201     C                   clear                   §CTROKVB          5 0
017300020305     C                   clear                   §CTROKVT          5 0
017400000801     C                   clear                   §CTRMO            5 0
017500000801     C                   clear                   §CTRNO            5 0
017600990910     C*
017700921023     C                   DO        *HIVAL
017800990913     C*
017900990915     C                   READ      tivin00r                               70
018000050627     C                   if        vindta > *blanks
018100000613     C                   add       1             rrnum
018200000801     C*
018300000801     C                   if        *in70 = *off
018400000801     C                             and
018500000801     C                             (vinflg = *blanks
018600000801     C                              or vinflg = '0'
018700000801     C                              or vinflg = '2')
018800000801     C*
018900000801     C                   clear                   vinmsg
019000000801     C                   eval      vinflg = '1'
019100070103     C*
019200070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
019300081114     C                   eval      curSped = %subst(vindta:163:6)
019400070213     C                   if        curSped <> depSped
019500070213     C* Se prima bolla => importo bolla corrente
019600070213     C                   if        §CTRMO = *zeros
019700070213     C                   exsr      inzvar
019800070213     C                   exsr      defcam
019900070213     C                   exsr      repnum
020000070213     C                   exsr      impvab                                       => carico  VAB
020100070213     C                   else
020200070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
020300070213     C                   exsr      wrivab                                       => scarico VAB
020400070213     C                   exsr      inzvar
020500070213     C                   exsr      defcam
020600070213     C                   exsr      repnum
020700070213     C                   exsr      impvab                                       => carico  VAB
020800070213     C                   endif
020900070213     C                   exsr      exevate                                      => write VAT-E
021000070103     C* Salvo il raggruppamento spedizione cliente corrente
021100070103     C                   eval      depSped = curSped
021200070213     C*
021300070213     C* Se collo successivo x stessa bolla
021400070213     C                   else
021500070213     C                   exsr      impvab                                       => carico  VAB
021600070213     C                   exsr      exevate                                      => write VAT-E
021700070103     C                   endif
021800000905     C*
021900000905     C                   else
022000000905     C                   eval      vinflg = '1'
022100050628     C                   endif
022200000905     C                   endif
022300000905     C*
022400000905     C  N70              update    tivin000
022500000905     C*
022600991022     C  N70              ENDdo
022700070213     C*
022800070213     C* Scarico testata bolla rimasta in sospesa
022900070213     C                   exsr      wrivab                                       => scarico VAB
023000010202     C*
023100010202     C                   endif
023200990910
023300990910     C* Se non ci sono record con errori ...
023400000710     C                   if        §ctrno = 0
023500990910     C* ... restituisco esito OK.
023600990921     C                   eval      wrkesito = '0'
023700990910     C                   else
023800010201     C                   if        §ctrokvb > 0
023900990921     C                   eval      wrkesito = '1'
024000000710     C                   else
024100000710     C                   eval      wrkesito = '2'
024200990910     C                   endif
024300000710     C                   endif
024400990910     C*
024500990914     C                   if        %open(tivin00r)
024600990908     C                   close     tivin00r
024700990914     C                   endif
024800021113     C                   if        %open(fivabwwr)
024900021113     C                   close     fivabwwr
025000990914     C                   endif
025100021113     C                   if        %open(fivatwwr)
025200021113     C                   close     fivatwwr
025300010201     C                   endif
025400990910     C*
025500010201     C                   if        §ctrokvb > 0
025600000724     C                             and vlrpoi <> *zeros
025700010202     C                   exsr      invio
025800990920     C                   endif
025900990920     C*
026000910830     C                   ENDSR
026100000613     C***
026200010305
026300010305     C*----------------------------------------------------*
026400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026500010305     C*----------------------------------------------------*
026600020305     C     WRIVAB        BEGSR
026700010305     C*
026800060225     C* Quindi scarico il buffer del file d testata
026900021113     C                   write     fivab000                                     => scarico il VAB
027000010305     C*
027100010305     C                   ENDSR
027200990920
027300000801     C*----------------------------------------------------*
027400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500000801     C*----------------------------------------------------*
027600010201     C     INZVAR        BEGSR
027700000801     C*
027800070213     C                   CLEAR                   FIVAB000
027900070213     C                   CLEAR                   FIVAT000
028000070213     C*
028100040802     C                   Z-ADD     *zeros        Num5_0            5 0
028200040802     C                   MOVEL     '0'           FlgCAS            1
028300000801     C*
028400000801     C                   ENDSR
028500000801     C*----------------------------------------------------*
028600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028700000801     C*----------------------------------------------------*
028800000801     C     DEFCAM        BEGSR
028900000801     C*
029000020619     C* Imposto i valori di default...
029100070213     C                   Z-ADD     0951267       VABCCM
029200070213     C                   Z-ADD     0951267       VATCCM
029300070213     C                   Z-ADD     050           VABLNP
029400070213     C                   Z-ADD     050           VATLNP
029500070213     C                   Z-ADD     101           VABCTR
029600070103     C                   MOVEL     '7Q'          VABCTM
029700040823     C                   MOVEL     '1'           VABCBO
029800070213     C                   MOVEL     'C'           VABTSP
029900020619     C* ... e poi verifico se sono stati passati come parametri
030000020619     C                   IF        vlrppt > *blanks
030100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
030200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030300020619     C                   EXSR      CHKNUM
030400020619     C                   IF        PiInt=*on
030500020619     C                   Z-ADD     PiVal         VABCCM
030600020619     C                   Z-ADD     PiVal         VATCCM
030700020619     C                   ENDIF
030800040506     C                   ENDIF
030900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
031000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031100020619     C                   EXSR      CHKNUM
031200020619     C                   IF        PiInt=*on
031300020619     C                   Z-ADD     PiVal         VABLNP
031400020619     C                   Z-ADD     PiVal         VATLNP
031500040506     C                   ENDIF
031600020619     C                   ENDIF
031700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031900020619     C                   EXSR      CHKNUM
032000020619     C                   IF        PiInt=*on
032100020619     C                   Z-ADD     PiVal         VABCTR
032200040506     C                   ENDIF
032300020619     C                   ENDIF
032400020619     C                   ENDIF
032500000801     C*
032600000801     C                   ENDSR
032700000801     C*----------------------------------------------------*
032800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032900000801     C*----------------------------------------------------*
033000040823     C     IMPVAB        BEGSR
033100010305     C*
033200000801     C                   Z-ADD     *zeros        errore            1 0
033300000830     C                   MOVEL     datcor        VABAAS
033400020305     C                   MOVEL     datcor        VATAAS
033500040526     C                   MOVE      datcor        VABMGS
033600040823     C                   MOVE(P)   vlrpoi        VABFGS
033700040823     C                   MOVE(P)   vlrpoi        VATFGS
033800050628     C*
033900091005     C*
034000091005     C                   EVAL      VABRSD=%trim(%subst(vindta:24:35))
034100091005     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034200091005     C     '@':'A'       XLATE     VABRSD        VABRSD
034300091005     C* ==
034400091005     C                   EVAL      VABRD2=%trim(%subst(vindta:24+35:43-35))
034500091005     C                   EVAL      VABIND=%trim(%subst(vindta:67:50))
034600091005     C                   EVAL      VABLOD=%trim(%subst(vindta:117:36))
034700091005     C                   EVAL      VABPRD=%trim(%subst(vindta:158:2))
034800091005     C                   EVAL      VABRMA=%trim(%subst(vindta:163:6))
034900091005     C                   EVAL      VABTSP=%trim(%subst(vindta:200:1))
035000091005     C                   EVAL      VABNOT=%trim(%subst(vindta:203:35))
035100091005     C                   EVAL      VABNT2=%trim(%subst(vindta:203+35:255-35))
035200091005     C*
035300091005     C* Forzature codice cliente e tariffa
035400091005     C                   SELECT
035500091005     C                   WHEN      %subst(vindta:201:2) = 'I '
035600091005     C                   EVAL      VABCCM = 0951267
035700091005     C                   EVAL      VATCCM = 0951267
035800091005     C                   EVAL      VABCTR = 101
035900091005     C                   WHEN      %subst(vindta:201:2) = 'J '
036000091005     C                   EVAL      VABCCM = 0951586
036100091005     C                   EVAL      VATCCM = 0951586
036200091005     C                   EVAL      VABCTR = 101
036300091005     C                   WHEN      %subst(vindta:201:2) = 'JQ'
036400091005     C                   EVAL      VABCCM = 0951597
036500091005     C                   EVAL      VATCCM = 0951597
036600091005     C                   EVAL      VABCTR = 000
036700091005     C                   ENDSL
036800050628     C* CAP
036900070213     C                   EVAL      PiStr=%trim(%subst(vindta:153:5))
037000010201     C                   EXSR      CHKNUM
037100010201     C                   IF        PiInt=*on
037200010201     C                   Z-ADD     PiVal         Num5_0
037300040506     C                   MOVEL(P)  Num5_0        VABCAD
037400010201     C                   ELSE
037500040506     C                   ADD       1             errore
037600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037700040506     C                             + ' ' + 'VABCAD'
037800010201     C                   ENDIF
037900040506     C* Reperisco la provincia dal CAP e dalla località
038000040526     C                   IF        VABCAD <> *blanks AND
038100040526     C                             VABPRD  = *blanks
038200040506     C                   CLEAR                   TISI95DS
038300040506     C                   EVAL      I95TCN = '3'
038400040506     C                   Z-ADD     datcor        I95DAT
038500040506     C                   EVAL      I95CAP = VABCAD
038600040506     C                   EVAL      I95LOC = VABLOD
038700050627     C                   EVAL      I95NAR = VABNZD
038800040506     C                   CALL      'TISI95R'
038900040506     C                   PARM                    TISI95DS
039000040506     C                   EVAL      VABPRD = O95PRV
039100040506     C                   ENDIF
039200060307     C* NCL
039300070213     C                   EVAL      VABNCL = VABNCL + 1
039400040506     C* PKB
039500070213     C                   EVAL      PiStr=%trim(%subst(vindta:190:5))
039600010201     C                   EXSR      CHKNUM
039700010201     C                   IF        PiNum=*on
039800070213     C                   ADD       PiVal         VABPKB
039900010201     C                   ELSE
040000010201     C                   ADD       1             errore
040100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040200010201     C                             + ' ' + 'VABPKB'
040300010201     C                   ENDIF
040400070213     C* VLB
040500070213     C                   EVAL      PiStr=%trim(%subst(vindta:195:5))
040600070213     C                   EXSR      CHKNUM
040700070213     C                   IF        PiNum=*on
040800070213     C                   ADD       PiVal         VABVLB
040900070213     C                   ELSE
041000070213     C                   ADD       1             errore
041100070213     C                   EVAL      vinmsg = %trimr(vinmsg)
041200070213     C                             + ' ' + 'VABVLB'
041300070213     C                   ENDIF
041400070103     C* RMN
041500091006     C                   IF        %subst(vindta:163:1) = '\'
041600091006     C                   EVAL      PiStr='0'+%trim(%subst(vindta:163+1:6-1))
041700091006     C                   ELSE
041800091006     C                   EVAL      PiStr=%trim(%subst(vindta:163:6))
041900091006     C                   ENDIF
042000050928     C                   EXSR      CHKNUM
042100050928     C                   IF        PiInt=*on
042200050928     C                   Z-ADD     PiVal         VABRMN
042300050928     C                   ELSE
042400050928     C                   ADD       1             errore
042500050928     C                   EVAL      vinmsg = %trimr(vinmsg)
042600070103     C                             + ' ' + 'VABRMN'
042700050928     C                   ENDIF
042800010205     C*
042900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043000040802     C                   IF        FlgCAS <> '0'
043100070102     C                   IF        VABCBO = '1'
043200010205     C                   EVAL      VABCBO = '4'
043300010205     C                   ELSE
043400070102     C                   EVAL      VABCBO = '6'
043500070102     C                   ENDIF
043600010205     C                   ENDIF
043700020305     C*
043800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043900011113     C                   EXSR      CHKIMPDIV
044000010202     C*
044100000801     C* Ebbene...
044200000801     C                   ADD       1             §CTRMO
044300010201     C                   IF        errore <> *zeros
044400000801     C                   ADD       1             §CTRNO
044500000801     C                   EVAL      recko = vindta
044600000801     C                   EXCEPT    rigadett
044700000801     C                   EVAL      vinflg = '2'
044800000801     C                   ELSE
044900010201     C                   ADD       1             §CTROKVB
045000000801     C                   ENDIF
045100000801     C*
045200000801     C                   ENDSR
045300070213     C*----------------------------------------------------*
045400070213     C*  REPERIMENTO NUMERATORE X TESTATA BOLLE
045500070213     C*----------------------------------------------------*
045600070213     C     REPNUM        BEGSR
045700070213     C*
045800070213     C* NSP => Stacco un numeratore da AZNUM
045900070213     C                   clear                   TRUL33DS
046000070213     C                   eval      I33OPE = *zeros
046100070213     C                   eval      I33CNU = 302
046200070213     C                   eval      I33NUM = 1
046300070213     C                   movel     TRUL33DS      KPJBU
046400070213     C                   call      'TRUL33R'
046500070213     C                   parm                    KPJBA
046600070213     C                   movel     KPJBU         TRUL33DS
046700070213     C                   if        O33ERR = *zeros
046800070213     C                   z-add     O33NRF        VABNSP
046900070213     C                   z-add     O33NRF        VATNSP
047000070213     C                   else
047100070213     C                   Z-ADD     1             errore
047200070213     C                   EVAL      vinmsg = %trimr(vinmsg)
047300070213     C                             + ' ' + 'VABNSP VATNSP'
047400070213     C                   endif
047500070213     C*
047600070213     C                   ENDSR
047700070102     C*----------------------------------------------------*
047800070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
047900070102     C*----------------------------------------------------*
048000070103     C     EXEVATE       BEGSR
048100070102     C*
048200070103     C                   EVAL      VATTRC='E'
048300070213     C                   EVAL      VATNOT = %trim(%subst(vindta:180:10))
048400070102     C                   exsr      wrivat                                       => scarico VAT
048500070102     C*
048600070102     C                   ENDSR
048700010201     C*----------------------------------------------------*
048800040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048900010201     C*----------------------------------------------------*
049000020305     C     WRIVAT        BEGSR
049100050628     C*
049200060223     C* Scrivo solo se valorizzato qualcosa
049300060223     C                   IF        VATNOT <> *blanks
049400040802     C                   WRITE     FIVAT000
049500060223     C                   ENDIF
049600010201     C*
049700010201     C                   ENDSR
049800010202     C*----------------------------------------------------*
049900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050000010202     C*----------------------------------------------------*
050100020305     C     PREVAT        BEGSR
050200010202     C*
050300021113     C* Compongo il nome del membro da dare al FIVATWWR
050400010202     C                   eval      parmbr = vlrhdl
050500010202     C                   movel     'M'           parmbr
050600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050700010202     C                   eval      paropz = '1'
050800010202     C* Effettuo la chiamata al CLLE preposto
050900040506     C                   call(e)   'TITVVTC'
051000010202     C                   parm                    parccm
051100010202     C                   parm                    parmbr
051200010202     C                   parm                    paropz
051300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051400010202     C                   if        %error
051500010202     C                   movel     '1'           chkcall
051600010202     C                   else
051700010202     C                   movel     '0'           chkcall
051800010202     C                   endif
051900010202     C*
052000010202     C                   ENDSR
052100000801     C*----------------------------------------------------*
052200000801     C*  CONTROLLO NUMERICITA' CAMPI
052300000801     C*----------------------------------------------------*
052400000801     C     CHKNUM        BEGSR
052500000801     C*
052600000801     C                   call(e)   'ISNUMERIC'
052700000801     C                   PARM                    PiStr            30
052800070213     C                   PARM      ','           PiDecChr          1
052900000801     C                   PARM      *ZEROS        PiVal            30 9
053000000801     C                   PARM      '0'           PiInt             1
053100000801     C                   PARM      '0'           PiNum             1
053200000801     C                   IF        %error
053300000801     C                   EVAL      PiInt=*off
053400000801     C                   ENDIF
053500000801     C*
053600000801     C                   ENDSR
053700000801     C***
053800000801
053900011113
054000011113     C*----------------------------------------------------*
054100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054200011113     C*----------------------------------------------------*
054300011113     C     CHKIMPDIV     BEGSR
054400011113     C*
054500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054600011113     C                   Z-ADD     *zeros        wrkDec            9 9
054700011113     C*
054800011113     C* Come prima cosa effettuo considerazioni sulla divisa
054900011113     C                   IF        vabIAS > *zeros
055000011113     C                   IF        vabVAS <> 'EUR'
055100011113     C                   EVAL      vabVAS =  'ITL'
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C                   IF        vabCAS > *zeros
055600011113     C                   IF        vabVCA <> 'EUR'
055700011113     C                   EVAL      vabVCA =  'ITL'
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C                   IF        vabVMD > *zeros
056200020305     C                   IF        vabVAD <> 'EUR'
056300011113     C                   EVAL      vabVAD =  'ITL'
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056800011113     C                   Z-ADD     vabIAS        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVAS = 'ITL'
057100011113     C                   EVAL      vabIAS = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057600011113     C                   Z-ADD     vabCAS        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVCA = 'ITL'
057900011113     C                   EVAL      vabCAS = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058400011113     C                   Z-ADD     vabVMD        wrkDec
058500011113     C                   IF        wrkDec > *zeros
058600011113     C                   IF        vabVAD = 'ITL'
058700011113     C                   EVAL      vabVMD = *zeros
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C                   ENDSR
059200011113     C***
059300011113
059400011113
059500000801
059600000801
059700990920      /TITLE Invio dei dati al punto operativo.
059800010202     C     invio         BEGSR
059900990920     C*
060000021113     C* 1° invio FIVAT
060100010201     C                   reset                   dscmz
060200010201     C                   move      vlrpoi        cmzdst
060300021113     C                   eval      cmzfld = 'FIVATWWR'
060400010201     C                   eval      cmzmbd = vlrhdl
060500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060600021009     C***                if        prmfir = *blanks
060700021113     C                   eval      cmzfla = 'FIVAT00F'
060800021113     C                   eval      cmzmba = 'FIVAT00F'
060900021009     C***                else
061000021009     C***                eval      cmzfla = prmfir
061100021009     C***                eval      cmzmba = prmfir
061200021009     C***                endif
061300010201     C                   eval      cmznrr = *zeros
061400020305     C                   move      §ctrokvt      cmznrr
061500021018     C                   eval      cmzlba = vlrfl1
061600010201     C                   call(e)   'TIS711C'
061700010201     C                   parm                    dscmz
061800010201     C                   parm      *blanks       esito
061900010205     C                   if        %error
062000010205     C                             or cmzerr = '1'
062100010205     C                             or esito  = '1'
062200010205     C                   eval      wrkesito = '3'
062300010205     C                   else
062400010201     C*
062500021113     C* 2° invio FIVAB
062600010201     C                   reset                   dscmz
062700010201     C                   move      vlrpoi        cmzdst
062800010201     C                   eval      cmzfld = vlrfou
062900010201     C                   eval      cmzmbd = vlrhdl
063000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063100021009     C***                if        prmfir = *blanks
063200021113     C                   eval      cmzfla = 'FIVAB00F'
063300021113     C                   eval      cmzmba = 'FIVAB00F'
063400021009     C***                else
063500021009     C***                eval      cmzfla = prmfir
063600021009     C***                eval      cmzmba = prmfir
063700021009     C***                endif
063800010201     C                   eval      cmznrr = *zeros
063900010201     C                   move      §ctrokvb      cmznrr
064000021018     C                   eval      cmzlba = vlrfl1
064100010201     C                   call(e)   'TIS711C'
064200010201     C                   parm                    dscmz
064300010201     C                   parm      *blanks       esito
064400010201     C                   if        %error
064500010201     C                             or cmzerr = '1'
064600010201     C                             or esito  = '1'
064700010201     C                   eval      wrkesito = '3'
064800010201     C                   endif
064900010205     C                   endif
065000990920     C*
065100000613     C                   ENDSR
065200000613     C***
065300070411
065400070411     C     *pssr         BEGSR
065500070411     C*
065600070411     C                   if        %open(tivin00r)
065700070411     C                   close     tivin00r
065800070411     C                   endif
065900070411     C                   if        %open(fivabwwr)
066000070411     C                   close     fivabwwr
066100070411     C                   endif
066200070411     C                   if        %open(fivatwwr)
066300070411     C                   close     fivatwwr
066400070411     C                   endif
066500070411     C*
066600070411     C* Effettuo la chiamata al CLLE preposto
066700070411     C                   call(e)   'TITVVTC'
066800070411     C                   parm                    parccm
066900070411     C                   parm                    parmbr
067000070411     C                   parm      '2'           paropz
067100070411     C*
067200070411     C                   eval      wrkesito = '2'
067300070411     C*
067400070411     C                   seton                                        LR
067500070411     C*
067600070411     C                   ENDSR     '*CANCL'
067700070411     C***
067800070411
067900990910
068000000613     C     *inzsr        BEGSR
068100990910     C*
068200990910     C     *entry        plist
068300990920     C                   parm                    tivlrds
068400990921     C                   parm      wrkesito      esito
068500000724     C                   parm                    prmlit
068600000710     C                   parm                    prmfir
068700000613     C*
068800000830     C* CALCOLA LA DATA CORRENTE
068900000830     C                   time                    wn14             14 0
069000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069100000830     C                   z-add     wn8           g08dat
069200000830     C                   z-add     *zeros        g08inv
069300000830     C                   movel     '0'           g08err
069400000830     C                   call      'XSRDA8'
069500000830     C                   parm                    wlbda8
069600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069700000830     C*
069800000613     C                   ENDSR
069900000613     C***
070000990908
070100070213     Otitvw9p   E            riepilogo         2
070200990915     O                                              'Upload via Internet'
070300990915     O                                           +1 'Traduzione TIVIN00R -'
070400021113     O                                           +1 'FIVABWWR/FIVATWWR'
070500010201     O                                           +1 'Report testate bolle'
070600990915     O          E            riepilogo   2
070700990915     O                       wrkdata
070800990915     O                       wrkora              +1
070900990915     O                       procname            +1
071000990915     O          E            riepilogo   2
071100990915     O                                              'Cliente..................:'
071200990915     O                       VABCCM        z     +1
071300990915     O          E            riepilogo   2
071400990920     O                                              'Riferimento Strategi.....:'
071500990920     O                       vlrhdl              +1
071600990915     O          E            riepilogo   2
071700990915     O                                              'Giusti...................:'
071800010201     O                       §CTROKVB      2   +  1
071900990915     O          E            riepilogo   2
072000990915     O                                              'Sbagliati e corretti.....:'
072100971022     O                       §CTRMO        2   +  1
072200990915     O          E            riepilogo   2
072300990915     O                                              'Sbagliati e scartati.....:'
072400971022     O                       §CTRNO        2   +  1
072500000613
072600070213     Otitvw9ps  E            testdett          2
072700000613     O                                              'Upload via Internet'
072800000613     O                                           +1 'Traduzione TIVIN00R -'
072900021113     O                                           +1 'FIVABWWR/FIVATWWR'
073000010201     O                                           +1 'Report testate bolle'
073100000616     O          E            testdett    3
073200000613     O                                           +2 'N° rec'
073300000613     O                                           +3 'Anteprima contenuto'
073400000616     O          E            rigadett    2
073500000613     O                       rrnum               +2
073600000621     O                       recko               +3
073700000616     O          E            findett     2
073800000613     O                       wrkdata
073900000613     O                       wrkora              +1
074000000613     O                       procname            +1
074100000616     O          E            findett     2
074200000613     O                                              'Cliente..................:'
074300000613     O                       VABCCM        z     +1
074400000616     O          E            findett     2
074500000613     O                                              'Riferimento Strategi.....:'
074600000613     O                       vlrhdl              +1
074700000616     O          E            findett     2
074800000613     O                                              'Giusti...................:'
074900010201     O                       §CTROKVB      2   +  1
075000000616     O          E            findett     2
075100000613     O                                              'Sbagliati e corretti.....:'
075200000613     O                       §CTRMO        2   +  1
075300000616     O          E            findett     2
075400000613     O                                              'Sbagliati e scartati.....:'
075500000613     O                       §CTRNO        2   +  1
075600000512** CMD - COMANDI CL
075700070213OVRPRTF FILE(TITVW9P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
075800070213OVRPRTF FILE(TITVW9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075900070213DLTOVR FILE(TITVW9P TITVW9PS) LVL(*)
076000000512
076100000512
