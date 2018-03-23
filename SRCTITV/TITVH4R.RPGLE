000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800050125     Ftitvh4p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050125     Ftitvh4ps  O    f  198        PRINTER usropn
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
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500051118     D depspe          s             10    INZ(*blanks)
003600051118     D curspe          s             10    INZ(*blanks)
003700030415     D cntspe          s              7  0 INZ(*zeros)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200010213
004300000801     D  Num5_0         s              5  0
004400030107     D  Num7_0         s              7  0
004500000830
004600000830     D*------------------
004700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800000830     D*------------------
004900000830     D WLBDA8          DS                  INZ
005000000830     D  G08DAT                 1      8  0
005100000830     D  G08INV                 9     16  0
005200000830     D  G08ERR                17     17
005300000830     D  G08TGI                18     22  0
005400000830     D*
005500990908
005600010201
005700010201
005800990915     C                   time                    wrkdata
005900990915     C                   time                    wrkora
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300000724     C*
006400000724     C* SE OCCORRE SPEDIRE IN FILIALE
006500021017     C                   if        vlrpoi <> *zeros
006600000724     C*
006700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006900021017     C     vlrpoi        chain     azorg01l
007000000724     C                   if        %found
007100000616     C                   movel(p)  CMD(1)        depcmd
007200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007300000616     C*
007400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007500011113     C                   Z-ADD     150           LENGH            15 5
007600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007700000616     C                   PARM                    depcmd
007800000616     C                   PARM                    LENGH
007900000724     C*
008000000724     C                   endif
008100000724     C                   endif
008200000616     C*
008300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008400000616     C                   movel(p)  CMD(2)        depcmd
008500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008600000616     C*
008700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008800011113     C                   Z-ADD     150           LENGH            15 5
008900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009000000616     C                   PARM                    depcmd
009100000616     C                   PARM                    LENGH
009200000616     C*
009300050125     C                   if        not %open(titvh4ps)
009400050125     C                   open      titvh4ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700000613     C*
009800030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200031201     C                   call(e)   'TITVVTC'
010300010202     C                   parm                    parccm
010400010202     C                   parm                    parmbr
010500010202     C                   parm      '2'           paropz
010600010202     C*
010700050125     C                   if        %open(titvh4ps)
010800000616     C                   except    findett
010900050125     C                   close     titvh4ps
011000000613     C                   endif
011100000616     C*
011200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011300011113     C                   Z-ADD     150           LENGH            15 5
011400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011500000616     C                   PARM                    CMD(3)
011600000616     C                   PARM                    LENGH
011700000616     C*
011800010201     C                   seton                                        LR
011900000613
012000000613
012100000613     C*--------------------------------------------------------
012200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012300000613     C*--------------------------------------------------------
012400000621     C     STPR          BEGSR
012500000613     C*
012600050125     C                   if        not %open(titvh4p)
012700050125     C                   open      titvh4p
012800990915     C                   endif
012900990915     C*
013000990915     C                   except    riepilogo
013100990915     C*
013200050125     C                   if        %open(titvh4p)
013300050125     C                   close     titvh4p
013400990914     C                   endif
013500990910     C*
013600000613     C                   ENDSR
013700000613     C***
013800990908
013900000801
014000910830     C*--------------------------------------------------------
014100030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
014200910830     C*--------------------------------------------------------
014300030226     C     RWFILE        BEGSR
014400990910     C*
014500990914     C                   if        not %open(tivin00r)
014600990908     C                   open      tivin00r
014700990914     C                   endif
014800021017     C                   if        not %open(fivabwwr)
014900021017     C                   open      fivabwwr
015000990914     C                   endif
015100021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015200020305     C                   exsr      prevat
015300010201     C*
015400010202     C                   if        chkcall = '0'
015500010202     C*
015600021017     C                   if        not %open(fivatwwr)
015700021017     C                   open      fivatwwr
015800010201     C                   endif
015900990910     C*
016000010201     C                   clear                   §CTROKVB          5 0
016100020305     C                   clear                   §CTROKVT          5 0
016200000801     C                   clear                   §CTRMO            5 0
016300000801     C                   clear                   §CTRNO            5 0
016400990910     C*
016500921023     C                   DO        *HIVAL
016600990913     C*
016700990915     C                   READ      tivin00r                               70
016800000905     C                   if        vindta > *blanks
016900000613     C                   add       1             rrnum
017000000801     C*
017100000801     C                   if        *in70 = *off
017200000801     C                             and
017300000801     C                             (vinflg = *blanks
017400000801     C                              or vinflg = '0'
017500000801     C                              or vinflg = '2')
017600000801     C*
017700000801     C                   clear                   vinmsg
017800030310     C                   eval      vinflg = '1'
017900030310     C*
018000030310     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018100051118     C                   eval      PiStr=%trim(%subst(vindta:167:10))
018200030411     C                   movel(p)  PiStr         curspe
018300050125     C                   add       1             cntspe
018400030411     C*
018500030411     C                   if        cntspe = 1
018600030411     C* A rottura inizializzo campi di totalizzazione
018700030411     C                   clear                   fivab000
018800030411     C*
018900030411     C                   exsr      impvab                                       => carico VAB
019000031201     C                   exsr      exevat                                       => carico VAT
019100030411     C                   eval      depspe = curspe
019200030411     C                   else
019300030411     C                   if        curspe <> depspe
019400030411     C                   exsr      wrivab                                       => scarico VAB
019500030411     C* A rottura inizializzo campi di totalizzazione
019600030411     C                   clear                   fivab000
019700030411     C*
019800030411     C                   exsr      impvab                                       => carico VAB
019900031201     C                   exsr      exevat                                       => carico VAT
020000030411     C                   eval      depspe = curspe
020100030411     C                   else
020200030411     C                   exsr      impvab                                       => carico VAB
020300031201     C                   exsr      exevat                                       => carico VAT
020400030411     C                   endif
020500030411     C                   endif
020600000905     C*
020700000905     C                   else
020800000905     C                   eval      vinflg = '1'
020900000905     C                   endif
021000030411     C                   endif
021100000905     C*
021200000905     C  N70              update    tivin000
021300000905     C*
021400991022     C  N70              ENDdo
021500030430     C*
021600030430     C* Al termine della lettura scarico i VAB/VAT ancora in "canna"
021700030430     C                   exsr      wrivab                                       => scarico VAB
021800010202     C*
021900010202     C                   endif
022000990910
022100990910     C* Se non ci sono record con errori ...
022200000710     C                   if        §ctrno = 0
022300990910     C* ... restituisco esito OK.
022400990921     C                   eval      wrkesito = '0'
022500990910     C                   else
022600010201     C                   if        §ctrokvb > 0
022700990921     C                   eval      wrkesito = '1'
022800000710     C                   else
022900000710     C                   eval      wrkesito = '2'
023000990910     C                   endif
023100000710     C                   endif
023200990910     C*
023300990914     C                   if        %open(tivin00r)
023400990908     C                   close     tivin00r
023500990914     C                   endif
023600021017     C                   if        %open(fivabwwr)
023700021017     C                   close     fivabwwr
023800990914     C                   endif
023900021017     C                   if        %open(fivatwwr)
024000021017     C                   close     fivatwwr
024100010201     C                   endif
024200021017     C*
024300021017     C                   if        vlrpoi <> 999
024400021017     C                   eval      vlrpoi = vlrpoi
024500021017     C                   endif
024600990910     C*
024700010201     C                   if        §ctrokvb > 0
024800021017     C                             and vlrpoi > *zeros
024900010202     C                   exsr      invio
025000990920     C                   endif
025100990920     C*
025200910830     C                   ENDSR
025300000613     C***
025400990920
025500000801     C*----------------------------------------------------*
025600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025700000801     C*----------------------------------------------------*
025800010201     C     INZVAR        BEGSR
025900000801     C*
026000010201     C                   Z-ADD     *zeros        Num5_0
026100030107     C                   Z-ADD     *zeros        Num7_0
026200030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026300030310     C                   MOVEL     '0'           FlgCAS            1
026400000801     C*
026500000801     C                   ENDSR
026600000801     C*----------------------------------------------------*
026700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026800000801     C*----------------------------------------------------*
026900000801     C     DEFCAM        BEGSR
027000000801     C*
027100020619     C* Imposto i valori di default...
027200050125     C                   Z-ADD     0390636       VABCCM
027300050125     C                   Z-ADD     0390636       VATCCM
027400021017     C                   Z-ADD     vlrpoi        VABFGS
027500021017     C                   Z-ADD     vlrpoi        VATFGS
027600050125     C                   Z-ADD     039           VABLNP
027700050125     C                   Z-ADD     039           VATLNP
027800050125     C                   Z-ADD     300           VABCTR
027900021017     C                   MOVEL     '7Q'          VABCTM
028000050125     C                   MOVEL     '1'           VABCBO
028100020619     C* ... e poi verifico se sono stati passati come parametri
028200020619     C                   IF        vlrppt > *blanks
028300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028400020619     C                   EXSR      CHKNUM
028500020619     C                   IF        PiInt=*on
028600020619     C                   Z-ADD     PiVal         VABCCM
028700020619     C                   Z-ADD     PiVal         VATCCM
028800020619     C                   ENDIF
028900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029000020619     C                   EXSR      CHKNUM
029100020619     C                   IF        PiInt=*on
029200020619     C                   Z-ADD     PiVal         VABLNP
029300020619     C                   Z-ADD     PiVal         VATLNP
029400020619     C                   ENDIF
029500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029600020619     C                   EXSR      CHKNUM
029700020619     C                   IF        PiInt=*on
029800020619     C                   Z-ADD     PiVal         VABCTR
029900020619     C                   ENDIF
030000020619     C                   ENDIF
030100000801     C*
030200000801     C                   ENDSR
030300000801     C*----------------------------------------------------*
030400030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030500000801     C*----------------------------------------------------*
030600010201     C     IMPVAB        BEGSR
030700000801     C*
030800020305     C                   EXSR      INZVAR
030900020305     C                   EXSR      DEFCAM
031000010305     C*
031100030226     C                   Z-ADD     *zeros        errore            1 0
031200000830     C                   MOVEL     datcor        VABAAS
031300050125     C                   MOVEL     datcor        VATAAS
031400000830     C                   MOVE      datcor        VABMGS
031500030226     C*
031600050128     C                   EVAL      VABRSD=%trim(%subst(vindta:26:35))
031700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031900020117     C* ==
032000050128     C                   EVAL      VABIND=%trim(%subst(vindta:61:35))
032100050128     C                   EVAL      VABLOD=%trim(%subst(vindta:101:35))
032200050128     C                   EVAL      VABPRD=%trim(%subst(vindta:136:2))
032300051114     C                   EVAL      VABRMA=%trim(%subst(vindta:1:8))
032400050125     C*
032500050125     C* Effettuo alcune considerazioni sul campo del cliente "Codice Società" che condiziona
032600050125     C* il ns. codice cliente mittente
032700050128     C*                  IF        %subst(vindta:144:4) = 'P002'
032800050127     C*                  EVAL      VABCCM = 0390636
032900050127     C*                  EVAL      VATCCM = 0390636
033000050127     C*                  ENDIF
033100050128     C*                  IF        %subst(vindta:144:4) = 'P006'
033200050127     C*                  EVAL      VABCCM = 0390733
033300050127     C*                  EVAL      VATCCM = 0390733
033400050127     C*                  ENDIF
033500050125     C*
033600030226     C* CAP
033700050128     C                   EVAL      PiStr=%trim(%subst(vindta:96:5))
033800010201     C                   EXSR      CHKNUM
033900010201     C                   IF        PiInt=*on
034000010201     C                   Z-ADD     PiVal         Num5_0
034100021017     C                   MOVEL     Num5_0        VABCAD
034200010201     C                   ELSE
034300010201     C                   ADD       1             errore
034400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034500021017     C                             + ' ' + 'VABCAD'
034600010201     C                   ENDIF
034700031201     C* NSP/RMN
034800051114     C                   EVAL      PiStr=%trim(%subst(vindta:167:10))
034900010201     C                   EXSR      CHKNUM
035000010201     C                   IF        PiInt=*on
035100030107     C                   Z-ADD     PiVal         Num7_0
035200030107     C                   MOVE(P)   Num7_0        VABNSP
035300030107     C                   MOVE(P)   Num7_0        VATNSP
035400031201     C                   Z-ADD     PiVal         VABRMN
035500010201     C                   ELSE
035600010201     C                   ADD       1             errore
035700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035800031201     C                             + ' ' + 'VABNSP VATNSP VABRMN'
035900010201     C                   ENDIF
036000030226     C* NCL
036100030415     C                   EVAL      VABNCL = VABNCL + 1
036200030226     C* PKB
036300050128     C                   EVAL      PiStr=%trim(%subst(vindta:148:7))
036400030107     C                   EXSR      CHKNUM
036500030107     C                   IF        PiNum=*on
036600050125     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 decim.
036700050125     C                   Z-ADD     PiVal         VABPKB
036800030107     C                   ELSE
036900030107     C                   ADD       1             errore
037000030107     C                   EVAL      vinmsg = %trimr(vinmsg)
037100030107     C                             + ' ' + 'VABPKB'
037200030107     C                   ENDIF
037300050125     C* CAS
037400050128     C                   IF        %subst(vindta:155:9) <> *zeros
037500050125     C                   MOVEL     '1'           FlgCAS
037600050128     C                   EVAL      VABVCA=%subst(vindta:164:3)
037700050128     C                   EVAL      PiStr=%trim(%subst(vindta:155:9))
037800050125     C                   EXSR      CHKNUM
037900050125     C                   IF        PiNum=*on
038000050125     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 decim.
038100050125     C                   Z-ADD     PiVal         VABCAS
038200050125     C                   ELSE
038300050125     C                   ADD       1             errore
038400050125     C                   EVAL      vinmsg = %trimr(vinmsg)
038500050125     C                             + ' ' + 'VABCAS'
038600050125     C                   ENDIF
038700050125     C                   ENDIF
038800040324     C* DCR
038900050128     C                   IF        %subst(vindta:138:6) <> *zeros
039000050128     C                   EVAL      PiStr='20'+%trim(%subst(vindta:138:6))
039100040324     C                   EXSR      CHKNUM
039200040324     C                   IF        PiInt=*on
039300040324     C                   Z-ADD     PiVal         VABDCR
039400040324     C                   ELSE
039500040324     C                   SETON                                        32
039600040324     C                   Z-ADD     *zeros        VABDCR
039700040324     C                   EVAL      vinmsg = %trimr(vinmsg)
039800040324     C                             + ' ' + 'VABDCR'
039900040324     C                   ENDIF
040000050125     C                   ENDIF
040100030310     C*
040200030310     C* Considerazioni finali su CBO/CAS
040300030310     C                   IF        FlgCAS = '1'
040400010205     C                   EVAL      VABCBO = '4'
040500010205     C                   ELSE
040600010205     C                   EVAL      VABCBO = '1'
040700010205     C                   ENDIF
040800020305     C*
040900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041000011113     C                   EXSR      CHKIMPDIV
041100010202     C*
041200000801     C* Ebbene...
041300000801     C                   ADD       1             §CTRMO
041400010201     C                   IF        errore <> *zeros
041500000801     C                   ADD       1             §CTRNO
041600000801     C                   EVAL      recko = vindta
041700000801     C                   EXCEPT    rigadett
041800000801     C                   EVAL      vinflg = '2'
041900000801     C                   ELSE
042000010201     C                   ADD       1             §CTROKVB
042100000801     C                   ENDIF
042200000801     C*
042300000801     C                   ENDSR
042400030226     C*----------------------------------------------------*
042500030226     C*  SCARICAMENTO BUFFER RECORDS VAB
042600030226     C*----------------------------------------------------*
042700030226     C     WRIVAB        BEGSR
042800030310     C*
042900030310     C                   write     FIVAB000
043000030226     C*
043100030226     C                   ENDSR
043200030226     C*----------------------------------------------------*
043300030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
043400030226     C*----------------------------------------------------*
043500031201     C     EXEVAT        BEGSR
043600030226     C*
043700030415     C* Valorizzo il barcode
043800031201     C                   EVAL      VATTRC='E'
043900050128     C                   EVAL      VATNOT=%trim(%subst(vindta:13:13))
044000031201     C                   exsr      wrivat                                       => scarico VAT
044100030226     C*
044200030226     C                   ENDSR
044300010201     C*----------------------------------------------------*
044400021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044500010201     C*----------------------------------------------------*
044600020305     C     WRIVAT        BEGSR
044700010201     C*
044800021017     C                   write     FIVAT000
044900010201     C*
045000010201     C                   ENDSR
045100010202     C*----------------------------------------------------*
045200021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045300010202     C*----------------------------------------------------*
045400020305     C     PREVAT        BEGSR
045500010202     C*
045600021017     C* Compongo il nome del membro da dare al FIVATWWR
045700010202     C                   eval      parmbr = vlrhdl
045800010202     C                   movel     'M'           parmbr
045900050125     C                   eval      parccm = %subst(vlrKSC:2:7)
046000010202     C                   eval      paropz = '1'
046100010202     C* Effettuo la chiamata al CLLE preposto
046200031201     C                   call(e)   'TITVVTC'
046300010202     C                   parm                    parccm
046400010202     C                   parm                    parmbr
046500010202     C                   parm                    paropz
046600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046700010202     C                   if        %error
046800010202     C                   movel     '1'           chkcall
046900010202     C                   else
047000010202     C                   movel     '0'           chkcall
047100010202     C                   endif
047200010202     C*
047300010202     C                   ENDSR
047400000801     C*----------------------------------------------------*
047500000801     C*  CONTROLLO NUMERICITA' CAMPI
047600000801     C*----------------------------------------------------*
047700000801     C     CHKNUM        BEGSR
047800000801     C*
047900000801     C                   call(e)   'ISNUMERIC'
048000000801     C                   PARM                    PiStr            30
048100030107     C                   PARM      ','           PiDecChr          1
048200000801     C                   PARM      *ZEROS        PiVal            30 9
048300000801     C                   PARM      '0'           PiInt             1
048400000801     C                   PARM      '0'           PiNum             1
048500000801     C                   IF        %error
048600000801     C                   EVAL      PiInt=*off
048700030107     C                   EVAL      PiNum=*off
048800000801     C                   ENDIF
048900000801     C*
049000000801     C                   ENDSR
049100000801     C***
049200000801
049300011113
049400011113     C*----------------------------------------------------*
049500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049600011113     C*----------------------------------------------------*
049700011113     C     CHKIMPDIV     BEGSR
049800011113     C*
049900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050000011113     C                   Z-ADD     *zeros        wrkDec            9 9
050100011113     C*
050200011113     C* Come prima cosa effettuo considerazioni sulla divisa
050300011113     C                   IF        vabIAS > *zeros
050400011113     C                   IF        vabVAS <> 'EUR'
050500011113     C                   EVAL      vabVAS =  'ITL'
050600011113     C                   ENDIF
050700011113     C                   ENDIF
050800011113     C*
050900011113     C                   IF        vabCAS > *zeros
051000011113     C                   IF        vabVCA <> 'EUR'
051100011113     C                   EVAL      vabVCA =  'ITL'
051200011113     C                   ENDIF
051300011113     C                   ENDIF
051400011113     C*
051500011113     C                   IF        vabVMD > *zeros
051600020305     C                   IF        vabVAD <> 'EUR'
051700011113     C                   EVAL      vabVAD =  'ITL'
051800011113     C                   ENDIF
051900011113     C                   ENDIF
052000011113     C*
052100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052200011113     C                   Z-ADD     vabIAS        wrkDec
052300011113     C                   IF        wrkDec > *zeros
052400011113     C                   IF        vabVAS = 'ITL'
052500011113     C                   EVAL      vabIAS = *zeros
052600011113     C                   ENDIF
052700011113     C                   ENDIF
052800011113     C*
052900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
053000011113     C                   Z-ADD     vabCAS        wrkDec
053100011113     C                   IF        wrkDec > *zeros
053200011113     C                   IF        vabVCA = 'ITL'
053300011113     C                   EVAL      vabCAS = *zeros
053400011113     C                   ENDIF
053500011113     C                   ENDIF
053600011113     C*
053700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053800011113     C                   Z-ADD     vabVMD        wrkDec
053900011113     C                   IF        wrkDec > *zeros
054000011113     C                   IF        vabVAD = 'ITL'
054100011113     C                   EVAL      vabVMD = *zeros
054200011113     C                   ENDIF
054300011113     C                   ENDIF
054400011113     C*
054500011113     C                   ENDSR
054600011113     C***
054700021017
054800000801
054900000801
055000990920      /TITLE Invio dei dati al punto operativo.
055100010202     C     invio         BEGSR
055200990920     C*
055300021017     C* 1° invio FIVAT
055400010201     C                   reset                   dscmz
055500021017     C                   move      vlrpoi        cmzdst
055600021017     C                   eval      cmzfld = 'FIVATWWR'
055700010201     C                   eval      cmzmbd = vlrhdl
055800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055900021009     C***                if        prmfir = *blanks
056000021017     C                   eval      cmzfla = 'FIVAT00F'
056100021017     C                   eval      cmzmba = 'FIVAT00F'
056200021009     C***                else
056300021009     C***                eval      cmzfla = prmfir
056400021009     C***                eval      cmzmba = prmfir
056500021009     C***                endif
056600010201     C                   eval      cmznrr = *zeros
056700020305     C                   move      §ctrokvt      cmznrr
056800021018     C                   eval      cmzlba = vlrfl1
056900010201     C                   call(e)   'TIS711C'
057000010201     C                   parm                    dscmz
057100010201     C                   parm      *blanks       esito
057200010205     C                   if        %error
057300010205     C                             or cmzerr = '1'
057400010205     C                             or esito  = '1'
057500010205     C                   eval      wrkesito = '3'
057600010205     C                   else
057700010201     C*
057800021017     C* 2° invio FIVAB
057900010201     C                   reset                   dscmz
058000021017     C                   move      vlrpoi        cmzdst
058100010201     C                   eval      cmzfld = vlrfou
058200010201     C                   eval      cmzmbd = vlrhdl
058300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058400021009     C***                if        prmfir = *blanks
058500021017     C                   eval      cmzfla = 'FIVAB00F'
058600021017     C                   eval      cmzmba = 'FIVAB00F'
058700021009     C***                else
058800021009     C***                eval      cmzfla = prmfir
058900021009     C***                eval      cmzmba = prmfir
059000021009     C***                endif
059100010201     C                   eval      cmznrr = *zeros
059200010201     C                   move      §ctrokvb      cmznrr
059300021018     C                   eval      cmzlba = vlrfl1
059400010201     C                   call(e)   'TIS711C'
059500010201     C                   parm                    dscmz
059600010201     C                   parm      *blanks       esito
059700010201     C                   if        %error
059800010201     C                             or cmzerr = '1'
059900010201     C                             or esito  = '1'
060000010201     C                   eval      wrkesito = '3'
060100010201     C                   endif
060200010205     C                   endif
060300990920     C*
060400000613     C                   ENDSR
060500000613     C***
060600070411
060700070411     C     *pssr         BEGSR
060800070411     C*
060900070411     C                   if        %open(tivin00r)
061000070411     C                   close     tivin00r
061100070411     C                   endif
061200070411     C                   if        %open(fivabwwr)
061300070411     C                   close     fivabwwr
061400070411     C                   endif
061500070411     C                   if        %open(fivatwwr)
061600070411     C                   close     fivatwwr
061700070411     C                   endif
061800070411     C*
061900070411     C* Effettuo la chiamata al CLLE preposto
062000070411     C                   call(e)   'TITVVTC'
062100070411     C                   parm                    parccm
062200070411     C                   parm                    parmbr
062300070411     C                   parm      '2'           paropz
062400070411     C*
062500070411     C                   eval      wrkesito = '2'
062600070411     C*
062700070411     C                   seton                                        LR
062800070411     C*
062900070411     C                   ENDSR     '*CANCL'
063000070411     C***
063100070411
063200990910
063300000613     C     *inzsr        BEGSR
063400990910     C*
063500990910     C     *entry        plist
063600990920     C                   parm                    tivlrds
063700990921     C                   parm      wrkesito      esito
063800000724     C                   parm                    prmlit
063900000710     C                   parm                    prmfir
064000000613     C*
064100000830     C* CALCOLA LA DATA CORRENTE
064200000830     C                   time                    wn14             14 0
064300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064400000830     C                   z-add     wn8           g08dat
064500000830     C                   z-add     *zeros        g08inv
064600000830     C                   movel     '0'           g08err
064700000830     C                   call      'XSRDA8'
064800000830     C                   parm                    wlbda8
064900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065000000830     C*
065100000613     C                   ENDSR
065200000613     C***
065300990908
065400050125     Otitvh4p   E            riepilogo         2
065500990915     O                                              'Upload via Internet'
065600990915     O                                           +1 'Traduzione TIVIN00R -'
065700021017     O                                           +1 'FIVABWWR/FIVATWWR'
065800010201     O                                           +1 'Report testate bolle'
065900990915     O          E            riepilogo   2
066000990915     O                       wrkdata
066100990915     O                       wrkora              +1
066200990915     O                       procname            +1
066300990915     O          E            riepilogo   2
066400990915     O                                              'Cliente..................:'
066500990915     O                       VABCCM        z     +1
066600990915     O          E            riepilogo   2
066700990920     O                                              'Riferimento Strategi.....:'
066800990920     O                       vlrhdl              +1
066900990915     O          E            riepilogo   2
067000990915     O                                              'Giusti...................:'
067100010201     O                       §CTROKVB      2   +  1
067200990915     O          E            riepilogo   2
067300990915     O                                              'Sbagliati e corretti.....:'
067400971022     O                       §CTRMO        2   +  1
067500990915     O          E            riepilogo   2
067600990915     O                                              'Sbagliati e scartati.....:'
067700971022     O                       §CTRNO        2   +  1
067800000613
067900050125     Otitvh4ps  E            testdett          2
068000000613     O                                              'Upload via Internet'
068100000613     O                                           +1 'Traduzione TIVIN00R -'
068200021017     O                                           +1 'FIVABWWR/FIVATWWR'
068300010201     O                                           +1 'Report testate bolle'
068400000616     O          E            testdett    3
068500000613     O                                           +2 'N° rec'
068600000613     O                                           +3 'Anteprima contenuto'
068700000616     O          E            rigadett    2
068800000613     O                       rrnum               +2
068900000621     O                       recko               +3
069000000616     O          E            findett     2
069100000613     O                       wrkdata
069200000613     O                       wrkora              +1
069300000613     O                       procname            +1
069400000616     O          E            findett     2
069500000613     O                                              'Cliente..................:'
069600000613     O                       VABCCM        z     +1
069700000616     O          E            findett     2
069800000613     O                                              'Riferimento Strategi.....:'
069900000613     O                       vlrhdl              +1
070000000616     O          E            findett     2
070100000613     O                                              'Giusti...................:'
070200010201     O                       §CTROKVB      2   +  1
070300000616     O          E            findett     2
070400000613     O                                              'Sbagliati e corretti.....:'
070500000613     O                       §CTRMO        2   +  1
070600000616     O          E            findett     2
070700000613     O                                              'Sbagliati e scartati.....:'
070800000613     O                       §CTRNO        2   +  1
070900000512** CMD - COMANDI CL
071000050125OVRPRTF FILE(TITVH4P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
071100050125OVRPRTF FILE(TITVH4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071200050125DLTOVR FILE(TITVH4P TITVH4PS) LVL(*)
071300000512
071400000512
