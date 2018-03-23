000100050805      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050805     FFIVABwwr  O    E             DISK    usropn
000700050805     FFIVATwwr  O    E             DISK    usropn
000800050805     Ftitvn6p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050805     Ftitvn6ps  O    f  198        PRINTER usropn
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
002600050805     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600040428     D depspe          s              8    INZ(*blanks)
003700040428     D curspe          s              8    INZ(*blanks)
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
009300050805     C                   if        not %open(titvn6ps)
009400050805     C                   open      titvn6ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700000613     C*
009800050802     C                   EXSR      RWVAB                                        LETT/SCR. VAB/VAT
009900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200050805     C                   call(e)   'TITVVTC'
010300010202     C                   parm                    parccm
010400010202     C                   parm                    parmbr
010500010202     C                   parm      '2'           paropz
010600050805     C*
010700050805     C* Effettuo lancio TISI95 solo x chiusura
010800050805     C                   CLEAR                   TISI95DS
010900050805     C                   EVAL      I95TLA = 'C'
011000050805     C                   CALL      'TISI95R'
011100050805     C                   PARM                    TISI95DS
011200010202     C*
011300050805     C                   if        %open(titvn6ps)
011400000616     C                   except    findett
011500050805     C                   close     titvn6ps
011600000613     C                   endif
011700000616     C*
011800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    CMD(3)
012200000616     C                   PARM                    LENGH
012300000616     C*
012400000801     C
012500010201     C                   seton                                        LR
012600000613
012700000613
012800000613     C*--------------------------------------------------------
012900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013000000613     C*--------------------------------------------------------
013100000621     C     STPR          BEGSR
013200000613     C*
013300050805     C                   if        not %open(titvn6p)
013400050805     C                   open      titvn6p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900050805     C                   if        %open(titvn6p)
014000050805     C                   close     titvn6p
014100990914     C                   endif
014200990910     C*
014300000613     C                   ENDSR
014400000613     C***
014500990908
014600000801
014700910830     C*--------------------------------------------------------
014800050805     C* RWVAB     LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
014900910830     C*--------------------------------------------------------
015000050802     C     RWVAB         BEGSR
015100990910     C*
015200990914     C                   if        not %open(tivin00r)
015300990908     C                   open      tivin00r
015400990914     C                   endif
015500050805     C                   if        not %open(fivabwwr)
015600050805     C                   open      fivabwwr
015700990914     C                   endif
015800050805     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015900020305     C                   exsr      prevat
016000010201     C*
016100040428     C                   if        chkcall = '0'
016200010202     C*
016300050805     C                   if        not %open(fivatwwr)
016400050805     C                   open      fivatwwr
016500010201     C                   endif
016600990910     C*
016700010201     C                   clear                   §CTROKVB          5 0
016800020305     C                   clear                   §CTROKVT          5 0
016900000801     C                   clear                   §CTRMO            5 0
017000000801     C                   clear                   §CTRNO            5 0
017100990910     C*
017200921023     C                   DO        *HIVAL
017300990913     C*
017400990915     C                   READ      tivin00r                               70
017500000905     C                   if        vindta > *blanks
017600000613     C                   add       1             rrnum
017700000801     C*
017800000801     C                   if        *in70 = *off
017900000801     C                             and
018000000801     C                             (vinflg = *blanks
018100000801     C                              or vinflg = '0'
018200000801     C                              or vinflg = '2')
018300000801     C*
018400000801     C                   clear                   vinmsg
018500010305     C*
018600050805     C                   exsr      impfile                                      => carico VAB/VAT
018700050805     C                   if        errore = *zeros
018800050805     C                   exsr      wrifile                                      => scrivo VAB/VAT
018900050805     C                   endif
019000010305     C                   endif
019100000905     C*
019200000905     C                   else
019300000905     C                   eval      vinflg = '1'
019400000905     C                   endif
019500000905     C*
019600000905     C  N70              update    tivin000
019700000905     C*
019800991022     C  N70              ENDdo
019900010202     C*
020000010202     C                   endif
020100050802     C*
020200990910     C* Se non ci sono record con errori ...
020300000710     C                   if        §ctrno = 0
020400990910     C* ... restituisco esito OK.
020500990921     C                   eval      wrkesito = '0'
020600990910     C                   else
020700010201     C                   if        §ctrokvb > 0
020800990921     C                   eval      wrkesito = '1'
020900000710     C                   else
021000000710     C                   eval      wrkesito = '2'
021100990910     C                   endif
021200000710     C                   endif
021300990910     C*
021400990914     C                   if        %open(tivin00r)
021500990908     C                   close     tivin00r
021600990914     C                   endif
021700050805     C                   if        %open(fivabwwr)
021800050805     C                   close     fivabwwr
021900990914     C                   endif
022000050805     C                   if        %open(fivatwwr)
022100050805     C                   close     fivatwwr
022200010201     C                   endif
022300021017     C*
022400021017     C                   if        vlrpoi <> 999
022500021017     C                   eval      vlrpoi = vlrpoi
022600021017     C                   endif
022700990910     C*
022800010201     C                   if        §ctrokvb > 0
022900021017     C                             and vlrpoi > *zeros
023000010202     C                   exsr      invio
023100990920     C                   endif
023200990920     C*
023300910830     C                   ENDSR
023400000613     C***
023500010305
023600010305     C*----------------------------------------------------*
023700050805     C*  SCARICAMENTO BUFFER RECORDS VAB E VAT
023800010305     C*----------------------------------------------------*
023900050805     C     WRIFILE       BEGSR
024000010305     C*
024100050805     C                   write     fivab000                                     => scarico il VAB
024200050805     C                   write     fivat000                                     => scarico il VAT
024300010305     C*
024400010305     C                   ENDSR
024500990920
024600000801     C*----------------------------------------------------*
024700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024800000801     C*----------------------------------------------------*
024900010201     C     INZVAR        BEGSR
025000000801     C*
025100010201     C                   Z-ADD     *zeros        Num5_0
025200030107     C                   Z-ADD     *zeros        Num7_0
025300030711     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
025400030711     C                   MOVEL     '0'           FlgCAS            1
025500000801     C*
025600000801     C                   ENDSR
025700000801     C*----------------------------------------------------*
025800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025900000801     C*----------------------------------------------------*
026000000801     C     DEFCAM        BEGSR
026100040428     C*
026200050805     C                   CLEAR                   FIVAB000
026300050805     C                   CLEAR                   FIVAT000
026400000801     C*
026500020619     C* Imposto i valori di default...
026600050805     C                   Z-ADD     0432900       VABCCM
026700050805     C                   Z-ADD     0432900       VATCCM
026800021017     C                   Z-ADD     vlrpoi        VABFGS
026900021017     C                   Z-ADD     vlrpoi        VATFGS
027000050805     C                   Z-ADD     043           VABLNP
027100050805     C                   Z-ADD     043           VATLNP
027200050802     C                   Z-ADD     000           VABCTR
027300050805     C                   Z-ADD     1             VABNCL
027400050802     C                   MOVEL     '1'           VABCBO
027500050805     C                   MOVEL     'B'           VATTRC
027600020619     C* ... e poi verifico se sono stati passati come parametri
027700020619     C                   IF        vlrppt > *blanks
027800040428     C                   IF        %subst(vlrppt:1:7) <> *blanks
027900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028000020619     C                   EXSR      CHKNUM
028100020619     C                   IF        PiInt=*on
028200020619     C                   Z-ADD     PiVal         VABCCM
028300020619     C                   Z-ADD     PiVal         VATCCM
028400020619     C                   ENDIF
028500040428     C                   ENDIF
028600040428     C                   IF        %subst(vlrppt:8:3) <> *blanks
028700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028800020619     C                   EXSR      CHKNUM
028900020619     C                   IF        PiInt=*on
029000020619     C                   Z-ADD     PiVal         VABLNP
029100020619     C                   Z-ADD     PiVal         VATLNP
029200020619     C                   ENDIF
029300040428     C                   ENDIF
029400040428     C                   IF        %subst(vlrppt:11:3) <> *blanks
029500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029600020619     C                   EXSR      CHKNUM
029700020619     C                   IF        PiInt=*on
029800020619     C                   Z-ADD     PiVal         VABCTR
029900020619     C                   ENDIF
030000040428     C                   ENDIF
030100020619     C                   ENDIF
030200000801     C*
030300000801     C                   ENDSR
030400000801     C*----------------------------------------------------*
030500050805     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB E FIVAT)
030600000801     C*----------------------------------------------------*
030700050805     C     IMPFILE       BEGSR
030800000801     C*
030900020305     C                   EXSR      INZVAR
031000020305     C                   EXSR      DEFCAM
031100010305     C*
031200000801     C                   Z-ADD     *zeros        errore            1 0
031300000830     C                   MOVEL     datcor        VABAAS
031400020305     C                   MOVEL     datcor        VATAAS
031500000830     C                   MOVE      datcor        VABMGS
031600031117     C*
031700050805     C                   MOVEL     *blanks       wRAGSOCDEST      70
031800050805     C                   EVAL      wRAGSOCDEST=%trim(%subst(vindta:9:32))+' '+
031900050805     C                                         %trim(%subst(vindta:41:32))+' '+
032000050805     C                                         %trim(%subst(vindta:73:24))
032100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032200050805     C     '@':'A'       XLATE     wRAGSOCDEST   wRAGSOCDEST
032300020117     C* ==
032400050805     C                   EVAL      VABRSD=%trim(%subst(wRAGSOCDEST:1:35))
032500050805     C                   EVAL      VABRD2=%trim(%subst(wRAGSOCDEST:36:35))
032600050805     C                   EVAL      VABIND=%trim(%subst(vindta:97:56))
032700050805     C                   EVAL      VABLOD=%trim(%subst(vindta:161:32))
032800050805     C                   EVAL      VABRMA=%trim(%subst(vindta:266:15))
032900050805     C                   EVAL      VATNOT=%trim(%subst(vindta:193:33))
033000030711     C* CAD
033100050805     C                   EVAL      PiStr=%trim(%subst(vindta:153:8))
033200010201     C                   EXSR      CHKNUM
033300010201     C                   IF        PiInt=*on
033400010201     C                   Z-ADD     PiVal         Num5_0
033500021017     C                   MOVEL     Num5_0        VABCAD
033600010201     C                   ELSE
033700010201     C                   ADD       1             errore
033800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033900021017     C                             + ' ' + 'VABCAD'
034000050805     C                   ENDIF
034100050805     C* Reperisco la provincia dal CAP e dalla località
034200050805     C                   IF        VABCAD <> *blanks
034300050805     C                   CLEAR                   TISI95DS
034400050805     C                   EVAL      I95TCN = '3'
034500050805     C                   Z-ADD     datcor        I95DAT
034600050805     C                   EVAL      I95CAP = VABCAD
034700050805     C                   EVAL      I95LOC = VABLOD
034800050805     C                   CALL      'TISI95R'
034900050805     C                   PARM                    TISI95DS
035000050805     C                   EVAL      VABPRD = O95PRV
035100010201     C                   ENDIF
035200030711     C* RMN
035300050805     C                   EVAL      PiStr=%trim(%subst(vindta:1:8))
035400010201     C                   EXSR      CHKNUM
035500010201     C                   IF        PiInt=*on
035600021017     C                   Z-ADD     PiVal         VABRMN
035700010201     C                   ELSE
035800010201     C                   ADD       1             errore
035900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036000050802     C                             + ' ' + 'VABRMN'
036100010201     C                   ENDIF
036200050802     C* NSP
036300050805     C                   EVAL      PiStr=%trim(%subst(vindta:1:8))
036400050802     C                   EXSR      CHKNUM
036500050802     C                   IF        PiInt=*on
036600050802     C                   Z-ADD     PiVal         Num7_0
036700050802     C                   MOVE(P)   Num7_0        VABNSP
036800050802     C                   MOVE(P)   Num7_0        VATNSP
036900050802     C                   ELSE
037000050802     C                   ADD       1             errore
037100050802     C                   EVAL      vinmsg = %trimr(vinmsg)
037200050802     C                             + ' ' + 'VABNSP VATNSP'
037300050802     C                   ENDIF
037400050802     C* PKB
037500050805     C                   EVAL      PiStr=%trim(%subst(vindta:251:15))
037600030107     C                   EXSR      CHKNUM
037700030107     C                   IF        PiNum=*on
037800050805     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg.
037900050802     C                   Z-ADD     PiVal         VABPKB
038000030107     C                   ELSE
038100030107     C                   ADD       1             errore
038200030107     C                   EVAL      vinmsg = %trimr(vinmsg)
038300030107     C                             + ' ' + 'VABPKB'
038400030107     C                   ENDIF
038500050805     C* CAS
038600050805     C                   IF        %subst(vindta:226:10) <> *blanks
038700050805     C                   EVAL      PiStr=%trim(%subst(vindta:226:10))
038800050802     C                   EXSR      CHKNUM
038900050802     C                   IF        PiNum=*on
039000050805     C                   Z-ADD     PiVal         VABCAS
039100050805     C                   MOVEL     '1'           FlgCAS
039200050805     C                   IF        VABCAS > *zeros
039300050805     C                   EVAL      VABVCA = 'EUR'
039400050802     C                   ENDIF
039500050802     C                   ELSE
039600050802     C                   ADD       1             errore
039700050802     C                   EVAL      vinmsg = %trimr(vinmsg)
039800050805     C                             + ' ' + 'VABCAS'
039900050802     C                   ENDIF
040000050802     C                   ENDIF
040100010205     C*
040200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
040300040428     C                   IF        FlgCAS = '0'
040400031117     C                   EVAL      VABCBO = '1'                                 * franco  NO contr.
040500040428     C                   ELSE
040600040428     C                   EVAL      VABCBO = '4'                                 * franco  SI contr.
040700031117     C                   ENDIF
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
042000050805     C                   EVAL      vinflg = '1'
042100010201     C                   ADD       1             §CTROKVB
042200050805     C                   ADD       1             §CTROKVT
042300000801     C                   ENDIF
042400000801     C*
042500000801     C                   ENDSR
042600010202     C*----------------------------------------------------*
042700050805     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042800010202     C*----------------------------------------------------*
042900020305     C     PREVAT        BEGSR
043000010202     C*
043100050805     C* Compongo il nome del membro da dare al FIVATWWR
043200010202     C                   eval      parmbr = vlrhdl
043300010202     C                   movel     'M'           parmbr
043400050805     C                   eval      parccm = '00432900'
043500010202     C                   eval      paropz = '1'
043600010202     C* Effettuo la chiamata al CLLE preposto
043700050805     C                   call(e)   'TITVVTC'
043800010202     C                   parm                    parccm
043900010202     C                   parm                    parmbr
044000010202     C                   parm                    paropz
044100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044200010202     C                   if        %error
044300010202     C                   movel     '1'           chkcall
044400010202     C                   else
044500010202     C                   movel     '0'           chkcall
044600010202     C                   endif
044700010202     C*
044800010202     C                   ENDSR
044900000801     C*----------------------------------------------------*
045000000801     C*  CONTROLLO NUMERICITA' CAMPI
045100000801     C*----------------------------------------------------*
045200000801     C     CHKNUM        BEGSR
045300000801     C*
045400000801     C                   call(e)   'ISNUMERIC'
045500000801     C                   PARM                    PiStr            30
045600050805     C                   PARM      ','           PiDecChr          1
045700000801     C                   PARM      *ZEROS        PiVal            30 9
045800000801     C                   PARM      '0'           PiInt             1
045900000801     C                   PARM      '0'           PiNum             1
046000000801     C                   IF        %error
046100000801     C                   EVAL      PiInt=*off
046200030107     C                   EVAL      PiNum=*off
046300000801     C                   ENDIF
046400000801     C*
046500000801     C                   ENDSR
046600000801     C***
046700000801
046800011113
046900011113     C*----------------------------------------------------*
047000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047100011113     C*----------------------------------------------------*
047200011113     C     CHKIMPDIV     BEGSR
047300011113     C*
047400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047500011113     C                   Z-ADD     *zeros        wrkDec            9 9
047600011113     C*
047700011113     C* Come prima cosa effettuo considerazioni sulla divisa
047800011113     C                   IF        vabIAS > *zeros
047900011113     C                   IF        vabVAS <> 'EUR'
048000011113     C                   EVAL      vabVAS =  'ITL'
048100011113     C                   ENDIF
048200011113     C                   ENDIF
048300011113     C*
048400011113     C                   IF        vabCAS > *zeros
048500011113     C                   IF        vabVCA <> 'EUR'
048600011113     C                   EVAL      vabVCA =  'ITL'
048700011113     C                   ENDIF
048800011113     C                   ENDIF
048900011113     C*
049000011113     C                   IF        vabVMD > *zeros
049100020305     C                   IF        vabVAD <> 'EUR'
049200011113     C                   EVAL      vabVAD =  'ITL'
049300011113     C                   ENDIF
049400011113     C                   ENDIF
049500011113     C*
049600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049700011113     C                   Z-ADD     vabIAS        wrkDec
049800011113     C                   IF        wrkDec > *zeros
049900011113     C                   IF        vabVAS = 'ITL'
050000011113     C                   EVAL      vabIAS = *zeros
050100011113     C                   ENDIF
050200011113     C                   ENDIF
050300011113     C*
050400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050500011113     C                   Z-ADD     vabCAS        wrkDec
050600011113     C                   IF        wrkDec > *zeros
050700011113     C                   IF        vabVCA = 'ITL'
050800011113     C                   EVAL      vabCAS = *zeros
050900011113     C                   ENDIF
051000011113     C                   ENDIF
051100011113     C*
051200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051300011113     C                   Z-ADD     vabVMD        wrkDec
051400011113     C                   IF        wrkDec > *zeros
051500011113     C                   IF        vabVAD = 'ITL'
051600011113     C                   EVAL      vabVMD = *zeros
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C                   ENDSR
052100011113     C***
052200021017
052300000801
052400000801
052500990920      /TITLE Invio dei dati al punto operativo.
052600010202     C     invio         BEGSR
052700990920     C*
052800050805     C* 1° invio FIVAT
052900010201     C                   reset                   dscmz
053000021017     C                   move      vlrpoi        cmzdst
053100050805     C                   eval      cmzfld = 'FIVATWWR'
053200010201     C                   eval      cmzmbd = vlrhdl
053300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053400021009     C***                if        prmfir = *blanks
053500050805     C                   eval      cmzfla = 'FIVAT00F'
053600050805     C                   eval      cmzmba = 'FIVAT00F'
053700021009     C***                else
053800021009     C***                eval      cmzfla = prmfir
053900021009     C***                eval      cmzmba = prmfir
054000021009     C***                endif
054100010201     C                   eval      cmznrr = *zeros
054200020305     C                   move      §ctrokvt      cmznrr
054300021018     C                   eval      cmzlba = vlrfl1
054400010201     C                   call(e)   'TIS711C'
054500010201     C                   parm                    dscmz
054600010201     C                   parm      *blanks       esito
054700010205     C                   if        %error
054800010205     C                             or cmzerr = '1'
054900010205     C                             or esito  = '1'
055000010205     C                   eval      wrkesito = '3'
055100010205     C                   else
055200010201     C*
055300050805     C* 2° invio FIVAB
055400010201     C                   reset                   dscmz
055500021017     C                   move      vlrpoi        cmzdst
055600010201     C                   eval      cmzfld = vlrfou
055700010201     C                   eval      cmzmbd = vlrhdl
055800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055900021009     C***                if        prmfir = *blanks
056000050805     C                   eval      cmzfla = 'FIVAB00F'
056100050805     C                   eval      cmzmba = 'FIVAB00F'
056200021009     C***                else
056300021009     C***                eval      cmzfla = prmfir
056400021009     C***                eval      cmzmba = prmfir
056500021009     C***                endif
056600010201     C                   eval      cmznrr = *zeros
056700010201     C                   move      §ctrokvb      cmznrr
056800021018     C                   eval      cmzlba = vlrfl1
056900010201     C                   call(e)   'TIS711C'
057000010201     C                   parm                    dscmz
057100010201     C                   parm      *blanks       esito
057200010201     C                   if        %error
057300010201     C                             or cmzerr = '1'
057400010201     C                             or esito  = '1'
057500010201     C                   eval      wrkesito = '3'
057600010201     C                   endif
057700010205     C                   endif
057800990920     C*
057900000613     C                   ENDSR
058000000613     C***
058100070411
058200070411     C     *pssr         BEGSR
058300070411     C*
058400070411     C                   if        %open(tivin00r)
058500070411     C                   close     tivin00r
058600070411     C                   endif
058700070411     C                   if        %open(fivabwwr)
058800070411     C                   close     fivabwwr
058900070411     C                   endif
059000070411     C                   if        %open(fivatwwr)
059100070411     C                   close     fivatwwr
059200070411     C                   endif
059300070411     C*
059400070411     C* Effettuo la chiamata al CLLE preposto
059500070411     C                   call(e)   'TITVVTC'
059600070411     C                   parm                    parccm
059700070411     C                   parm                    parmbr
059800070411     C                   parm      '2'           paropz
059900070411     C*
060000070411     C                   eval      wrkesito = '2'
060100070411     C*
060200070411     C                   seton                                        LR
060300070411     C*
060400070411     C                   ENDSR     '*CANCL'
060500070411     C***
060600070411
060700990910
060800000613     C     *inzsr        BEGSR
060900990910     C*
061000990910     C     *entry        plist
061100990920     C                   parm                    tivlrds
061200990921     C                   parm      wrkesito      esito
061300000724     C                   parm                    prmlit
061400000710     C                   parm                    prmfir
061500000613     C*
061600000830     C* CALCOLA LA DATA CORRENTE
061700000830     C                   time                    wn14             14 0
061800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061900050802     C                   movel     wn14          oracor            6 0          *ORA
062000000830     C                   z-add     wn8           g08dat
062100000830     C                   z-add     *zeros        g08inv
062200000830     C                   movel     '0'           g08err
062300000830     C                   call      'XSRDA8'
062400000830     C                   parm                    wlbda8
062500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062600000830     C*
062700000613     C                   ENDSR
062800000613     C***
062900990908
063000050805     Otitvn6p   E            riepilogo         2
063100990915     O                                              'Upload via Internet'
063200990915     O                                           +1 'Traduzione TIVIN00R -'
063300050805     O                                           +1 'FIVABWWR/FIVATWWR'
063400010201     O                                           +1 'Report testate bolle'
063500990915     O          E            riepilogo   2
063600990915     O                       wrkdata
063700990915     O                       wrkora              +1
063800990915     O                       procname            +1
063900990915     O          E            riepilogo   2
064000990915     O                                              'Cliente..................:'
064100990915     O                       VABCCM        z     +1
064200990915     O          E            riepilogo   2
064300990920     O                                              'Riferimento Strategi.....:'
064400990920     O                       vlrhdl              +1
064500990915     O          E            riepilogo   2
064600990915     O                                              'Giusti...................:'
064700010201     O                       §CTROKVB      2   +  1
064800990915     O          E            riepilogo   2
064900990915     O                                              'Sbagliati e corretti.....:'
065000971022     O                       §CTRMO        2   +  1
065100990915     O          E            riepilogo   2
065200990915     O                                              'Sbagliati e scartati.....:'
065300971022     O                       §CTRNO        2   +  1
065400000613
065500050805     Otitvn6ps  E            testdett          2
065600000613     O                                              'Upload via Internet'
065700000613     O                                           +1 'Traduzione TIVIN00R -'
065800050805     O                                           +1 'FIVABWWR/FIVATWWR'
065900010201     O                                           +1 'Report testate bolle'
066000000616     O          E            testdett    3
066100000613     O                                           +2 'N° rec'
066200000613     O                                           +3 'Anteprima contenuto'
066300000616     O          E            rigadett    2
066400000613     O                       rrnum               +2
066500000621     O                       recko               +3
066600000616     O          E            findett     2
066700000613     O                       wrkdata
066800000613     O                       wrkora              +1
066900000613     O                       procname            +1
067000000616     O          E            findett     2
067100000613     O                                              'Cliente..................:'
067200000613     O                       VABCCM        z     +1
067300000616     O          E            findett     2
067400000613     O                                              'Riferimento Strategi.....:'
067500000613     O                       vlrhdl              +1
067600000616     O          E            findett     2
067700000613     O                                              'Giusti...................:'
067800010201     O                       §CTROKVB      2   +  1
067900000616     O          E            findett     2
068000000613     O                                              'Sbagliati e corretti.....:'
068100000613     O                       §CTRMO        2   +  1
068200000616     O          E            findett     2
068300000613     O                                              'Sbagliati e scartati.....:'
068400000613     O                       §CTRNO        2   +  1
068500000512** CMD - COMANDI CL
068600050805OVRPRTF FILE(TITVN6P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
068700050805OVRPRTF FILE(TITVN6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
068800050805DLTOVR FILE(TITVN6P TITVN6PS) LVL(*)
068900000512
069000000512
