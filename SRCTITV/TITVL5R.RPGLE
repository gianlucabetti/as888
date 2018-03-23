000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800050329     Ftitvl5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050329     Ftitvl5ps  O    f  198        PRINTER usropn
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
003500050329     D depspe          s              9    INZ(*blanks)
003600050329     D curspe          s              9    INZ(*blanks)
003700050329     D cntspe          s              9  0 INZ(*zeros)
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
009300050329     C                   if        not %open(titvl5ps)
009400050329     C                   open      titvl5ps
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
010700050329     C                   if        %open(titvl5ps)
010800000616     C                   except    findett
010900050329     C                   close     titvl5ps
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
012600050329     C                   if        not %open(titvl5p)
012700050329     C                   open      titvl5p
012800990915     C                   endif
012900990915     C*
013000990915     C                   except    riepilogo
013100990915     C*
013200050329     C                   if        %open(titvl5p)
013300050329     C                   close     titvl5p
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
018100050329     C                   eval      PiStr=%trim(%subst(vindta:393:9))
018200030411     C                   movel(p)  PiStr         curspe
018300030411     C                   add       1             cntspe
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
027200050329     C                   Z-ADD     0053087       VABCCM
027300050329     C                   Z-ADD     0053087       VATCCM
027400021017     C                   Z-ADD     vlrpoi        VABFGS
027500021017     C                   Z-ADD     vlrpoi        VATFGS
027600050329     C                   Z-ADD     005           VABLNP
027700050329     C                   Z-ADD     005           VATLNP
027800030226     C                   Z-ADD     000           VABCTR
027900021017     C                   MOVEL     '7Q'          VABCTM
028000050317     C                   MOVEL     'C'           VABTSP
028100050321     C                   MOVEL     '1'           VABCBO
028200020619     C* ... e poi verifico se sono stati passati come parametri
028300020619     C                   IF        vlrppt > *blanks
028400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028500020619     C                   EXSR      CHKNUM
028600020619     C                   IF        PiInt=*on
028700020619     C                   Z-ADD     PiVal         VABCCM
028800020619     C                   Z-ADD     PiVal         VATCCM
028900020619     C                   ENDIF
029000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029100020619     C                   EXSR      CHKNUM
029200020619     C                   IF        PiInt=*on
029300020619     C                   Z-ADD     PiVal         VABLNP
029400020619     C                   Z-ADD     PiVal         VATLNP
029500020619     C                   ENDIF
029600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029700020619     C                   EXSR      CHKNUM
029800020619     C                   IF        PiInt=*on
029900020619     C                   Z-ADD     PiVal         VABCTR
030000020619     C                   ENDIF
030100020619     C                   ENDIF
030200000801     C*
030300000801     C                   ENDSR
030400000801     C*----------------------------------------------------*
030500030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030600000801     C*----------------------------------------------------*
030700010201     C     IMPVAB        BEGSR
030800000801     C*
030900020305     C                   EXSR      INZVAR
031000020305     C                   EXSR      DEFCAM
031100010305     C*
031200030226     C                   Z-ADD     *zeros        errore            1 0
031300000830     C                   MOVEL     datcor        VABAAS
031400000830     C                   MOVE      datcor        VABMGS
031500030226     C*
031600050330     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
031700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031900020117     C* ==
032000050330     C                   EVAL      VABRD2=%trim(%subst(vindta:17+35:40-35))
032100050329     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
032200050329     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
032300050329     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
032400050329     C                   EVAL      VABRMA=%trim(%subst(vindta:187:15))
032500050318
032600050329     C                   IF        %trim(%subst(vindta:15:2)) = '01'
032700050329     C                   EVAL      VABFFD = 'S'
032800050318     C                   ENDIF
032900050317
033000030226     C* CAP
033100050329     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033200010201     C                   EXSR      CHKNUM
033300010201     C                   IF        PiInt=*on
033400010201     C                   Z-ADD     PiVal         Num5_0
033500021017     C                   MOVEL     Num5_0        VABCAD
033600010201     C                   ELSE
033700010201     C                   ADD       1             errore
033800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033900021017     C                             + ' ' + 'VABCAD'
034000010201     C                   ENDIF
034100031201     C* NSP/RMN
034200050329     C                   EVAL      PiStr=%trim(%subst(vindta:393:9))
034300010201     C                   EXSR      CHKNUM
034400050329     C                   IF        PiInt=*on
034500050317     C                   Z-ADD     PiVal         VABNSP
034600050317     C                   Z-ADD     PiVal         VATNSP
034700031201     C                   Z-ADD     PiVal         VABRMN
034800010201     C                   ELSE
034900050329     C                   ADD       1             errore
035000050329     C                   EVAL      vinmsg = %trimr(vinmsg)
035100031201     C                             + ' ' + 'VABNSP VATNSP VABRMN'
035200010201     C                   ENDIF
035300030226     C* NCL
035400050329     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
035500050317     C                   EXSR      CHKNUM
035600050317     C                   IF        PiInt=*on
035700050317     C                   Z-ADD     PiVal         VABNCL
035800050317     C                   ELSE
035900050317     C                   ADD       1             errore
036000050317     C                   EVAL      vinmsg = %trimr(vinmsg)
036100050317     C                             + ' ' + 'VABNCL'
036200050317     C                   ENDIF
036300030226     C* PKB
036400050329     C                   EVAL      PiStr=%trim(%subst(vindta:170:7))
036500050329     C                   EVAL      PiDecChr = '.'
036600030107     C                   EXSR      CHKNUM
036700030107     C                   IF        PiNum=*on
036800050317     C                   Z-ADD     PiVal         VABPKB
036900030107     C                   ELSE
037000030107     C                   ADD       1             errore
037100030107     C                   EVAL      vinmsg = %trimr(vinmsg)
037200030107     C                             + ' ' + 'VABPKB'
037300030107     C                   ENDIF
037400050317     C* CAS/VCA
037500050329     C                   IF        %subst(vindta:280:12) <> '000000000,00'
037600050321     C                   MOVEL     '1'           FlgCAS
037700050329     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
037800050329     C                   EVAL      PiDecChr = ','
037900050317     C                   EXSR      CHKNUM
038000050317     C                   IF        PiNum=*on
038100050321     C                   Z-ADD     PiVal         VABCAS
038200050329     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
038300050317     C                   ELSE
038400050317     C                   SETON                                        32
038500050317     C                   Z-ADD     *zeros        VABCAS
038600050317     C                   EVAL      vinmsg = %trimr(vinmsg)
038700050317     C                             + ' ' + 'VABCAS'
038800050317     C                   ENDIF
038900050321     C                   ENDIF
039000050329     C* IAS/VAS
039100050329     C                   IF        %subst(vindta:295:12) <> '000000000,00'
039200050329     C                   EVAL      PiDecChr = ','
039300050329     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
039400050329     C                   EXSR      CHKNUM
039500050329     C                   IF        PiNum=*on
039600050329     C                   Z-ADD     PiVal         VABIAS
039700050329     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
039800050329     C                   ELSE
039900050329     C                   SETON                                        32
040000050329     C                   Z-ADD     *zeros        VABIAS
040100050329     C                   EVAL      vinmsg = %trimr(vinmsg)
040200050329     C                             + ' ' + 'VABIAS'
040300050329     C                   ENDIF
040400050329     C                   ENDIF
040500030310     C*
040600050321     C* Considerazioni finali su CBO/CAS
040700030310     C                   IF        FlgCAS = '1'
040800050321     C                   IF        VABCBO = '1'
040900010205     C                   EVAL      VABCBO = '4'
041000050321     C                   ENDIF
041100050321     C                   IF        VABCBO = '2'
041200050321     C                   EVAL      VABCBO = '6'
041300050321     C                   ENDIF
041400010205     C                   ENDIF
041500020305     C*
041600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041700011113     C                   EXSR      CHKIMPDIV
041800010202     C*
041900000801     C* Ebbene...
042000000801     C                   ADD       1             §CTRMO
042100010201     C                   IF        errore <> *zeros
042200000801     C                   ADD       1             §CTRNO
042300000801     C                   EVAL      recko = vindta
042400000801     C                   EXCEPT    rigadett
042500000801     C                   EVAL      vinflg = '2'
042600000801     C                   ELSE
042700010201     C                   ADD       1             §CTROKVB
042800000801     C                   ENDIF
042900000801     C*
043000000801     C                   ENDSR
043100030226     C*----------------------------------------------------*
043200030226     C*  SCARICAMENTO BUFFER RECORDS VAB
043300030226     C*----------------------------------------------------*
043400030226     C     WRIVAB        BEGSR
043500030310     C*
043600030310     C                   write     FIVAB000
043700030226     C*
043800030226     C                   ENDSR
043900030226     C*----------------------------------------------------*
044000030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
044100030226     C*----------------------------------------------------*
044200031201     C     EXEVAT        BEGSR
044300030226     C*
044400030226     C                   EXSR      INZVAR
044500030226     C                   EXSR      DEFCAM
044600030226     C*
044700030226     C                   MOVEL     datcor        VATAAS
044800030226     C*
044900030415     C* Valorizzo il barcode
045000031201     C                   EVAL      VATTRC='E'
045100050329     C                   EVAL      VATNOT=%trim(%subst(vindta:331:15))
045200031201     C                   exsr      wrivat                                       => scarico VAT
045300050329     C*
045400050329     C* Valorizzo il referente sempre xchè è il destinatario della bolla
045500050329     C                   EVAL      VATTRC='A'
045600050329     C                   EVAL      VATNOT=%trim(%subst(vindta:17:40))
045700050329     C                   exsr      wrivat                                       => scarico VAT
045800050329     C*
045900050329     C* Valorizzo il telefono destinatario (se valorizzato)
046000050329     C                   IF        %subst(vindta:92:15) <> *blanks
046100050329     C                   EVAL      VATTRC='B'
046200050329     C                   EVAL      VATNOT=%trim(%subst(vindta:92:15))
046300050329     C                   exsr      wrivat                                       => scarico VAT
046400050329     C                   ENDIF
046500030226     C*
046600030226     C                   ENDSR
046700010201     C*----------------------------------------------------*
046800021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046900010201     C*----------------------------------------------------*
047000020305     C     WRIVAT        BEGSR
047100010201     C*
047200021017     C                   write     FIVAT000
047300010201     C*
047400010201     C                   ENDSR
047500010202     C*----------------------------------------------------*
047600021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047700010202     C*----------------------------------------------------*
047800020305     C     PREVAT        BEGSR
047900010202     C*
048000021017     C* Compongo il nome del membro da dare al FIVATWWR
048100010202     C                   eval      parmbr = vlrhdl
048200010202     C                   movel     'M'           parmbr
048300050317     C                   eval      parccm = %subst(vlrKSC:2:7)
048400010202     C                   eval      paropz = '1'
048500010202     C* Effettuo la chiamata al CLLE preposto
048600031201     C                   call(e)   'TITVVTC'
048700010202     C                   parm                    parccm
048800010202     C                   parm                    parmbr
048900010202     C                   parm                    paropz
049000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049100010202     C                   if        %error
049200010202     C                   movel     '1'           chkcall
049300010202     C                   else
049400010202     C                   movel     '0'           chkcall
049500010202     C                   endif
049600010202     C*
049700010202     C                   ENDSR
049800000801     C*----------------------------------------------------*
049900000801     C*  CONTROLLO NUMERICITA' CAMPI
050000000801     C*----------------------------------------------------*
050100000801     C     CHKNUM        BEGSR
050200050329
050300050329     C                   IF        PiDecChr = *blanks
050400050329     C                   EVAL      PiDecChr = ','
050500050329     C                   ENDIF
050600000801     C*
050700000801     C                   call(e)   'ISNUMERIC'
050800000801     C                   PARM                    PiStr            30
050900050329     C                   PARM                    PiDecChr          1
051000000801     C                   PARM      *ZEROS        PiVal            30 9
051100000801     C                   PARM      '0'           PiInt             1
051200000801     C                   PARM      '0'           PiNum             1
051300000801     C                   IF        %error
051400000801     C                   EVAL      PiInt=*off
051500030107     C                   EVAL      PiNum=*off
051600000801     C                   ENDIF
051700000801     C*
051800000801     C                   ENDSR
051900000801     C***
052000000801
052100011113
052200011113     C*----------------------------------------------------*
052300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052400011113     C*----------------------------------------------------*
052500011113     C     CHKIMPDIV     BEGSR
052600011113     C*
052700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052800011113     C                   Z-ADD     *zeros        wrkDec            9 9
052900011113     C*
053000011113     C* Come prima cosa effettuo considerazioni sulla divisa
053100011113     C                   IF        vabIAS > *zeros
053200011113     C                   IF        vabVAS <> 'EUR'
053300011113     C                   EVAL      vabVAS =  'ITL'
053400011113     C                   ENDIF
053500011113     C                   ENDIF
053600011113     C*
053700011113     C                   IF        vabCAS > *zeros
053800011113     C                   IF        vabVCA <> 'EUR'
053900011113     C                   EVAL      vabVCA =  'ITL'
054000011113     C                   ENDIF
054100011113     C                   ENDIF
054200011113     C*
054300011113     C                   IF        vabVMD > *zeros
054400020305     C                   IF        vabVAD <> 'EUR'
054500011113     C                   EVAL      vabVAD =  'ITL'
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055000011113     C                   Z-ADD     vabIAS        wrkDec
055100011113     C                   IF        wrkDec > *zeros
055200011113     C                   IF        vabVAS = 'ITL'
055300011113     C                   EVAL      vabIAS = *zeros
055400011113     C                   ENDIF
055500011113     C                   ENDIF
055600011113     C*
055700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055800011113     C                   Z-ADD     vabCAS        wrkDec
055900011113     C                   IF        wrkDec > *zeros
056000011113     C                   IF        vabVCA = 'ITL'
056100011113     C                   EVAL      vabCAS = *zeros
056200011113     C                   ENDIF
056300011113     C                   ENDIF
056400011113     C*
056500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056600011113     C                   Z-ADD     vabVMD        wrkDec
056700011113     C                   IF        wrkDec > *zeros
056800011113     C                   IF        vabVAD = 'ITL'
056900011113     C                   EVAL      vabVMD = *zeros
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C                   ENDSR
057400011113     C***
057500021017
057600000801
057700000801
057800990920      /TITLE Invio dei dati al punto operativo.
057900010202     C     invio         BEGSR
058000990920     C*
058100021017     C* 1° invio FIVAT
058200010201     C                   reset                   dscmz
058300021017     C                   move      vlrpoi        cmzdst
058400021017     C                   eval      cmzfld = 'FIVATWWR'
058500010201     C                   eval      cmzmbd = vlrhdl
058600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058700021017     C                   eval      cmzfla = 'FIVAT00F'
058800021017     C                   eval      cmzmba = 'FIVAT00F'
058900010201     C                   eval      cmznrr = *zeros
059000020305     C                   move      §ctrokvt      cmznrr
059100021018     C                   eval      cmzlba = vlrfl1
059200010201     C                   call(e)   'TIS711C'
059300010201     C                   parm                    dscmz
059400010201     C                   parm      *blanks       esito
059500010205     C                   if        %error
059600010205     C                             or cmzerr = '1'
059700010205     C                             or esito  = '1'
059800010205     C                   eval      wrkesito = '3'
059900010205     C                   else
060000010201     C*
060100021017     C* 2° invio FIVAB
060200010201     C                   reset                   dscmz
060300021017     C                   move      vlrpoi        cmzdst
060400010201     C                   eval      cmzfld = vlrfou
060500010201     C                   eval      cmzmbd = vlrhdl
060600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060700021017     C                   eval      cmzfla = 'FIVAB00F'
060800021017     C                   eval      cmzmba = 'FIVAB00F'
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
067200050329     Otitvl5p   E            riepilogo         2
067300990915     O                                              'Upload via Internet'
067400990915     O                                           +1 'Traduzione TIVIN00R -'
067500021017     O                                           +1 'FIVABWWR/FIVATWWR'
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
069700050329     Otitvl5ps  E            testdett          2
069800000613     O                                              'Upload via Internet'
069900000613     O                                           +1 'Traduzione TIVIN00R -'
070000021017     O                                           +1 'FIVABWWR/FIVATWWR'
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
072800050329OVRPRTF FILE(TITVL5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
072900050329OVRPRTF FILE(TITVL5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
073000050329DLTOVR FILE(TITVL5P TITVL5PS) LVL(*)
073100000512
073200000512
