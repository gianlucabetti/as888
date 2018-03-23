000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030430     Ftitv51p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030430     Ftitv51ps  O    f  198        PRINTER usropn
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
003500030415     D depspe          s              7    INZ(*blanks)
003600030415     D curspe          s              7    INZ(*blanks)
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
009300030430     C                   if        not %open(titv51ps)
009400030430     C                   open      titv51ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700000613     C*
009800030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200050128     C                   call(e)   'TITVVTC'
010300010202     C                   parm                    parccm
010400010202     C                   parm                    parmbr
010500010202     C                   parm      '2'           paropz
010600010202     C*
010700030430     C                   if        %open(titv51ps)
010800000616     C                   except    findett
010900030430     C                   close     titv51ps
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
012600030430     C                   if        not %open(titv51p)
012700030430     C                   open      titv51p
012800990915     C                   endif
012900990915     C*
013000990915     C                   except    riepilogo
013100990915     C*
013200030430     C                   if        %open(titv51p)
013300030430     C                   close     titv51p
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
018100090826     C                   eval      PiStr=%trim(%subst(vindta:318:6))
018200030411     C                   movel(p)  PiStr         curspe
018300030411     C                   add       1             cntspe
018400030411     C*
018500030411     C                   if        cntspe = 1
018600030411     C* A rottura inizializzo campi di totalizzazione
018700030411     C                   clear                   fivab000
018800030411     C*
018900030411     C                   exsr      impvab                                       => carico VAB
019000030411     C                   exsr      impvat                                       => carico VAT
019100030411     C                   eval      depspe = curspe
019200030411     C                   else
019300030411     C                   if        curspe <> depspe
019400030411     C                   exsr      wrivab                                       => scarico VAB
019500030411     C                   exsr      wrivat                                       => scarico VAT
019600030411     C* A rottura inizializzo campi di totalizzazione
019700030411     C                   clear                   fivab000
019800030411     C*
019900030411     C                   exsr      impvab                                       => carico VAB
020000030411     C                   exsr      impvat                                       => carico VAT
020100030411     C                   eval      depspe = curspe
020200030411     C                   else
020300030411     C                   exsr      wrivat                                       => scarico VAT
020400030411     C                   exsr      impvab                                       => carico VAB
020500030411     C                   exsr      impvat                                       => carico VAT
020600030411     C                   endif
020700030411     C                   endif
020800000905     C*
020900000905     C                   else
021000000905     C                   eval      vinflg = '1'
021100000905     C                   endif
021200030411     C                   endif
021300000905     C*
021400000905     C  N70              update    tivin000
021500000905     C*
021600991022     C  N70              ENDdo
021700030430     C*
021800030430     C* Al termine della lettura scarico i VAB/VAT ancora in "canna"
021900030430     C                   exsr      wrivab                                       => scarico VAB
022000030430     C                   exsr      wrivat                                       => scarico VAT
022100010202     C*
022200010202     C                   endif
022300990910
022400990910     C* Se non ci sono record con errori ...
022500000710     C                   if        §ctrno = 0
022600990910     C* ... restituisco esito OK.
022700990921     C                   eval      wrkesito = '0'
022800990910     C                   else
022900010201     C                   if        §ctrokvb > 0
023000990921     C                   eval      wrkesito = '1'
023100000710     C                   else
023200000710     C                   eval      wrkesito = '2'
023300990910     C                   endif
023400000710     C                   endif
023500990910     C*
023600990914     C                   if        %open(tivin00r)
023700990908     C                   close     tivin00r
023800990914     C                   endif
023900021017     C                   if        %open(fivabwwr)
024000021017     C                   close     fivabwwr
024100990914     C                   endif
024200021017     C                   if        %open(fivatwwr)
024300021017     C                   close     fivatwwr
024400010201     C                   endif
024500021017     C*
024600021017     C                   if        vlrpoi <> 999
024700021017     C                   eval      vlrpoi = vlrpoi
024800021017     C                   endif
024900990910     C*
025000010201     C                   if        §ctrokvb > 0
025100021017     C                             and vlrpoi > *zeros
025200010202     C                   exsr      invio
025300990920     C                   endif
025400990920     C*
025500910830     C                   ENDSR
025600000613     C***
025700990920
025800000801     C*----------------------------------------------------*
025900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026000000801     C*----------------------------------------------------*
026100010201     C     INZVAR        BEGSR
026200000801     C*
026300010201     C                   Z-ADD     *zeros        Num5_0
026400030107     C                   Z-ADD     *zeros        Num7_0
026500030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026600030310     C                   MOVEL     '0'           FlgCAS            1
026700000801     C*
026800000801     C                   ENDSR
026900000801     C*----------------------------------------------------*
027000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027100000801     C*----------------------------------------------------*
027200000801     C     DEFCAM        BEGSR
027300000801     C*
027400020619     C* Imposto i valori di default...
027500030430     C                   Z-ADD     0060681       VABCCM
027600030430     C                   Z-ADD     0060681       VATCCM
027700021017     C                   Z-ADD     vlrpoi        VABFGS
027800021017     C                   Z-ADD     vlrpoi        VATFGS
027900030430     C                   Z-ADD     006           VABLNP
028000030430     C                   Z-ADD     006           VATLNP
028100030430     C                   Z-ADD     300           VABCTR
028200021017     C                   MOVEL     '7Q'          VABCTM
028300020305     C                   MOVEL     'E'           VATTRC
028400020619     C* ... e poi verifico se sono stati passati come parametri
028500020619     C                   IF        vlrppt > *blanks
028600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028700020619     C                   EXSR      CHKNUM
028800020619     C                   IF        PiInt=*on
028900020619     C                   Z-ADD     PiVal         VABCCM
029000020619     C                   Z-ADD     PiVal         VATCCM
029100020619     C                   ENDIF
029200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029300020619     C                   EXSR      CHKNUM
029400020619     C                   IF        PiInt=*on
029500020619     C                   Z-ADD     PiVal         VABLNP
029600020619     C                   Z-ADD     PiVal         VATLNP
029700020619     C                   ENDIF
029800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029900020619     C                   EXSR      CHKNUM
030000020619     C                   IF        PiInt=*on
030100020619     C                   Z-ADD     PiVal         VABCTR
030200020619     C                   ENDIF
030300020619     C                   ENDIF
030400000801     C*
030500000801     C                   ENDSR
030600000801     C*----------------------------------------------------*
030700030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030800000801     C*----------------------------------------------------*
030900010201     C     IMPVAB        BEGSR
031000000801     C*
031100020305     C                   EXSR      INZVAR
031200020305     C                   EXSR      DEFCAM
031300010305     C*
031400030226     C                   Z-ADD     *zeros        errore            1 0
031500000830     C                   MOVEL     datcor        VABAAS
031600000830     C                   MOVE      datcor        VABMGS
031700030226     C*
031800030430     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
031900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032100020117     C* ==
032200030430     C                   EVAL      VABRD2=%trim(%subst(vindta:92:15))
032300030430     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
032400030430     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
032500030430     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
032600030430     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
032700030430     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
032800030430     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
032900030430     C                   IF        %subst(vindta:12:1) = 'D'
033000030430     C                   EVAL      VABTSP='E'
033100030430     C                   ENDIF
033200030430     C                   IF        %subst(vindta:16:1) = 'S'
033300030430     C                   EVAL      VABFFD='S'
033400030430     C                   ENDIF
033500090826     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
033600090826     C                   EVAL      VABNT2=%trim(%subst(vindta:(197+35):35))
033700030226     C* CAP
033800030430     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033900010201     C                   EXSR      CHKNUM
034000010201     C                   IF        PiInt=*on
034100010201     C                   Z-ADD     PiVal         Num5_0
034200021017     C                   MOVEL     Num5_0        VABCAD
034300010201     C                   ELSE
034400010201     C                   ADD       1             errore
034500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034600021017     C                             + ' ' + 'VABCAD'
034700010201     C                   ENDIF
034800030311     C* NSP
034900090826     C                   EVAL      PiStr=%trim(%subst(vindta:318:6))
035000010201     C                   EXSR      CHKNUM
035100010201     C                   IF        PiInt=*on
035200030107     C                   Z-ADD     PiVal         Num7_0
035300030107     C                   MOVE(P)   Num7_0        VABNSP
035400030107     C                   MOVE(P)   Num7_0        VATNSP
035500010201     C                   ELSE
035600010201     C                   ADD       1             errore
035700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035800030311     C                             + ' ' + 'VABNSP VATNSP'
035900010201     C                   ENDIF
036000030314     C* RMN
036100090826     C                   EVAL      PiStr=%trim(%subst(vindta:190:6))
036200030311     C                   EXSR      CHKNUM
036300030311     C                   IF        PiInt=*on
036400030311     C                   Z-ADD     PiVal         VABRMN
036500030311     C                   ELSE
036600030311     C                   ADD       1             errore
036700030311     C                   EVAL      vinmsg = %trimr(vinmsg)
036800030311     C                             + ' ' + 'VABRMN'
036900030311     C                   ENDIF
037000030226     C* NCL
037100030430     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
037200030430     C                   EXSR      CHKNUM
037300030430     C                   IF        PiInt=*on
037400030430     C                   Z-ADD     PiVal         VABNCL
037500030430     C                   ELSE
037600030430     C                   ADD       1             errore
037700030430     C                   EVAL      vinmsg = %trimr(vinmsg)
037800030430     C                             + ' ' + 'VABNCL'
037900030430     C                   ENDIF
038000030226     C* PKB
038100030430     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
038200030107     C                   EXSR      CHKNUM
038300030107     C                   IF        PiNum=*on
038400030430     C                   Z-ADD     PiVal         VABPKB
038500030107     C                   ELSE
038600030107     C                   ADD       1             errore
038700030107     C                   EVAL      vinmsg = %trimr(vinmsg)
038800030107     C                             + ' ' + 'VABPKB'
038900030107     C                   ENDIF
039000030430     C* CAS
039100090827     C                   IF        %subst(vindta:280:12) <> *blanks AND
039200090827     C                             %trim(%subst(vindta:280:12)) <> *zeros
039300030310     C                   MOVEL     '1'           FlgCAS
039400030430     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
039500030310     C                   EXSR      CHKNUM
039600030310     C                   IF        PiNum=*on
039700030310     C                   Z-ADD     PiVal         VABCAS
039800050603     C                   EVAL      VABTIC = 'BM'
039900030310     C                   ELSE
040000030310     C                   SETON                                        32
040100030310     C                   Z-ADD     *zeros        VABCAS
040200030310     C                   EVAL      vinmsg = %trimr(vinmsg)
040300030310     C                             + ' ' + 'VABCAS'
040400030310     C                   ENDIF
040500030310     C                   ENDIF
040600030430     C* IAS
040700090826     C                   IF        %subst(vindta:295:12) <> *blanks
040800030430     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
040900030430     C                   EXSR      CHKNUM
041000030430     C                   IF        PiNum=*on
041100030430     C                   Z-ADD     PiVal         VABIAS
041200030430     C                   ELSE
041300030430     C                   SETON                                        32
041400030430     C                   Z-ADD     *zeros        VABIAS
041500030430     C                   EVAL      vinmsg = %trimr(vinmsg)
041600030430     C                             + ' ' + 'VABIAS'
041700030430     C                   ENDIF
041800090826     C                   ENDIF
041900030310     C*
042000030310     C* Considerazioni finali su CBO/CAS
042100030310     C                   IF        FlgCAS = '1'
042200010205     C                   EVAL      VABCBO = '4'
042300010205     C                   ELSE
042400010205     C                   EVAL      VABCBO = '1'
042500010205     C                   ENDIF
042600020305     C*
042700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042800011113     C                   EXSR      CHKIMPDIV
042900010202     C*
043000000801     C* Ebbene...
043100000801     C                   ADD       1             §CTRMO
043200010201     C                   IF        errore <> *zeros
043300000801     C                   ADD       1             §CTRNO
043400000801     C                   EVAL      recko = vindta
043500000801     C                   EXCEPT    rigadett
043600000801     C                   EVAL      vinflg = '2'
043700000801     C                   ELSE
043800010201     C                   ADD       1             §CTROKVB
043900000801     C                   ENDIF
044000000801     C*
044100000801     C                   ENDSR
044200030226     C*----------------------------------------------------*
044300030226     C*  SCARICAMENTO BUFFER RECORDS VAB
044400030226     C*----------------------------------------------------*
044500030226     C     WRIVAB        BEGSR
044600030310     C*
044700030310     C                   write     FIVAB000
044800030226     C*
044900030226     C                   ENDSR
045000030226     C*----------------------------------------------------*
045100030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
045200030226     C*----------------------------------------------------*
045300030226     C     IMPVAT        BEGSR
045400030226     C*
045500030226     C                   EXSR      INZVAR
045600030226     C                   EXSR      DEFCAM
045700030226     C*
045800030226     C                   MOVEL     datcor        VATAAS
045900030226     C*
046000030415     C* Valorizzo il barcode
046100090827     C                   MOVEL     *blanks       depCHISONO        9
046200090827     C                   EVAL      depCHISONO=%trim(%subst(vindta:331:9))
046300030226     C*
046400030226     C                   ENDSR
046500010201     C*----------------------------------------------------*
046600021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046700010201     C*----------------------------------------------------*
046800020305     C     WRIVAT        BEGSR
046900010201     C*
047000090827     C* Ciclo sul numero d colli reperito e creo tanti "CHI SONO" x ogni collo
047100090827     C                   move(p)   depCHISONO    wNumColloDA       5 0
047200090827     C                   eval      wNumColloDA = wNumColloDA - 1
047300090827     C                   z-add     1             wNCL              5 0
047400090827     C                   dow       wNCL <= VABNCL
047500090827     C                   eval      wNumColloDA = wNumColloDA + 1
047600090827     C                   move(p)   wNumColloDA   wNCLalfa          5
047700090827     C                   eval      VATNOT = %subst(depCHISONO:1:4) +
047800090827     C                                      wNCLalfa
047900021017     C                   write     FIVAT000
048000050725     C                   add       1             wNCL
048100050725     C                   enddo
048200010201     C*
048300010201     C                   ENDSR
048400010202     C*----------------------------------------------------*
048500021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048600010202     C*----------------------------------------------------*
048700020305     C     PREVAT        BEGSR
048800010202     C*
048900021017     C* Compongo il nome del membro da dare al FIVATWWR
049000010202     C                   eval      parmbr = vlrhdl
049100010202     C                   movel     'M'           parmbr
049200030430     C                   eval      parccm = '00060681'
049300010202     C                   eval      paropz = '1'
049400010202     C* Effettuo la chiamata al CLLE preposto
049500050128     C                   call(e)   'TITVVTC'
049600010202     C                   parm                    parccm
049700010202     C                   parm                    parmbr
049800010202     C                   parm                    paropz
049900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050000010202     C                   if        %error
050100010202     C                   movel     '1'           chkcall
050200010202     C                   else
050300010202     C                   movel     '0'           chkcall
050400010202     C                   endif
050500010202     C*
050600010202     C                   ENDSR
050700000801     C*----------------------------------------------------*
050800000801     C*  CONTROLLO NUMERICITA' CAMPI
050900000801     C*----------------------------------------------------*
051000000801     C     CHKNUM        BEGSR
051100000801     C*
051200000801     C                   call(e)   'ISNUMERIC'
051300000801     C                   PARM                    PiStr            30
051400090826     C                   PARM      ','           PiDecChr          1
051500000801     C                   PARM      *ZEROS        PiVal            30 9
051600000801     C                   PARM      '0'           PiInt             1
051700000801     C                   PARM      '0'           PiNum             1
051800000801     C                   IF        %error
051900000801     C                   EVAL      PiInt=*off
052000030107     C                   EVAL      PiNum=*off
052100000801     C                   ENDIF
052200000801     C*
052300000801     C                   ENDSR
052400000801     C***
052500000801
052600011113
052700011113     C*----------------------------------------------------*
052800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052900011113     C*----------------------------------------------------*
053000011113     C     CHKIMPDIV     BEGSR
053100011113     C*
053200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053300011113     C                   Z-ADD     *zeros        wrkDec            9 9
053400011113     C*
053500011113     C* Come prima cosa effettuo considerazioni sulla divisa
053600011113     C                   IF        vabIAS > *zeros
053700011113     C                   IF        vabVAS <> 'EUR'
053800011113     C                   EVAL      vabVAS =  'ITL'
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C                   IF        vabCAS > *zeros
054300011113     C                   IF        vabVCA <> 'EUR'
054400011113     C                   EVAL      vabVCA =  'ITL'
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C                   IF        vabVMD > *zeros
054900020305     C                   IF        vabVAD <> 'EUR'
055000011113     C                   EVAL      vabVAD =  'ITL'
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055500011113     C                   Z-ADD     vabIAS        wrkDec
055600011113     C                   IF        wrkDec > *zeros
055700011113     C                   IF        vabVAS = 'ITL'
055800011113     C                   EVAL      vabIAS = *zeros
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056300011113     C                   Z-ADD     vabCAS        wrkDec
056400011113     C                   IF        wrkDec > *zeros
056500011113     C                   IF        vabVCA = 'ITL'
056600011113     C                   EVAL      vabCAS = *zeros
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057100011113     C                   Z-ADD     vabVMD        wrkDec
057200011113     C                   IF        wrkDec > *zeros
057300011113     C                   IF        vabVAD = 'ITL'
057400011113     C                   EVAL      vabVMD = *zeros
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C                   ENDSR
057900011113     C***
058000021017
058100000801
058200000801
058300990920      /TITLE Invio dei dati al punto operativo.
058400010202     C     invio         BEGSR
058500990920     C*
058600021017     C* 1° invio FIVAT
058700010201     C                   reset                   dscmz
058800021017     C                   move      vlrpoi        cmzdst
058900021017     C                   eval      cmzfld = 'FIVATWWR'
059000010201     C                   eval      cmzmbd = vlrhdl
059100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059200021009     C***                if        prmfir = *blanks
059300021017     C                   eval      cmzfla = 'FIVAT00F'
059400021017     C                   eval      cmzmba = 'FIVAT00F'
059500021009     C***                else
059600021009     C***                eval      cmzfla = prmfir
059700021009     C***                eval      cmzmba = prmfir
059800021009     C***                endif
059900010201     C                   eval      cmznrr = *zeros
060000020305     C                   move      §ctrokvt      cmznrr
060100021018     C                   eval      cmzlba = vlrfl1
060200010201     C                   call(e)   'TIS711C'
060300010201     C                   parm                    dscmz
060400010201     C                   parm      *blanks       esito
060500010205     C                   if        %error
060600010205     C                             or cmzerr = '1'
060700010205     C                             or esito  = '1'
060800010205     C                   eval      wrkesito = '3'
060900010205     C                   else
061000010201     C*
061100021017     C* 2° invio FIVAB
061200010201     C                   reset                   dscmz
061300021017     C                   move      vlrpoi        cmzdst
061400010201     C                   eval      cmzfld = vlrfou
061500010201     C                   eval      cmzmbd = vlrhdl
061600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061700021009     C***                if        prmfir = *blanks
061800021017     C                   eval      cmzfla = 'FIVAB00F'
061900021017     C                   eval      cmzmba = 'FIVAB00F'
062000021009     C***                else
062100021009     C***                eval      cmzfla = prmfir
062200021009     C***                eval      cmzmba = prmfir
062300021009     C***                endif
062400010201     C                   eval      cmznrr = *zeros
062500010201     C                   move      §ctrokvb      cmznrr
062600021018     C                   eval      cmzlba = vlrfl1
062700010201     C                   call(e)   'TIS711C'
062800010201     C                   parm                    dscmz
062900010201     C                   parm      *blanks       esito
063000010201     C                   if        %error
063100010201     C                             or cmzerr = '1'
063200010201     C                             or esito  = '1'
063300010201     C                   eval      wrkesito = '3'
063400010201     C                   endif
063500010205     C                   endif
063600990920     C*
063700000613     C                   ENDSR
063800000613     C***
063900070411
064000070411     C     *pssr         BEGSR
064100070411     C*
064200070411     C                   if        %open(tivin00r)
064300070411     C                   close     tivin00r
064400070411     C                   endif
064500070411     C                   if        %open(fivabwwr)
064600070411     C                   close     fivabwwr
064700070411     C                   endif
064800070411     C                   if        %open(fivatwwr)
064900070411     C                   close     fivatwwr
065000070411     C                   endif
065100070411     C*
065200070411     C* Effettuo la chiamata al CLLE preposto
065300070411     C                   call(e)   'TITVVTC'
065400070411     C                   parm                    parccm
065500070411     C                   parm                    parmbr
065600070411     C                   parm      '2'           paropz
065700070411     C*
065800070411     C                   eval      wrkesito = '2'
065900070411     C*
066000070411     C                   seton                                        LR
066100070411     C*
066200070411     C                   ENDSR     '*CANCL'
066300070411     C***
066400070411
066500990910
066600000613     C     *inzsr        BEGSR
066700990910     C*
066800990910     C     *entry        plist
066900990920     C                   parm                    tivlrds
067000990921     C                   parm      wrkesito      esito
067100000724     C                   parm                    prmlit
067200000710     C                   parm                    prmfir
067300000613     C*
067400000830     C* CALCOLA LA DATA CORRENTE
067500000830     C                   time                    wn14             14 0
067600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067700000830     C                   z-add     wn8           g08dat
067800000830     C                   z-add     *zeros        g08inv
067900000830     C                   movel     '0'           g08err
068000000830     C                   call      'XSRDA8'
068100000830     C                   parm                    wlbda8
068200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068300000830     C*
068400000613     C                   ENDSR
068500000613     C***
068600990908
068700030430     Otitv51p   E            riepilogo         2
068800990915     O                                              'Upload via Internet'
068900990915     O                                           +1 'Traduzione TIVIN00R -'
069000021017     O                                           +1 'FIVABWWR/FIVATWWR'
069100010201     O                                           +1 'Report testate bolle'
069200990915     O          E            riepilogo   2
069300990915     O                       wrkdata
069400990915     O                       wrkora              +1
069500990915     O                       procname            +1
069600990915     O          E            riepilogo   2
069700990915     O                                              'Cliente..................:'
069800990915     O                       VABCCM        z     +1
069900990915     O          E            riepilogo   2
070000990920     O                                              'Riferimento Strategi.....:'
070100990920     O                       vlrhdl              +1
070200990915     O          E            riepilogo   2
070300990915     O                                              'Giusti...................:'
070400010201     O                       §CTROKVB      2   +  1
070500990915     O          E            riepilogo   2
070600990915     O                                              'Sbagliati e corretti.....:'
070700971022     O                       §CTRMO        2   +  1
070800990915     O          E            riepilogo   2
070900990915     O                                              'Sbagliati e scartati.....:'
071000971022     O                       §CTRNO        2   +  1
071100000613
071200030430     Otitv51ps  E            testdett          2
071300000613     O                                              'Upload via Internet'
071400000613     O                                           +1 'Traduzione TIVIN00R -'
071500021017     O                                           +1 'FIVABWWR/FIVATWWR'
071600010201     O                                           +1 'Report testate bolle'
071700000616     O          E            testdett    3
071800000613     O                                           +2 'N° rec'
071900000613     O                                           +3 'Anteprima contenuto'
072000000616     O          E            rigadett    2
072100000613     O                       rrnum               +2
072200000621     O                       recko               +3
072300000616     O          E            findett     2
072400000613     O                       wrkdata
072500000613     O                       wrkora              +1
072600000613     O                       procname            +1
072700000616     O          E            findett     2
072800000613     O                                              'Cliente..................:'
072900000613     O                       VABCCM        z     +1
073000000616     O          E            findett     2
073100000613     O                                              'Riferimento Strategi.....:'
073200000613     O                       vlrhdl              +1
073300000616     O          E            findett     2
073400000613     O                                              'Giusti...................:'
073500010201     O                       §CTROKVB      2   +  1
073600000616     O          E            findett     2
073700000613     O                                              'Sbagliati e corretti.....:'
073800000613     O                       §CTRMO        2   +  1
073900000616     O          E            findett     2
074000000613     O                                              'Sbagliati e scartati.....:'
074100000613     O                       §CTRNO        2   +  1
074200000512** CMD - COMANDI CL
074300030430OVRPRTF FILE(TITV51P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
074400030430OVRPRTF FILE(TITV51PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074500030430DLTOVR FILE(TITV51P TITV51PS) LVL(*)
074600000512
074700000512
