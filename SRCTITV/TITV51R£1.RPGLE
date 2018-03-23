000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030430     Ftitv51p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000030430     Ftitv51ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200990908
001300000512     D*------------
001400000512     D* COMANDI
001500000512     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000801     D*----------------------------------------------------
001800000801     D* DICHIARAZIOINE VARIABILI DI WRK
001900000801     D*----------------------------------------------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300990920     D tivlrds       e ds                  extname(tivlr00f)
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800990915     D wrkdata         s               d
002900990915     D wrkora          s               t
003000000613     D rrnum           s              6  0 INZ(*zeros)
003100000621     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300030415     D depspe          s              7    INZ(*blanks)
003400030415     D curspe          s              7    INZ(*blanks)
003500030415     D cntspe          s              7  0 INZ(*zeros)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000010213
004100000801     D  Num5_0         s              5  0
004200030107     D  Num7_0         s              7  0
004300000830
004400000830     D*------------------
004500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004600000830     D*------------------
004700000830     D WLBDA8          DS                  INZ
004800000830     D  G08DAT                 1      8  0
004900000830     D  G08INV                 9     16  0
005000000830     D  G08ERR                17     17
005100000830     D  G08TGI                18     22  0
005200000830     D*
005300990908
005400010201
005500010201
005600990915     C                   time                    wrkdata
005700990915     C                   time                    wrkora
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100000724     C*
006200000724     C* SE OCCORRE SPEDIRE IN FILIALE
006300021017     C                   if        vlrpoi <> *zeros
006400000724     C*
006500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006700021017     C     vlrpoi        chain     azorg01l
006800000724     C                   if        %found
006900000616     C                   movel(p)  CMD(1)        depcmd
007000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007100000616     C*
007200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007300011113     C                   Z-ADD     150           LENGH            15 5
007400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007500000616     C                   PARM                    depcmd
007600000616     C                   PARM                    LENGH
007700000724     C*
007800000724     C                   endif
007900000724     C                   endif
008000000616     C*
008100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008200000616     C                   movel(p)  CMD(2)        depcmd
008300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008400000616     C*
008500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008600011113     C                   Z-ADD     150           LENGH            15 5
008700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008800000616     C                   PARM                    depcmd
008900000616     C                   PARM                    LENGH
009000000616     C*
009100030430     C                   if        not %open(titv51ps)
009200030430     C                   open      titv51ps
009300000616     C                   except    testdett
009400000613     C                   endif
009500000613     C*
009600030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009800000613     C*
009900010202     C* Effettuo la chiamata al CLLE preposto
010000050128     C                   call(e)   'TITVVTC'
010100010202     C                   parm                    parccm
010200010202     C                   parm                    parmbr
010300010202     C                   parm      '2'           paropz
010400010202     C*
010500030430     C                   if        %open(titv51ps)
010600000616     C                   except    findett
010700030430     C                   close     titv51ps
010800000613     C                   endif
010900000616     C*
011000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011100011113     C                   Z-ADD     150           LENGH            15 5
011200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011300000616     C                   PARM                    CMD(3)
011400000616     C                   PARM                    LENGH
011500000616     C*
011600010201     C                   seton                                        LR
011700000613
011800000613
011900000613     C*--------------------------------------------------------
012000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012100000613     C*--------------------------------------------------------
012200000621     C     STPR          BEGSR
012300000613     C*
012400030430     C                   if        not %open(titv51p)
012500030430     C                   open      titv51p
012600990915     C                   endif
012700990915     C*
012800990915     C                   except    riepilogo
012900990915     C*
013000030430     C                   if        %open(titv51p)
013100030430     C                   close     titv51p
013200990914     C                   endif
013300990910     C*
013400000613     C                   ENDSR
013500000613     C***
013600990908
013700000801
013800910830     C*--------------------------------------------------------
013900030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
014000910830     C*--------------------------------------------------------
014100030226     C     RWFILE        BEGSR
014200990910     C*
014300990914     C                   if        not %open(tivin00r)
014400990908     C                   open      tivin00r
014500990914     C                   endif
014600021017     C                   if        not %open(fivabwwr)
014700021017     C                   open      fivabwwr
014800990914     C                   endif
014900021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015000020305     C                   exsr      prevat
015100010201     C*
015200010202     C                   if        chkcall = '0'
015300010202     C*
015400021017     C                   if        not %open(fivatwwr)
015500021017     C                   open      fivatwwr
015600010201     C                   endif
015700990910     C*
015800010201     C                   clear                   §CTROKVB          5 0
015900020305     C                   clear                   §CTROKVT          5 0
016000000801     C                   clear                   §CTRMO            5 0
016100000801     C                   clear                   §CTRNO            5 0
016200990910     C*
016300921023     C                   DO        *HIVAL
016400990913     C*
016500990915     C                   READ      tivin00r                               70
016600000905     C                   if        vindta > *blanks
016700000613     C                   add       1             rrnum
016800000801     C*
016900000801     C                   if        *in70 = *off
017000000801     C                             and
017100000801     C                             (vinflg = *blanks
017200000801     C                              or vinflg = '0'
017300000801     C                              or vinflg = '2')
017400000801     C*
017500000801     C                   clear                   vinmsg
017600030310     C                   eval      vinflg = '1'
017700030310     C*
017800030310     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017900030430     C                   eval      PiStr=%trim(%subst(vindta:315:16))
018000030411     C                   movel(p)  PiStr         curspe
018100030411     C                   add       1             cntspe
018200030411     C*
018300030411     C                   if        cntspe = 1
018400030411     C* A rottura inizializzo campi di totalizzazione
018500030411     C                   clear                   fivab000
018600030411     C*
018700030411     C                   exsr      impvab                                       => carico VAB
018800030411     C                   exsr      impvat                                       => carico VAT
018900030411     C                   eval      depspe = curspe
019000030411     C                   else
019100030411     C                   if        curspe <> depspe
019200030411     C                   exsr      wrivab                                       => scarico VAB
019300030411     C                   exsr      wrivat                                       => scarico VAT
019400030411     C* A rottura inizializzo campi di totalizzazione
019500030411     C                   clear                   fivab000
019600030411     C*
019700030411     C                   exsr      impvab                                       => carico VAB
019800030411     C                   exsr      impvat                                       => carico VAT
019900030411     C                   eval      depspe = curspe
020000030411     C                   else
020100030411     C                   exsr      wrivat                                       => scarico VAT
020200030411     C                   exsr      impvab                                       => carico VAB
020300030411     C                   exsr      impvat                                       => carico VAT
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
021800030430     C                   exsr      wrivat                                       => scarico VAT
021900010202     C*
022000010202     C                   endif
022100990910
022200990910     C* Se non ci sono record con errori ...
022300000710     C                   if        §ctrno = 0
022400990910     C* ... restituisco esito OK.
022500990921     C                   eval      wrkesito = '0'
022600990910     C                   else
022700010201     C                   if        §ctrokvb > 0
022800990921     C                   eval      wrkesito = '1'
022900000710     C                   else
023000000710     C                   eval      wrkesito = '2'
023100990910     C                   endif
023200000710     C                   endif
023300990910     C*
023400990914     C                   if        %open(tivin00r)
023500990908     C                   close     tivin00r
023600990914     C                   endif
023700021017     C                   if        %open(fivabwwr)
023800021017     C                   close     fivabwwr
023900990914     C                   endif
024000021017     C                   if        %open(fivatwwr)
024100021017     C                   close     fivatwwr
024200010201     C                   endif
024300021017     C*
024400021017     C                   if        vlrpoi <> 999
024500021017     C                   eval      vlrpoi = vlrpoi
024600021017     C                   endif
024700990910     C*
024800010201     C                   if        §ctrokvb > 0
024900021017     C                             and vlrpoi > *zeros
025000010202     C                   exsr      invio
025100990920     C                   endif
025200990920     C*
025300910830     C                   ENDSR
025400000613     C***
025500990920
025600000801     C*----------------------------------------------------*
025700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025800000801     C*----------------------------------------------------*
025900010201     C     INZVAR        BEGSR
026000000801     C*
026100010201     C                   Z-ADD     *zeros        Num5_0
026200030107     C                   Z-ADD     *zeros        Num7_0
026300030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026400030310     C                   MOVEL     '0'           FlgCAS            1
026500000801     C*
026600000801     C                   ENDSR
026700000801     C*----------------------------------------------------*
026800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026900000801     C*----------------------------------------------------*
027000000801     C     DEFCAM        BEGSR
027100000801     C*
027200020619     C* Imposto i valori di default...
027300030430     C                   Z-ADD     0060681       VABCCM
027400030430     C                   Z-ADD     0060681       VATCCM
027500021017     C                   Z-ADD     vlrpoi        VABFGS
027600021017     C                   Z-ADD     vlrpoi        VATFGS
027700030430     C                   Z-ADD     006           VABLNP
027800030430     C                   Z-ADD     006           VATLNP
027900030430     C                   Z-ADD     300           VABCTR
028000021017     C                   MOVEL     '7Q'          VABCTM
028100020305     C                   MOVEL     'E'           VATTRC
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
031600030430     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
031700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031900020117     C* ==
032000030430     C                   EVAL      VABRD2=%trim(%subst(vindta:92:15))
032100030430     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
032200030430     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
032300030430     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
032400030430     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
032500030430     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
032600030430     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
032700030430     C                   IF        %subst(vindta:12:1) = 'D'
032800030430     C                   EVAL      VABTSP='E'
032900030430     C                   ENDIF
033000030430     C                   IF        %subst(vindta:16:1) = 'S'
033100030430     C                   EVAL      VABFFD='S'
033200030430     C                   ENDIF
033300030430     C                   EVAL      VABNOT=%trim(%subst(vindta:198:35))
033400030430     C                   EVAL      VABNT2=%trim(%subst(vindta:(198+35):35))
033500030226     C* CAP
033600030430     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033700010201     C                   EXSR      CHKNUM
033800010201     C                   IF        PiInt=*on
033900010201     C                   Z-ADD     PiVal         Num5_0
034000021017     C                   MOVEL     Num5_0        VABCAD
034100010201     C                   ELSE
034200010201     C                   ADD       1             errore
034300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034400021017     C                             + ' ' + 'VABCAD'
034500010201     C                   ENDIF
034600030311     C* NSP
034700030430     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
034800010201     C                   EXSR      CHKNUM
034900010201     C                   IF        PiInt=*on
035000030107     C                   Z-ADD     PiVal         Num7_0
035100030107     C                   MOVE(P)   Num7_0        VABNSP
035200030107     C                   MOVE(P)   Num7_0        VATNSP
035300010201     C                   ELSE
035400010201     C                   ADD       1             errore
035500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035600030311     C                             + ' ' + 'VABNSP VATNSP'
035700010201     C                   ENDIF
035800030314     C* RMN
035900030430     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
036000030311     C                   EXSR      CHKNUM
036100030311     C                   IF        PiInt=*on
036200030311     C                   Z-ADD     PiVal         VABRMN
036300030311     C                   ELSE
036400030311     C                   ADD       1             errore
036500030311     C                   EVAL      vinmsg = %trimr(vinmsg)
036600030311     C                             + ' ' + 'VABRMN'
036700030311     C                   ENDIF
036800030226     C* NCL
036900030430     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
037000030430     C                   EXSR      CHKNUM
037100030430     C                   IF        PiInt=*on
037200030430     C                   Z-ADD     PiVal         VABNCL
037300030430     C                   ELSE
037400030430     C                   ADD       1             errore
037500030430     C                   EVAL      vinmsg = %trimr(vinmsg)
037600030430     C                             + ' ' + 'VABNCL'
037700030430     C                   ENDIF
037800030226     C* PKB
037900030430     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
038000030107     C                   EXSR      CHKNUM
038100030107     C                   IF        PiNum=*on
038200030430     C                   Z-ADD     PiVal         VABPKB
038300030107     C                   ELSE
038400030107     C                   ADD       1             errore
038500030107     C                   EVAL      vinmsg = %trimr(vinmsg)
038600030107     C                             + ' ' + 'VABPKB'
038700030107     C                   ENDIF
038800030430     C* CAS
038900030430     C                   IF        %subst(vindta:280:12) <> *blanks
039000030310     C                   MOVEL     '1'           FlgCAS
039100030430     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
039200030310     C                   EXSR      CHKNUM
039300030310     C                   IF        PiNum=*on
039400030310     C                   Z-ADD     PiVal         VABCAS
039500050603     C                   EVAL      VABTIC = 'BM'
039600030310     C                   ELSE
039700030310     C                   SETON                                        32
039800030310     C                   Z-ADD     *zeros        VABCAS
039900030310     C                   EVAL      vinmsg = %trimr(vinmsg)
040000030310     C                             + ' ' + 'VABCAS'
040100030310     C                   ENDIF
040200030310     C                   ENDIF
040300030430     C* IAS
040400030430     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
040500030430     C                   EXSR      CHKNUM
040600030430     C                   IF        PiNum=*on
040700030430     C                   Z-ADD     PiVal         VABIAS
040800030430     C                   ELSE
040900030430     C                   SETON                                        32
041000030430     C                   Z-ADD     *zeros        VABIAS
041100030430     C                   EVAL      vinmsg = %trimr(vinmsg)
041200030430     C                             + ' ' + 'VABIAS'
041300030430     C                   ENDIF
041400030310     C*
041500030310     C* Considerazioni finali su CBO/CAS
041600030310     C                   IF        FlgCAS = '1'
041700010205     C                   EVAL      VABCBO = '4'
041800010205     C                   ELSE
041900010205     C                   EVAL      VABCBO = '1'
042000010205     C                   ENDIF
042100020305     C*
042200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042300011113     C                   EXSR      CHKIMPDIV
042400010202     C*
042500000801     C* Ebbene...
042600000801     C                   ADD       1             §CTRMO
042700010201     C                   IF        errore <> *zeros
042800000801     C                   ADD       1             §CTRNO
042900000801     C                   EVAL      recko = vindta
043000000801     C                   EXCEPT    rigadett
043100000801     C                   EVAL      vinflg = '2'
043200000801     C                   ELSE
043300010201     C                   ADD       1             §CTROKVB
043400000801     C                   ENDIF
043500000801     C*
043600000801     C                   ENDSR
043700030226     C*----------------------------------------------------*
043800030226     C*  SCARICAMENTO BUFFER RECORDS VAB
043900030226     C*----------------------------------------------------*
044000030226     C     WRIVAB        BEGSR
044100030310     C*
044200030310     C                   write     FIVAB000
044300030226     C*
044400030226     C                   ENDSR
044500030226     C*----------------------------------------------------*
044600030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
044700030226     C*----------------------------------------------------*
044800030226     C     IMPVAT        BEGSR
044900030226     C*
045000030226     C                   EXSR      INZVAR
045100030226     C                   EXSR      DEFCAM
045200030226     C*
045300030226     C                   MOVEL     datcor        VATAAS
045400030226     C*
045500030415     C* Valorizzo il barcode
045600050725     C                   MOVEL     *blanks       depCHISONO        7
045700050725     C                   EVAL      depCHISONO=%trim(%subst(vindta:331:26))
045800030226     C*
045900030226     C                   ENDSR
046000010201     C*----------------------------------------------------*
046100021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046200010201     C*----------------------------------------------------*
046300020305     C     WRIVAT        BEGSR
046400010201     C*
046500050725     C* Ciclo sul numero d colli reperito e creo tanti "CHI SONO" x ogni collo aggiungendo
046600050725     C* 1 progressivo d 2 byte al barcode indicato dal cliente x l'intera spedizione
046700050725     C                   z-add     1             wNCL              2 0
046800050725     C                   dow       wNCL <= VABNCL
046900050725     C                   move(p)   wNCL          wNCLalfa          2
047000050725     C                   eval      VATNOT = depCHISONO + wNCLalfa
047100021017     C                   write     FIVAT000
047200050725     C                   add       1             wNCL
047300050725     C                   enddo
047400010201     C*
047500010201     C                   ENDSR
047600010202     C*----------------------------------------------------*
047700021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047800010202     C*----------------------------------------------------*
047900020305     C     PREVAT        BEGSR
048000010202     C*
048100021017     C* Compongo il nome del membro da dare al FIVATWWR
048200010202     C                   eval      parmbr = vlrhdl
048300010202     C                   movel     'M'           parmbr
048400030430     C                   eval      parccm = '00060681'
048500010202     C                   eval      paropz = '1'
048600010202     C* Effettuo la chiamata al CLLE preposto
048700050128     C                   call(e)   'TITVVTC'
048800010202     C                   parm                    parccm
048900010202     C                   parm                    parmbr
049000010202     C                   parm                    paropz
049100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049200010202     C                   if        %error
049300010202     C                   movel     '1'           chkcall
049400010202     C                   else
049500010202     C                   movel     '0'           chkcall
049600010202     C                   endif
049700010202     C*
049800010202     C                   ENDSR
049900000801     C*----------------------------------------------------*
050000000801     C*  CONTROLLO NUMERICITA' CAMPI
050100000801     C*----------------------------------------------------*
050200000801     C     CHKNUM        BEGSR
050300000801     C*
050400000801     C                   call(e)   'ISNUMERIC'
050500000801     C                   PARM                    PiStr            30
050600030430     C                   PARM      '.'           PiDecChr          1
050700000801     C                   PARM      *ZEROS        PiVal            30 9
050800000801     C                   PARM      '0'           PiInt             1
050900000801     C                   PARM      '0'           PiNum             1
051000000801     C                   IF        %error
051100000801     C                   EVAL      PiInt=*off
051200030107     C                   EVAL      PiNum=*off
051300000801     C                   ENDIF
051400000801     C*
051500000801     C                   ENDSR
051600000801     C***
051700000801
051800011113
051900011113     C*----------------------------------------------------*
052000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052100011113     C*----------------------------------------------------*
052200011113     C     CHKIMPDIV     BEGSR
052300011113     C*
052400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052500011113     C                   Z-ADD     *zeros        wrkDec            9 9
052600011113     C*
052700011113     C* Come prima cosa effettuo considerazioni sulla divisa
052800011113     C                   IF        vabIAS > *zeros
052900011113     C                   IF        vabVAS <> 'EUR'
053000011113     C                   EVAL      vabVAS =  'ITL'
053100011113     C                   ENDIF
053200011113     C                   ENDIF
053300011113     C*
053400011113     C                   IF        vabCAS > *zeros
053500011113     C                   IF        vabVCA <> 'EUR'
053600011113     C                   EVAL      vabVCA =  'ITL'
053700011113     C                   ENDIF
053800011113     C                   ENDIF
053900011113     C*
054000011113     C                   IF        vabVMD > *zeros
054100020305     C                   IF        vabVAD <> 'EUR'
054200011113     C                   EVAL      vabVAD =  'ITL'
054300011113     C                   ENDIF
054400011113     C                   ENDIF
054500011113     C*
054600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054700011113     C                   Z-ADD     vabIAS        wrkDec
054800011113     C                   IF        wrkDec > *zeros
054900011113     C                   IF        vabVAS = 'ITL'
055000011113     C                   EVAL      vabIAS = *zeros
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055500011113     C                   Z-ADD     vabCAS        wrkDec
055600011113     C                   IF        wrkDec > *zeros
055700011113     C                   IF        vabVCA = 'ITL'
055800011113     C                   EVAL      vabCAS = *zeros
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056300011113     C                   Z-ADD     vabVMD        wrkDec
056400011113     C                   IF        wrkDec > *zeros
056500011113     C                   IF        vabVAD = 'ITL'
056600011113     C                   EVAL      vabVMD = *zeros
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C                   ENDSR
057100011113     C***
057200021017
057300000801
057400000801
057500990920      /TITLE Invio dei dati al punto operativo.
057600010202     C     invio         BEGSR
057700990920     C*
057800021017     C* 1° invio FIVAT
057900010201     C                   reset                   dscmz
058000021017     C                   move      vlrpoi        cmzdst
058100021017     C                   eval      cmzfld = 'FIVATWWR'
058200010201     C                   eval      cmzmbd = vlrhdl
058300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058400021009     C***                if        prmfir = *blanks
058500021017     C                   eval      cmzfla = 'FIVAT00F'
058600021017     C                   eval      cmzmba = 'FIVAT00F'
058700021009     C***                else
058800021009     C***                eval      cmzfla = prmfir
058900021009     C***                eval      cmzmba = prmfir
059000021009     C***                endif
059100010201     C                   eval      cmznrr = *zeros
059200020305     C                   move      §ctrokvt      cmznrr
059300021018     C                   eval      cmzlba = vlrfl1
059400010201     C                   call(e)   'TIS711C'
059500010201     C                   parm                    dscmz
059600010201     C                   parm      *blanks       esito
059700010205     C                   if        %error
059800010205     C                             or cmzerr = '1'
059900010205     C                             or esito  = '1'
060000010205     C                   eval      wrkesito = '3'
060100010205     C                   else
060200010201     C*
060300021017     C* 2° invio FIVAB
060400010201     C                   reset                   dscmz
060500021017     C                   move      vlrpoi        cmzdst
060600010201     C                   eval      cmzfld = vlrfou
060700010201     C                   eval      cmzmbd = vlrhdl
060800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060900021009     C***                if        prmfir = *blanks
061000021017     C                   eval      cmzfla = 'FIVAB00F'
061100021017     C                   eval      cmzmba = 'FIVAB00F'
061200021009     C***                else
061300021009     C***                eval      cmzfla = prmfir
061400021009     C***                eval      cmzmba = prmfir
061500021009     C***                endif
061600010201     C                   eval      cmznrr = *zeros
061700010201     C                   move      §ctrokvb      cmznrr
061800021018     C                   eval      cmzlba = vlrfl1
061900010201     C                   call(e)   'TIS711C'
062000010201     C                   parm                    dscmz
062100010201     C                   parm      *blanks       esito
062200010201     C                   if        %error
062300010201     C                             or cmzerr = '1'
062400010201     C                             or esito  = '1'
062500010201     C                   eval      wrkesito = '3'
062600010201     C                   endif
062700010205     C                   endif
062800990920     C*
062900000613     C                   ENDSR
063000000613     C***
063100990910
063200000613     C     *inzsr        BEGSR
063300990910     C*
063400990910     C     *entry        plist
063500990920     C                   parm                    tivlrds
063600990921     C                   parm      wrkesito      esito
063700000724     C                   parm                    prmlit
063800000710     C                   parm                    prmfir
063900000613     C*
064000000830     C* CALCOLA LA DATA CORRENTE
064100000830     C                   time                    wn14             14 0
064200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064300000830     C                   z-add     wn8           g08dat
064400000830     C                   z-add     *zeros        g08inv
064500000830     C                   movel     '0'           g08err
064600000830     C                   call      'XSRDA8'
064700000830     C                   parm                    wlbda8
064800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064900000830     C*
065000000613     C                   ENDSR
065100000613     C***
065200990908
065300030430     Otitv51p   E            riepilogo         2
065400990915     O                                              'Upload via Internet'
065500990915     O                                           +1 'Traduzione TIVIN00R -'
065600021017     O                                           +1 'FIVABWWR/FIVATWWR'
065700010201     O                                           +1 'Report testate bolle'
065800990915     O          E            riepilogo   2
065900990915     O                       wrkdata
066000990915     O                       wrkora              +1
066100990915     O                       procname            +1
066200990915     O          E            riepilogo   2
066300990915     O                                              'Cliente..................:'
066400990915     O                       VABCCM        z     +1
066500990915     O          E            riepilogo   2
066600990920     O                                              'Riferimento Strategi.....:'
066700990920     O                       vlrhdl              +1
066800990915     O          E            riepilogo   2
066900990915     O                                              'Giusti...................:'
067000010201     O                       §CTROKVB      2   +  1
067100990915     O          E            riepilogo   2
067200990915     O                                              'Sbagliati e corretti.....:'
067300971022     O                       §CTRMO        2   +  1
067400990915     O          E            riepilogo   2
067500990915     O                                              'Sbagliati e scartati.....:'
067600971022     O                       §CTRNO        2   +  1
067700000613
067800030430     Otitv51ps  E            testdett          2
067900000613     O                                              'Upload via Internet'
068000000613     O                                           +1 'Traduzione TIVIN00R -'
068100021017     O                                           +1 'FIVABWWR/FIVATWWR'
068200010201     O                                           +1 'Report testate bolle'
068300000616     O          E            testdett    3
068400000613     O                                           +2 'N° rec'
068500000613     O                                           +3 'Anteprima contenuto'
068600000616     O          E            rigadett    2
068700000613     O                       rrnum               +2
068800000621     O                       recko               +3
068900000616     O          E            findett     2
069000000613     O                       wrkdata
069100000613     O                       wrkora              +1
069200000613     O                       procname            +1
069300000616     O          E            findett     2
069400000613     O                                              'Cliente..................:'
069500000613     O                       VABCCM        z     +1
069600000616     O          E            findett     2
069700000613     O                                              'Riferimento Strategi.....:'
069800000613     O                       vlrhdl              +1
069900000616     O          E            findett     2
070000000613     O                                              'Giusti...................:'
070100010201     O                       §CTROKVB      2   +  1
070200000616     O          E            findett     2
070300000613     O                                              'Sbagliati e corretti.....:'
070400000613     O                       §CTRMO        2   +  1
070500000616     O          E            findett     2
070600000613     O                                              'Sbagliati e scartati.....:'
070700000613     O                       §CTRNO        2   +  1
070800000512** CMD - COMANDI CL
070900030430OVRPRTF FILE(TITV51P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
071000030430OVRPRTF FILE(TITV51PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071100030430DLTOVR FILE(TITV51P TITV51PS) LVL(*)
071200000512
071300000512
