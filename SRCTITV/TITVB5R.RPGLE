000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800040428     Ftitvb5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040428     Ftitvb5ps  O    f  198        PRINTER usropn
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
003500040428     D depspe          s              8    INZ(*blanks)
003600040428     D curspe          s              8    INZ(*blanks)
003700010202     D parccm          s              8    INZ(*blanks)
003800010202     D parmbr          s             10    INZ(*blanks)
003900010202     D paropz          s              1    INZ(*blanks)
004000010202     D chkcall         s              1    INZ(*blanks)
004100010213
004200000801     D  Num5_0         s              5  0
004300030107     D  Num7_0         s              7  0
004400000830
004500000830     D*------------------
004600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700000830     D*------------------
004800000830     D WLBDA8          DS                  INZ
004900000830     D  G08DAT                 1      8  0
005000000830     D  G08INV                 9     16  0
005100000830     D  G08ERR                17     17
005200000830     D  G08TGI                18     22  0
005300000830     D*
005400990908
005500010201
005600010201
005700990915     C                   time                    wrkdata
005800990915     C                   time                    wrkora
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200000724     C*
006300000724     C* SE OCCORRE SPEDIRE IN FILIALE
006400021017     C                   if        vlrpoi <> *zeros
006500000724     C*
006600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006800021017     C     vlrpoi        chain     azorg01l
006900000724     C                   if        %found
007000000616     C                   movel(p)  CMD(1)        depcmd
007100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007200000616     C*
007300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007400011113     C                   Z-ADD     150           LENGH            15 5
007500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007600000616     C                   PARM                    depcmd
007700000616     C                   PARM                    LENGH
007800000724     C*
007900000724     C                   endif
008000000724     C                   endif
008100000616     C*
008200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000616     C                   movel(p)  CMD(2)        depcmd
008400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008700011113     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000616     C*
009200040428     C                   if        not %open(titvb5ps)
009300040428     C                   open      titvb5ps
009400000616     C                   except    testdett
009500000613     C                   endif
009600000613     C*
009700040428     C                   EXSR      RWBSH                                        LETT/SCR. VAB/VAT
009800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009900000613     C*
010000010202     C* Effettuo la chiamata al CLLE preposto
010100030711     C                   call(e)   'TITVVTC'
010200010202     C                   parm                    parccm
010300010202     C                   parm                    parmbr
010400010202     C                   parm      '2'           paropz
010500010202     C*
010600040428     C                   if        %open(titvb5ps)
010700000616     C                   except    findett
010800040428     C                   close     titvb5ps
010900000613     C                   endif
011000000616     C*
011100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011200011113     C                   Z-ADD     150           LENGH            15 5
011300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011400000616     C                   PARM                    CMD(3)
011500000616     C                   PARM                    LENGH
011600000616     C*
011700000801     C
011800010201     C                   seton                                        LR
011900000613
012000000613
012100000613     C*--------------------------------------------------------
012200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012300000613     C*--------------------------------------------------------
012400000621     C     STPR          BEGSR
012500000613     C*
012600040428     C                   if        not %open(titvb5p)
012700040428     C                   open      titvb5p
012800990915     C                   endif
012900990915     C*
013000990915     C                   except    riepilogo
013100990915     C*
013200040428     C                   if        %open(titvb5p)
013300040428     C                   close     titvb5p
013400990914     C                   endif
013500990910     C*
013600000613     C                   ENDSR
013700000613     C***
013800990908
013900000801
014000910830     C*--------------------------------------------------------
014100040428     C* RWBSH     LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
014200910830     C*--------------------------------------------------------
014300040428     C     RWBSH         BEGSR
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
015400040428     C                   if        chkcall = '0'
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
017800010305     C*
017900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018000040428     C                   EVAL      PiStr=%trim(%subst(vindta:298:8))
018100020305     C                   MOVEL(p)  PiStr         curspe
018200010305     C*
018300010305     C                   if        depspe = *blanks                             => 1° giro
018400010305     C                   eval      depspe = curspe                              => memorizz. spediz
018500040428     C                   exsr      impvab                                       => carico VAB
018600040428     C                   exsr      wrivat                                       => carico/scrivo VAT
018700010305     C                   else
018800010305     C                   if        curspe <> depspe                             => rottura di spediz
018900010305     C                   eval      depspe = curspe                              => memorizz. spediz
019000040428     C                   exsr      wrivab                                       => scrivo VAB
019100040428     C                   exsr      impvab                                       => carico VAB
019200040428     C                   exsr      wrivat                                       => carico/scrivo VAT
019300020305     C                   else                                                   => x stessa spediz
019400040428     C                   exsr      impvab                                       => carico VAB
019500040428     C                   exsr      wrivat                                       => carico/scrivo VAT
019600010305     C                   endif
019700010305     C                   endif
019800010305     C                   endif
019900000905     C*
020000000905     C                   else
020100000905     C                   eval      vinflg = '1'
020200000905     C                   endif
020300000905     C*
020400000905     C  N70              update    tivin000
020500000905     C*
020600991022     C  N70              ENDdo
020700010202     C*
020800020305     C* Scarico i VAB rimasti "in sospeso"
020900020305     C                   exsr      wrivab
021000010202     C*
021100010202     C                   endif
021200990910
021300990910     C* Se non ci sono record con errori ...
021400000710     C                   if        §ctrno = 0
021500990910     C* ... restituisco esito OK.
021600990921     C                   eval      wrkesito = '0'
021700990910     C                   else
021800010201     C                   if        §ctrokvb > 0
021900990921     C                   eval      wrkesito = '1'
022000000710     C                   else
022100000710     C                   eval      wrkesito = '2'
022200990910     C                   endif
022300000710     C                   endif
022400990910     C*
022500990914     C                   if        %open(tivin00r)
022600990908     C                   close     tivin00r
022700990914     C                   endif
022800021017     C                   if        %open(fivabwwr)
022900021017     C                   close     fivabwwr
023000990914     C                   endif
023100021017     C                   if        %open(fivatwwr)
023200021017     C                   close     fivatwwr
023300010201     C                   endif
023400021017     C*
023500021017     C                   if        vlrpoi <> 999
023600021017     C                   eval      vlrpoi = vlrpoi
023700021017     C                   endif
023800990910     C*
023900010201     C                   if        §ctrokvb > 0
024000021017     C                             and vlrpoi > *zeros
024100010202     C                   exsr      invio
024200990920     C                   endif
024300990920     C*
024400910830     C                   ENDSR
024500000613     C***
024600010305
024700010305     C*----------------------------------------------------*
024800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024900010305     C*----------------------------------------------------*
025000020305     C     WRIVAB        BEGSR
025100010305     C*
025200021017     C                   write     fivab000                                     => scarico il VAB
025300010305     C*
025400010305     C                   ENDSR
025500990920
025600000801     C*----------------------------------------------------*
025700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025800000801     C*----------------------------------------------------*
025900010201     C     INZVAR        BEGSR
026000000801     C*
026100010201     C                   Z-ADD     *zeros        Num5_0
026200030107     C                   Z-ADD     *zeros        Num7_0
026300030711     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026400030711     C                   MOVEL     '0'           FlgCAS            1
026500000801     C*
026600000801     C                   ENDSR
026700000801     C*----------------------------------------------------*
026800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026900000801     C*----------------------------------------------------*
027000000801     C     DEFCAM        BEGSR
027100040428     C*
027200040428     C                   CLEAR                   FIVAB000
027300000801     C*
027400020619     C* Imposto i valori di default...
027500040428     C                   Z-ADD     0052323       VABCCM
027600040428     C                   Z-ADD     0052323       VATCCM
027700021017     C                   Z-ADD     vlrpoi        VABFGS
027800021017     C                   Z-ADD     vlrpoi        VATFGS
027900040428     C                   Z-ADD     005           VABLNP
028000040428     C                   Z-ADD     005           VATLNP
028100040428     C                   Z-ADD     001           VABCTR
028200021017     C                   MOVEL     '7Q'          VABCTM
028300020305     C                   MOVEL     'E'           VATTRC
028400020619     C* ... e poi verifico se sono stati passati come parametri
028500020619     C                   IF        vlrppt > *blanks
028600040428     C                   IF        %subst(vlrppt:1:7) <> *blanks
028700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028800020619     C                   EXSR      CHKNUM
028900020619     C                   IF        PiInt=*on
029000020619     C                   Z-ADD     PiVal         VABCCM
029100020619     C                   Z-ADD     PiVal         VATCCM
029200020619     C                   ENDIF
029300040428     C                   ENDIF
029400040428     C                   IF        %subst(vlrppt:8:3) <> *blanks
029500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029600020619     C                   EXSR      CHKNUM
029700020619     C                   IF        PiInt=*on
029800020619     C                   Z-ADD     PiVal         VABLNP
029900020619     C                   Z-ADD     PiVal         VATLNP
030000020619     C                   ENDIF
030100040428     C                   ENDIF
030200040428     C                   IF        %subst(vlrppt:11:3) <> *blanks
030300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030400020619     C                   EXSR      CHKNUM
030500020619     C                   IF        PiInt=*on
030600020619     C                   Z-ADD     PiVal         VABCTR
030700020619     C                   ENDIF
030800040428     C                   ENDIF
030900020619     C                   ENDIF
031000000801     C*
031100000801     C                   ENDSR
031200000801     C*----------------------------------------------------*
031300021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031400000801     C*----------------------------------------------------*
031500010201     C     IMPVAB        BEGSR
031600000801     C*
031700020305     C                   EXSR      INZVAR
031800020305     C                   EXSR      DEFCAM
031900010305     C*
032000000801     C                   Z-ADD     *zeros        errore            1 0
032100000830     C                   MOVEL     datcor        VABAAS
032200020305     C                   MOVEL     datcor        VATAAS
032300000830     C                   MOVE      datcor        VABMGS
032400031117     C*
032500040428     C                   EVAL      VABRSD=%trim(%subst(vindta:145:35))
032600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032800020117     C* ==
032900040428     C                   EVAL      VABRD2=%trim(%subst(vindta:180:15))
033000040428     C                   EVAL      VABIND=%trim(%subst(vindta:195:25))
033100040428     C                   EVAL      VABLOD=%trim(%subst(vindta:229:25))
033200040428     C                   EVAL      VABPRD=%trim(%subst(vindta:503:2))
033300040428     C                   EVAL      VABRMA=%trim(%subst(vindta:298:8))
033400030711     C* CAD
033500040428     C                   EVAL      PiStr=%trim(%subst(vindta:223:5))
033600010201     C                   EXSR      CHKNUM
033700010201     C                   IF        PiInt=*on
033800010201     C                   Z-ADD     PiVal         Num5_0
033900021017     C                   MOVEL     Num5_0        VABCAD
034000010201     C                   ELSE
034100010201     C                   ADD       1             errore
034200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034300021017     C                             + ' ' + 'VABCAD'
034400010201     C                   ENDIF
034500030711     C* RMN
034600040428     C                   EVAL      PiStr=%trim(%subst(vindta:298:8))
034700010201     C                   EXSR      CHKNUM
034800010201     C                   IF        PiInt=*on
034900021017     C                   Z-ADD     PiVal         VABRMN
035000040428     C                   Z-ADD     PiVal         Num7_0
035100040428     C                   MOVE(P)   Num7_0        VABNSP
035200040428     C                   MOVE(P)   Num7_0        VATNSP
035300010201     C                   ELSE
035400010201     C                   ADD       1             errore
035500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035600040428     C                             + ' ' + 'VABRMN VABNSP VATNSP'
035700010201     C                   ENDIF
035800040428     C* NCL 1
035900040428     C                   EVAL      PiStr=%trim(%subst(vindta:338:4))
036000030107     C                   EXSR      CHKNUM
036100030107     C                   IF        PiInt=*on
036200040428     C                   ADD       PiVal         VABNCL
036300030107     C                   ELSE
036400030107     C                   ADD       1             errore
036500030107     C                   EVAL      vinmsg = %trimr(vinmsg)
036600030107     C                             + ' ' + 'VABNCL'
036700030107     C                   ENDIF
036800040428     C* NCL 2
036900040428     C                   EVAL      PiStr=%trim(%subst(vindta:369:4))
037000040428     C                   EXSR      CHKNUM
037100040428     C                   IF        PiInt=*on
037200040428     C                   ADD       PiVal         VABNCL
037300040428     C                   ELSE
037400040428     C                   ADD       1             errore
037500040428     C                   EVAL      vinmsg = %trimr(vinmsg)
037600040428     C                             + ' ' + 'VABNCL'
037700040428     C                   ENDIF
037800040428     C* NCL 3
037900040428     C                   EVAL      PiStr=%trim(%subst(vindta:400:4))
038000040428     C                   EXSR      CHKNUM
038100040428     C                   IF        PiInt=*on
038200040428     C                   ADD       PiVal         VABNCL
038300040428     C                   ELSE
038400040428     C                   ADD       1             errore
038500040428     C                   EVAL      vinmsg = %trimr(vinmsg)
038600040428     C                             + ' ' + 'VABNCL'
038700040428     C                   ENDIF
038800040428     C* NCL 4
038900040428     C                   EVAL      PiStr=%trim(%subst(vindta:431:4))
039000040428     C                   EXSR      CHKNUM
039100040428     C                   IF        PiInt=*on
039200040428     C                   ADD       PiVal         VABNCL
039300040428     C                   ELSE
039400040428     C                   ADD       1             errore
039500040428     C                   EVAL      vinmsg = %trimr(vinmsg)
039600040428     C                             + ' ' + 'VABNCL'
039700040428     C                   ENDIF
039800040428     C* PKB 1
039900040428     C                   EVAL      PiStr=%trim(%subst(vindta:362:5))
040000030107     C                   EXSR      CHKNUM
040100030107     C                   IF        PiNum=*on
040200040428     C                   EVAL      VABPKB = VABPKB + PiVal/10                   * gestisco 1 decim.
040300030107     C                   ELSE
040400030107     C                   ADD       1             errore
040500030107     C                   EVAL      vinmsg = %trimr(vinmsg)
040600030107     C                             + ' ' + 'VABPKB'
040700030107     C                   ENDIF
040800040428     C* PKB 2
040900040428     C                   EVAL      PiStr=%trim(%subst(vindta:393:5))
041000040428     C                   EXSR      CHKNUM
041100040428     C                   IF        PiNum=*on
041200040428     C                   EVAL      VABPKB = VABPKB + PiVal/10                   * gestisco 1 decim.
041300040428     C                   ELSE
041400040428     C                   ADD       1             errore
041500040428     C                   EVAL      vinmsg = %trimr(vinmsg)
041600040428     C                             + ' ' + 'VABPKB'
041700040428     C                   ENDIF
041800040428     C* PKB 3
041900040428     C                   EVAL      PiStr=%trim(%subst(vindta:424:5))
042000040428     C                   EXSR      CHKNUM
042100040428     C                   IF        PiNum=*on
042200040428     C                   EVAL      VABPKB = VABPKB + PiVal/10                   * gestisco 1 decim.
042300040428     C                   ELSE
042400040428     C                   ADD       1             errore
042500040428     C                   EVAL      vinmsg = %trimr(vinmsg)
042600040428     C                             + ' ' + 'VABPKB'
042700040428     C                   ENDIF
042800040428     C* PKB 4
042900040428     C                   EVAL      PiStr=%trim(%subst(vindta:455:5))
043000040428     C                   EXSR      CHKNUM
043100040428     C                   IF        PiNum=*on
043200040428     C                   EVAL      VABPKB = VABPKB + PiVal/10                   * gestisco 1 decim.
043300040428     C                   ELSE
043400040428     C                   ADD       1             errore
043500040428     C                   EVAL      vinmsg = %trimr(vinmsg)
043600040428     C                             + ' ' + 'VABPKB'
043700040428     C                   ENDIF
043800010205     C*
043900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
044000040428     C                   IF        FlgCAS = '0'
044100031117     C                   EVAL      VABCBO = '1'                                 * franco  NO contr.
044200040428     C                   ELSE
044300040428     C                   EVAL      VABCBO = '4'                                 * franco  SI contr.
044400040428     C                   EVAL      VABVCA = 'EUR'
044500031117     C                   ENDIF
044600020305     C*
044700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044800011113     C                   EXSR      CHKIMPDIV
044900010202     C*
045000000801     C* Ebbene...
045100000801     C                   ADD       1             §CTRMO
045200010201     C                   IF        errore <> *zeros
045300000801     C                   ADD       1             §CTRNO
045400000801     C                   EVAL      recko = vindta
045500000801     C                   EXCEPT    rigadett
045600000801     C                   EVAL      vinflg = '2'
045700000801     C                   ELSE
045800010201     C                   ADD       1             §CTROKVB
045900000801     C                   ENDIF
046000000801     C*
046100000801     C                   ENDSR
046200010201     C*----------------------------------------------------*
046300021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046400010201     C*----------------------------------------------------*
046500020305     C     WRIVAT        BEGSR
046600040428     C*
046700040428     C* X ogni collo indicato in testata
046800040428     C                   z-add     1             wColli            5 0
046900040428     C                   dow       wColli <= VABNCL
047000010201     C*
047100031117     C* Valorizzo il buffer di scrittura del FIVAT
047200050112     C                   move(p)   wColli        wColliAlfa        2
047300050112     C                   eval      VATNOT = '00'+%subst(vindta:298:8)+
047400040528     C                                      wColliAlfa
047500021017     C                   write     FIVAT000
047600040428     C*
047700040428     C                   add       1             wColli
047800040428     C                   enddo
047900010201     C*
048000010201     C                   ENDSR
048100010202     C*----------------------------------------------------*
048200021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048300010202     C*----------------------------------------------------*
048400020305     C     PREVAT        BEGSR
048500010202     C*
048600021017     C* Compongo il nome del membro da dare al FIVATWWR
048700010202     C                   eval      parmbr = vlrhdl
048800010202     C                   movel     'M'           parmbr
048900040428     C                   eval      parccm = '00052323'
049000010202     C                   eval      paropz = '1'
049100010202     C* Effettuo la chiamata al CLLE preposto
049200030711     C                   call(e)   'TITVVTC'
049300010202     C                   parm                    parccm
049400010202     C                   parm                    parmbr
049500010202     C                   parm                    paropz
049600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049700010202     C                   if        %error
049800010202     C                   movel     '1'           chkcall
049900010202     C                   else
050000010202     C                   movel     '0'           chkcall
050100010202     C                   endif
050200010202     C*
050300010202     C                   ENDSR
050400000801     C*----------------------------------------------------*
050500000801     C*  CONTROLLO NUMERICITA' CAMPI
050600000801     C*----------------------------------------------------*
050700000801     C     CHKNUM        BEGSR
050800000801     C*
050900000801     C                   call(e)   'ISNUMERIC'
051000000801     C                   PARM                    PiStr            30
051100030711     C                   PARM      '.'           PiDecChr          1
051200000801     C                   PARM      *ZEROS        PiVal            30 9
051300000801     C                   PARM      '0'           PiInt             1
051400000801     C                   PARM      '0'           PiNum             1
051500000801     C                   IF        %error
051600000801     C                   EVAL      PiInt=*off
051700030107     C                   EVAL      PiNum=*off
051800000801     C                   ENDIF
051900000801     C*
052000000801     C                   ENDSR
052100000801     C***
052200000801
052300011113
052400011113     C*----------------------------------------------------*
052500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052600011113     C*----------------------------------------------------*
052700011113     C     CHKIMPDIV     BEGSR
052800011113     C*
052900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053000011113     C                   Z-ADD     *zeros        wrkDec            9 9
053100011113     C*
053200011113     C* Come prima cosa effettuo considerazioni sulla divisa
053300011113     C                   IF        vabIAS > *zeros
053400011113     C                   IF        vabVAS <> 'EUR'
053500011113     C                   EVAL      vabVAS =  'ITL'
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C                   IF        vabCAS > *zeros
054000011113     C                   IF        vabVCA <> 'EUR'
054100011113     C                   EVAL      vabVCA =  'ITL'
054200011113     C                   ENDIF
054300011113     C                   ENDIF
054400011113     C*
054500011113     C                   IF        vabVMD > *zeros
054600020305     C                   IF        vabVAD <> 'EUR'
054700011113     C                   EVAL      vabVAD =  'ITL'
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055200011113     C                   Z-ADD     vabIAS        wrkDec
055300011113     C                   IF        wrkDec > *zeros
055400011113     C                   IF        vabVAS = 'ITL'
055500011113     C                   EVAL      vabIAS = *zeros
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056000011113     C                   Z-ADD     vabCAS        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVCA = 'ITL'
056300011113     C                   EVAL      vabCAS = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056800011113     C                   Z-ADD     vabVMD        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVAD = 'ITL'
057100011113     C                   EVAL      vabVMD = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C                   ENDSR
057600011113     C***
057700021017
057800000801
057900000801
058000990920      /TITLE Invio dei dati al punto operativo.
058100010202     C     invio         BEGSR
058200990920     C*
058300021017     C* 1° invio FIVAT
058400010201     C                   reset                   dscmz
058500021017     C                   move      vlrpoi        cmzdst
058600021017     C                   eval      cmzfld = 'FIVATWWR'
058700010201     C                   eval      cmzmbd = vlrhdl
058800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058900021009     C***                if        prmfir = *blanks
059000021017     C                   eval      cmzfla = 'FIVAT00F'
059100021017     C                   eval      cmzmba = 'FIVAT00F'
059200021009     C***                else
059300021009     C***                eval      cmzfla = prmfir
059400021009     C***                eval      cmzmba = prmfir
059500021009     C***                endif
059600010201     C                   eval      cmznrr = *zeros
059700020305     C                   move      §ctrokvt      cmznrr
059800021018     C                   eval      cmzlba = vlrfl1
059900010201     C                   call(e)   'TIS711C'
060000010201     C                   parm                    dscmz
060100010201     C                   parm      *blanks       esito
060200010205     C                   if        %error
060300010205     C                             or cmzerr = '1'
060400010205     C                             or esito  = '1'
060500010205     C                   eval      wrkesito = '3'
060600010205     C                   else
060700010201     C*
060800021017     C* 2° invio FIVAB
060900010201     C                   reset                   dscmz
061000021017     C                   move      vlrpoi        cmzdst
061100010201     C                   eval      cmzfld = vlrfou
061200010201     C                   eval      cmzmbd = vlrhdl
061300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061400021009     C***                if        prmfir = *blanks
061500021017     C                   eval      cmzfla = 'FIVAB00F'
061600021017     C                   eval      cmzmba = 'FIVAB00F'
061700021009     C***                else
061800021009     C***                eval      cmzfla = prmfir
061900021009     C***                eval      cmzmba = prmfir
062000021009     C***                endif
062100010201     C                   eval      cmznrr = *zeros
062200010201     C                   move      §ctrokvb      cmznrr
062300021018     C                   eval      cmzlba = vlrfl1
062400010201     C                   call(e)   'TIS711C'
062500010201     C                   parm                    dscmz
062600010201     C                   parm      *blanks       esito
062700010201     C                   if        %error
062800010201     C                             or cmzerr = '1'
062900010201     C                             or esito  = '1'
063000010201     C                   eval      wrkesito = '3'
063100010201     C                   endif
063200010205     C                   endif
063300990920     C*
063400000613     C                   ENDSR
063500000613     C***
063600070411
063700070411     C     *pssr         BEGSR
063800070411     C*
063900070411     C                   if        %open(tivin00r)
064000070411     C                   close     tivin00r
064100070411     C                   endif
064200070411     C                   if        %open(fivabwwr)
064300070411     C                   close     fivabwwr
064400070411     C                   endif
064500070411     C                   if        %open(fivatwwr)
064600070411     C                   close     fivatwwr
064700070411     C                   endif
064800070411     C*
064900070411     C* Effettuo la chiamata al CLLE preposto
065000070411     C                   call(e)   'TITVVTC'
065100070411     C                   parm                    parccm
065200070411     C                   parm                    parmbr
065300070411     C                   parm      '2'           paropz
065400070411     C*
065500070411     C                   eval      wrkesito = '2'
065600070411     C*
065700070411     C                   seton                                        LR
065800070411     C*
065900070411     C                   ENDSR     '*CANCL'
066000070411     C***
066100990910
066200000613     C     *inzsr        BEGSR
066300990910     C*
066400990910     C     *entry        plist
066500990920     C                   parm                    tivlrds
066600990921     C                   parm      wrkesito      esito
066700000724     C                   parm                    prmlit
066800000710     C                   parm                    prmfir
066900000613     C*
067000000830     C* CALCOLA LA DATA CORRENTE
067100000830     C                   time                    wn14             14 0
067200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067300000830     C                   z-add     wn8           g08dat
067400000830     C                   z-add     *zeros        g08inv
067500000830     C                   movel     '0'           g08err
067600000830     C                   call      'XSRDA8'
067700000830     C                   parm                    wlbda8
067800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067900000830     C*
068000000613     C                   ENDSR
068100000613     C***
068200990908
068300040428     Otitvb5p   E            riepilogo         2
068400990915     O                                              'Upload via Internet'
068500990915     O                                           +1 'Traduzione TIVIN00R -'
068600021017     O                                           +1 'FIVABWWR/FIVATWWR'
068700010201     O                                           +1 'Report testate bolle'
068800990915     O          E            riepilogo   2
068900990915     O                       wrkdata
069000990915     O                       wrkora              +1
069100990915     O                       procname            +1
069200990915     O          E            riepilogo   2
069300990915     O                                              'Cliente..................:'
069400990915     O                       VABCCM        z     +1
069500990915     O          E            riepilogo   2
069600990920     O                                              'Riferimento Strategi.....:'
069700990920     O                       vlrhdl              +1
069800990915     O          E            riepilogo   2
069900990915     O                                              'Giusti...................:'
070000010201     O                       §CTROKVB      2   +  1
070100990915     O          E            riepilogo   2
070200990915     O                                              'Sbagliati e corretti.....:'
070300971022     O                       §CTRMO        2   +  1
070400990915     O          E            riepilogo   2
070500990915     O                                              'Sbagliati e scartati.....:'
070600971022     O                       §CTRNO        2   +  1
070700000613
070800040428     Otitvb5ps  E            testdett          2
070900000613     O                                              'Upload via Internet'
071000000613     O                                           +1 'Traduzione TIVIN00R -'
071100021017     O                                           +1 'FIVABWWR/FIVATWWR'
071200010201     O                                           +1 'Report testate bolle'
071300000616     O          E            testdett    3
071400000613     O                                           +2 'N° rec'
071500000613     O                                           +3 'Anteprima contenuto'
071600000616     O          E            rigadett    2
071700000613     O                       rrnum               +2
071800000621     O                       recko               +3
071900000616     O          E            findett     2
072000000613     O                       wrkdata
072100000613     O                       wrkora              +1
072200000613     O                       procname            +1
072300000616     O          E            findett     2
072400000613     O                                              'Cliente..................:'
072500000613     O                       VABCCM        z     +1
072600000616     O          E            findett     2
072700000613     O                                              'Riferimento Strategi.....:'
072800000613     O                       vlrhdl              +1
072900000616     O          E            findett     2
073000000613     O                                              'Giusti...................:'
073100010201     O                       §CTROKVB      2   +  1
073200000616     O          E            findett     2
073300000613     O                                              'Sbagliati e corretti.....:'
073400000613     O                       §CTRMO        2   +  1
073500000616     O          E            findett     2
073600000613     O                                              'Sbagliati e scartati.....:'
073700000613     O                       §CTRNO        2   +  1
073800000512** CMD - COMANDI CL
073900040428OVRPRTF FILE(TITVB5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
074000040428OVRPRTF FILE(TITVB5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074100040428DLTOVR FILE(TITVB5P TITVB5PS) LVL(*)
074200000512
074300000512
