000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030423     Ftitv40p   O    f  198        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030310     Ftitv40ps  O    f  198        PRINTER usropn
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
003400030423     D dettRec         s            170    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600030310     D depspe          s              9    INZ(*blanks)
003700030310     D curspe          s              9    INZ(*blanks)
003800030411     D cntspe          s              7  0 INZ(*zeros)
003900010202     D parccm          s              8    INZ(*blanks)
004000010202     D parmbr          s             10    INZ(*blanks)
004100010202     D paropz          s              1    INZ(*blanks)
004200010202     D chkcall         s              1    INZ(*blanks)
004300010213
004400000801     D  Num5_0         s              5  0
004500030107     D  Num7_0         s              7  0
004600000830
004700000830     D*------------------
004800000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004900000830     D*------------------
005000000830     D WLBDA8          DS                  INZ
005100000830     D  G08DAT                 1      8  0
005200000830     D  G08INV                 9     16  0
005300000830     D  G08ERR                17     17
005400000830     D  G08TGI                18     22  0
005500000830     D*
005600990908
005700010201
005800010201
005900990915     C                   time                    wrkdata
006000990915     C                   time                    wrkora
006100000913     C                   reset                   rrnum
006200990921     C                   reset                   esito
006300990921     C                   reset                   wrkesito
006400000724     C*
006500000724     C* SE OCCORRE SPEDIRE IN FILIALE
006600021017     C                   if        vlrpoi <> *zeros
006700000724     C*
006800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007000021017     C     vlrpoi        chain     azorg01l
007100000724     C                   if        %found
007200030424     C                   movel(p)  CMD(1)        depcmd
007300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007400030424     C                   EVAL      depcmd=%trim(depcmd)+' PAGESIZE(*N 198)'
007500000616     C*
007600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007700011113     C                   Z-ADD     150           LENGH            15 5
007800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007900000616     C                   PARM                    depcmd
008000000616     C                   PARM                    LENGH
008100000724     C*
008200000724     C                   endif
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000616     C*
009500030310     C                   if        not %open(titv40ps)
009600030310     C                   open      titv40ps
009700000616     C                   except    testdett
009800000613     C                   endif
009900000613     C*
010000030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010200000613     C*
010300010202     C* Effettuo la chiamata al CLLE preposto
010400050128     C                   call(e)   'TITVVTC'
010500010202     C                   parm                    parccm
010600010202     C                   parm                    parmbr
010700010202     C                   parm      '2'           paropz
010800010202     C*
010900030310     C                   if        %open(titv40ps)
011000000616     C                   except    findett
011100030310     C                   close     titv40ps
011200000613     C                   endif
011300000616     C*
011400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011500011113     C                   Z-ADD     150           LENGH            15 5
011600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011700000616     C                   PARM                    CMD(3)
011800000616     C                   PARM                    LENGH
011900000616     C*
012000010201     C                   seton                                        LR
012100000613
012200000613
012300000613     C*--------------------------------------------------------
012400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012500000613     C*--------------------------------------------------------
012600000621     C     STPR          BEGSR
012700000613     C*
012800030310     C                   if        not %open(titv40p)
012900030310     C                   open      titv40p
013000990915     C                   endif
013100990915     C*
013200990915     C                   except    riepilogo
013300990915     C*
013400030310     C                   if        %open(titv40p)
013500030310     C                   close     titv40p
013600990914     C                   endif
013700990910     C*
013800000613     C                   ENDSR
013900000613     C***
014000990908
014100000801
014200910830     C*--------------------------------------------------------
014300030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
014400910830     C*--------------------------------------------------------
014500030226     C     RWFILE        BEGSR
014600990910     C*
014700990914     C                   if        not %open(tivin00r)
014800990908     C                   open      tivin00r
014900990914     C                   endif
015000021017     C                   if        not %open(fivabwwr)
015100021017     C                   open      fivabwwr
015200990914     C                   endif
015300021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015400020305     C                   exsr      prevat
015500010201     C*
015600010202     C                   if        chkcall = '0'
015700010202     C*
015800021017     C                   if        not %open(fivatwwr)
015900021017     C                   open      fivatwwr
016000010201     C                   endif
016100990910     C*
016200010201     C                   clear                   §CTROKVB          5 0
016300020305     C                   clear                   §CTROKVT          5 0
016400000801     C                   clear                   §CTRMO            5 0
016500000801     C                   clear                   §CTRNO            5 0
016600030423     C                   clear                   §CTRPKB          11 1
016700990910     C*
016800030423     C* Stampo l'intestazione dei record di dettaglio che verranno stampati
016900030423     C                   if        not %open(titv40p)
017000030423     C                   open      titv40p
017100030423     C                   endif
017200030423     C                   except    dettT
017300030423     C*
017400921023     C                   DO        *HIVAL
017500990913     C*
017600990915     C                   READ      tivin00r                               70
017700000905     C                   if        vindta > *blanks
017800000613     C                   add       1             rrnum
017900000801     C*
018000000801     C                   if        *in70 = *off
018100000801     C                             and
018200000801     C                             (vinflg = *blanks
018300000801     C                              or vinflg = '0'
018400000801     C                              or vinflg = '2')
018500000801     C*
018600030423     C* Stampo il contenuto del record da tradurre corrente (stampa x filiale)
018700030423     C                   eval      dettRec = %subst(vindta:1:170)
018800030423     C                   except    dettR1
018900030423     C                   eval      dettRec = %subst(vindta:171:170)
019000030423     C                   except    dettR2
019100030423     C                   eval      dettRec = %subst(vindta:341:170)
019200030423     C                   except    dettR3
019300030423     C*
019400000801     C                   clear                   vinmsg
019500030310     C                   eval      vinflg = '1'
019600030310     C*
019700030310     C* Determino il numero di Spedizione e a rottura eseguo operazioni
019800030411     C                   eval      PiStr=%trim(%subst(vindta:25:9))
019900030411     C                   movel(p)  PiStr         curspe
020000030411     C                   add       1             cntspe
020100030411     C*
020200030411     C                   if        cntspe = 1
020300030411     C* A rottura inizializzo campi di totalizzazione
020400030411     C                   clear                   fivab000
020500030411     C*
020600030411     C                   exsr      impvab                                       => carico VAB
020700030411     C                   exsr      impvat                                       => carico VAT
020800030411     C                   eval      depspe = curspe
020900030411     C                   else
021000030411     C                   if        curspe <> depspe
021100030411     C                   exsr      wrivab                                       => scarico VAB
021200030411     C                   exsr      wrivat                                       => scarico VAT
021300030411     C* A rottura inizializzo campi di totalizzazione
021400030411     C                   clear                   fivab000
021500030411     C*
021600030411     C                   exsr      impvab                                       => carico VAB
021700030411     C                   exsr      impvat                                       => carico VAT
021800030411     C                   eval      depspe = curspe
021900030411     C                   else
022000030411     C                   exsr      wrivat                                       => scarico VAT
022100030411     C                   exsr      impvab                                       => carico VAB
022200030411     C                   exsr      impvat                                       => carico VAT
022300030411     C                   endif
022400030411     C                   endif
022500000905     C*
022600000905     C                   else
022700000905     C                   eval      vinflg = '1'
022800000905     C                   endif
022900030411     C                   endif
023000000905     C*
023100000905     C  N70              update    tivin000
023200000905     C*
023300991022     C  N70              ENDdo
023400030424     C*
023500030424     C* Al termine della lettura scarico i VAB/VAT ancora in "canna"
023600030424     C                   exsr      wrivab                                       => scarico VAB
023700030424     C                   exsr      wrivat                                       => scarico VAT
023800010202     C*
023900010202     C                   endif
024000990910
024100990910     C* Se non ci sono record con errori ...
024200000710     C                   if        §ctrno = 0
024300990910     C* ... restituisco esito OK.
024400990921     C                   eval      wrkesito = '0'
024500990910     C                   else
024600010201     C                   if        §ctrokvb > 0
024700990921     C                   eval      wrkesito = '1'
024800000710     C                   else
024900000710     C                   eval      wrkesito = '2'
025000990910     C                   endif
025100000710     C                   endif
025200990910     C*
025300990914     C                   if        %open(tivin00r)
025400990908     C                   close     tivin00r
025500990914     C                   endif
025600021017     C                   if        %open(fivabwwr)
025700021017     C                   close     fivabwwr
025800990914     C                   endif
025900021017     C                   if        %open(fivatwwr)
026000021017     C                   close     fivatwwr
026100010201     C                   endif
026200021017     C*
026300021017     C                   if        vlrpoi <> 999
026400021017     C                   eval      vlrpoi = vlrpoi
026500021017     C                   endif
026600990910     C*
026700010201     C                   if        §ctrokvb > 0
026800021017     C                             and vlrpoi > *zeros
026900010202     C                   exsr      invio
027000990920     C                   endif
027100990920     C*
027200910830     C                   ENDSR
027300000613     C***
027400990920
027500000801     C*----------------------------------------------------*
027600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027700000801     C*----------------------------------------------------*
027800010201     C     INZVAR        BEGSR
027900000801     C*
028000010201     C                   Z-ADD     *zeros        Num5_0
028100030107     C                   Z-ADD     *zeros        Num7_0
028200030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028300030310     C                   MOVEL     '0'           FlgCAS            1
028400000801     C*
028500000801     C                   ENDSR
028600000801     C*----------------------------------------------------*
028700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028800000801     C*----------------------------------------------------*
028900000801     C     DEFCAM        BEGSR
029000000801     C*
029100020619     C* Imposto i valori di default...
029200030310     C                   Z-ADD     0501331       VABCCM
029300030310     C                   Z-ADD     0501331       VATCCM
029400021017     C                   Z-ADD     vlrpoi        VABFGS
029500021017     C                   Z-ADD     vlrpoi        VATFGS
029600030226     C                   Z-ADD     050           VABLNP
029700030226     C                   Z-ADD     050           VATLNP
029800030226     C                   Z-ADD     000           VABCTR
029900021017     C                   MOVEL     '7Q'          VABCTM
030000020305     C                   MOVEL     'E'           VATTRC
030100020619     C* ... e poi verifico se sono stati passati come parametri
030200020619     C                   IF        vlrppt > *blanks
030300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030400020619     C                   EXSR      CHKNUM
030500020619     C                   IF        PiInt=*on
030600020619     C                   Z-ADD     PiVal         VABCCM
030700020619     C                   Z-ADD     PiVal         VATCCM
030800020619     C                   ENDIF
030900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031000020619     C                   EXSR      CHKNUM
031100020619     C                   IF        PiInt=*on
031200020619     C                   Z-ADD     PiVal         VABLNP
031300020619     C                   Z-ADD     PiVal         VATLNP
031400020619     C                   ENDIF
031500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031600020619     C                   EXSR      CHKNUM
031700020619     C                   IF        PiInt=*on
031800020619     C                   Z-ADD     PiVal         VABCTR
031900020619     C                   ENDIF
032000020619     C                   ENDIF
032100000801     C*
032200000801     C                   ENDSR
032300000801     C*----------------------------------------------------*
032400021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032500000801     C*----------------------------------------------------*
032600010201     C     IMPVAB        BEGSR
032700000801     C*
032800020305     C                   EXSR      INZVAR
032900020305     C                   EXSR      DEFCAM
033000010305     C*
033100030226     C                   Z-ADD     *zeros        errore            1 0
033200000830     C                   MOVEL     datcor        VABAAS
033300000830     C                   MOVE      datcor        VABMGS
033400030226     C*
033500030310     C                   EVAL      VABRSD=%trim(%subst(vindta:82:35))
033600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033800020117     C* ==
033900030423     C* Verifico particolari condizioni
034000030423     C                   EVAL      VABLOD=%trim(%subst(vindta:187:35))
034100030423     C                   IF        %subst(vindta:152:35) <> *blanks
034200030423     C                   EVAL      VABIND=%trim(%subst(vindta:152:35))
034300030423     C                   EVAL      VABRD2=%trim(%subst(vindta:117:35))
034400030423     C                   EVAL      VABNT2=%trim(%subst(vindta:47:35))
034500030423     C                   ELSE
034600030423     C                   EVAL      VABIND=%trim(%subst(vindta:117:35))
034700030423     C                   EVAL      VABRD2=%trim(%subst(vindta:47:35))
034800030423     C                   ENDIF
034900030423     C* ...infine
035000030423     C                   IF        VABRSD = *blanks
035100030423     C                   EVAL      VABRSD = VABRD2
035200030423     C                   EVAL      VABRD2 = VABNT2
035300030423     C                   EVAL      VABNT2 = *blanks
035400030423     C                   ENDIF
035500030423     C*
035600030310     C                   EVAL      VABPRD=%trim(%subst(vindta:222:2))
035700030317     C                   EVAL      VABRMA=%trim(%subst(vindta:25:9))
035800030310     C                   EVAL      VABNAS=%trim(%subst(vindta:266:70))
035900030410     C                   IF        %subst(vindta:234:15) <> *blanks
036000030410     C                   EVAL      VABNOT='tel'+%trim(%subst(vindta:234:15))
036100030410     C                   ENDIF
036200030410     C                   IF        %subst(vindta:382:1) = 'T'
036300030410     C                   EVAL      VABNOT=%trim(VABNOT)+' '+
036400030410     C                                    'T1'+%subst(vindta:391:8)+' del'+
036500030410     C                                    %subst(vindta:385:6)
036600030410     C                   ENDIF
036700030311     C                   EVAL      VABVCA='EUR'
036800030226     C* CAP
036900030310     C                   EVAL      PiStr=%trim(%subst(vindta:224:5))
037000010201     C                   EXSR      CHKNUM
037100010201     C                   IF        PiInt=*on
037200010201     C                   Z-ADD     PiVal         Num5_0
037300021017     C                   MOVEL     Num5_0        VABCAD
037400010201     C                   ELSE
037500010201     C                   ADD       1             errore
037600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037700021017     C                             + ' ' + 'VABCAD'
037800010201     C                   ENDIF
037900030311     C* NSP
038000030310     C                   EVAL      PiStr=%trim(%subst(vindta:25:9))
038100010201     C                   EXSR      CHKNUM
038200010201     C                   IF        PiInt=*on
038300030107     C                   Z-ADD     PiVal         Num7_0
038400030107     C                   MOVE(P)   Num7_0        VABNSP
038500030107     C                   MOVE(P)   Num7_0        VATNSP
038600010201     C                   ELSE
038700010201     C                   ADD       1             errore
038800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038900030311     C                             + ' ' + 'VABNSP VATNSP'
039000010201     C                   ENDIF
039100030314     C* RMN
039200030311     C                   EVAL      PiStr=%trim(%subst(vindta:1:12))
039300030311     C                   EXSR      CHKNUM
039400030311     C                   IF        PiInt=*on
039500030311     C                   Z-ADD     PiVal         VABRMN
039600030311     C                   ELSE
039700030311     C                   ADD       1             errore
039800030311     C                   EVAL      vinmsg = %trimr(vinmsg)
039900030311     C                             + ' ' + 'VABRMN'
040000030311     C                   ENDIF
040100030226     C* NCL
040200030310     C                   EVAL      PiStr=%trim(%subst(vindta:249:5))
040300030107     C                   EXSR      CHKNUM
040400030107     C                   IF        PiInt=*on
040500030107     C                   Z-ADD     PiVal         VABNCL
040600030107     C                   ELSE
040700030107     C                   ADD       1             errore
040800030107     C                   EVAL      vinmsg = %trimr(vinmsg)
040900030107     C                             + ' ' + 'VABNCL'
041000030107     C                   ENDIF
041100030226     C* PKB
041200030310     C                   EVAL      PiStr=%trim(%subst(vindta:255:11))
041300030107     C                   EXSR      CHKNUM
041400030107     C                   IF        PiNum=*on
041500030310     C                   EVAL      PiVal = PiVal / 100                          *gestisco 2 decimali
041600030310     C                   IF        %subst(vindta:254:1) = 'K'
041700030310     C                   Z-ADD(H)  PiVal         VABPKB                         * se Kg.
041800030423     C                   ADD(H)    PiVal         §CTRPKB
041900030310     C                   ELSE
042000030310     C                   EVAL      PiVal = PiVal * (1/0,450)
042100030310     C                   Z-ADD(H)  PiVal         VABPKB                         * se Libbre
042200030423     C                   ADD(H)    PiVal         §CTRPKB
042300030310     C                   ENDIF
042400030107     C                   ELSE
042500030107     C                   ADD       1             errore
042600030107     C                   EVAL      vinmsg = %trimr(vinmsg)
042700030107     C                             + ' ' + 'VABPKB'
042800030107     C                   ENDIF
042900030410     C* VLB
043000030410     C                   Z-ADD     *zeros        wLen              3 0
043100030410     C                   Z-ADD     *zeros        wWidth            3 0
043200030410     C                   Z-ADD     *zeros        wHeight           3 0
043300030410     C                   Z-ADD     *zeros        wVLB             30 9
043400030410     C*
043500030410     C                   EVAL      PiStr=%trim(%subst(vindta:399:3))
043600030410     C                   EXSR      CHKNUM
043700030410     C                   IF        PiInt=*on
043800030410     C                   Z-ADD     PiVal         wLen
043900030410     C                   ELSE
044000030410     C                   ADD       1             errore
044100030410     C                   EVAL      vinmsg = %trimr(vinmsg)
044200030410     C                             + ' ' + 'VABVLB'
044300030410     C                   ENDIF
044400030410     C*
044500030410     C                   EVAL      PiStr=%trim(%subst(vindta:402:3))
044600030410     C                   EXSR      CHKNUM
044700030410     C                   IF        PiInt=*on
044800030410     C                   Z-ADD     PiVal         wWidth
044900030410     C                   ELSE
045000030410     C                   ADD       1             errore
045100030410     C                   EVAL      vinmsg = %trimr(vinmsg)
045200030410     C                             + ' ' + 'VABVLB'
045300030410     C                   ENDIF
045400030410     C*
045500030410     C                   EVAL      PiStr=%trim(%subst(vindta:405:3))
045600030410     C                   EXSR      CHKNUM
045700030410     C                   IF        PiInt=*on
045800030410     C                   Z-ADD     PiVal         wHeight
045900030410     C                   ELSE
046000030410     C                   ADD       1             errore
046100030410     C                   EVAL      vinmsg = %trimr(vinmsg)
046200030410     C                             + ' ' + 'VABVLB'
046300030410     C                   ENDIF
046400030410     C* => x cui volume = wLen * wWidth * wHeight
046500030410     C                   EVAL      wVLB = (wLen * wWidth * wHeight)/1000000
046600030411     C                   ADD       wVLB          VABVLB
046700030310     C*
046800030310     C* Se richiesto pagamento oneri doganali
046900030310     C                   IF        %subst(vindta:348:1) = 'R'
047000051213     C*
047100051213     C* Se trattasi d spedizione in contrassegno imposto differente codice cliente mittente
047200051213     C* e tariffa specifica in quanto il cliente necessita d fatturazione separata
047300060102     C*                  Z-ADD     0502924       VABCCM
047400060102     C*                  Z-ADD     0502924       VATCCM
047500060102     C*                  Z-ADD     000           VABCTR
047600051213     C*
047700030310     C                   MOVEL     '1'           FlgCAS
047800030311     C                   EVAL      PiStr=%trim(%subst(vindta:356:11))
047900030310     C*
048000030310     C                   EXSR      CHKNUM
048100030310     C                   IF        PiNum=*on
048200030310     C                   EVAL      PiVal = PiVal / 100                          *gestisco 2 decimali
048300030310     C                   Z-ADD     PiVal         VABCAS
048400030310     C                   MOVEL     'BM'          VABTIC
048500030310     C                   ELSE
048600030310     C                   SETON                                        32
048700030310     C                   Z-ADD     *zeros        VABCAS
048800030310     C                   EVAL      vinmsg = %trimr(vinmsg)
048900030310     C                             + ' ' + 'VABCAS'
049000030310     C                   ENDIF
049100030310     C                   ENDIF
049200030310     C*
049300030310     C* Considerazioni finali su CBO/CAS
049400030310     C                   IF        FlgCAS = '1'
049500010205     C                   EVAL      VABCBO = '4'
049600010205     C                   ELSE
049700010205     C                   EVAL      VABCBO = '1'
049800010205     C                   ENDIF
049900020305     C*
050000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050100011113     C                   EXSR      CHKIMPDIV
050200010202     C*
050300000801     C* Ebbene...
050400000801     C                   ADD       1             §CTRMO
050500010201     C                   IF        errore <> *zeros
050600000801     C                   ADD       1             §CTRNO
050700000801     C                   EVAL      recko = vindta
050800000801     C                   EXCEPT    rigadett
050900000801     C                   EVAL      vinflg = '2'
051000000801     C                   ELSE
051100010201     C                   ADD       1             §CTROKVB
051200000801     C                   ENDIF
051300000801     C*
051400000801     C                   ENDSR
051500030226     C*----------------------------------------------------*
051600030226     C*  SCARICAMENTO BUFFER RECORDS VAB
051700030226     C*----------------------------------------------------*
051800030226     C     WRIVAB        BEGSR
051900030310     C*
052000030310     C                   write     FIVAB000
052100030226     C*
052200030226     C                   ENDSR
052300030226     C*----------------------------------------------------*
052400030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
052500030226     C*----------------------------------------------------*
052600030226     C     IMPVAT        BEGSR
052700030226     C*
052800030226     C                   EXSR      INZVAR
052900030226     C                   EXSR      DEFCAM
053000030226     C*
053100030226     C                   MOVEL     datcor        VATAAS
053200030226     C*
053300030310     C* Verifico se prendere come barcode l'AWB o l'MPS
053400030310     C                   IF        %subst(vindta:13:12) > *zeros
053500030310     C                   EVAL      VATNOT=%trim(%subst(vindta:13:12))
053600030310     C                   ELSE
053700030310     C                   EVAL      VATNOT=%trim(%subst(vindta:1:12))
053800030310     C                   ENDIF
053900030226     C*
054000030226     C                   ENDSR
054100010201     C*----------------------------------------------------*
054200021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
054300010201     C*----------------------------------------------------*
054400020305     C     WRIVAT        BEGSR
054500010201     C*
054600021017     C                   write     FIVAT000
054700010201     C*
054800010201     C                   ENDSR
054900010202     C*----------------------------------------------------*
055000021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
055100010202     C*----------------------------------------------------*
055200020305     C     PREVAT        BEGSR
055300010202     C*
055400021017     C* Compongo il nome del membro da dare al FIVATWWR
055500010202     C                   eval      parmbr = vlrhdl
055600010202     C                   movel     'M'           parmbr
055700030415     C                   eval      parccm = '00501331'
055800010202     C                   eval      paropz = '1'
055900010202     C* Effettuo la chiamata al CLLE preposto
056000050128     C                   call(e)   'TITVVTC'
056100010202     C                   parm                    parccm
056200010202     C                   parm                    parmbr
056300010202     C                   parm                    paropz
056400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056500010202     C                   if        %error
056600010202     C                   movel     '1'           chkcall
056700010202     C                   else
056800010202     C                   movel     '0'           chkcall
056900010202     C                   endif
057000010202     C*
057100010202     C                   ENDSR
057200000801     C*----------------------------------------------------*
057300000801     C*  CONTROLLO NUMERICITA' CAMPI
057400000801     C*----------------------------------------------------*
057500000801     C     CHKNUM        BEGSR
057600000801     C*
057700000801     C                   call(e)   'ISNUMERIC'
057800000801     C                   PARM                    PiStr            30
057900030107     C                   PARM      ','           PiDecChr          1
058000000801     C                   PARM      *ZEROS        PiVal            30 9
058100000801     C                   PARM      '0'           PiInt             1
058200000801     C                   PARM      '0'           PiNum             1
058300000801     C                   IF        %error
058400000801     C                   EVAL      PiInt=*off
058500030107     C                   EVAL      PiNum=*off
058600000801     C                   ENDIF
058700000801     C*
058800000801     C                   ENDSR
058900000801     C***
059000000801
059100011113
059200011113     C*----------------------------------------------------*
059300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059400011113     C*----------------------------------------------------*
059500011113     C     CHKIMPDIV     BEGSR
059600011113     C*
059700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059800011113     C                   Z-ADD     *zeros        wrkDec            9 9
059900011113     C*
060000011113     C* Come prima cosa effettuo considerazioni sulla divisa
060100011113     C                   IF        vabIAS > *zeros
060200011113     C                   IF        vabVAS <> 'EUR'
060300011113     C                   EVAL      vabVAS =  'ITL'
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C                   IF        vabCAS > *zeros
060800011113     C                   IF        vabVCA <> 'EUR'
060900011113     C                   EVAL      vabVCA =  'ITL'
061000011113     C                   ENDIF
061100011113     C                   ENDIF
061200011113     C*
061300011113     C                   IF        vabVMD > *zeros
061400020305     C                   IF        vabVAD <> 'EUR'
061500011113     C                   EVAL      vabVAD =  'ITL'
061600011113     C                   ENDIF
061700011113     C                   ENDIF
061800011113     C*
061900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062000011113     C                   Z-ADD     vabIAS        wrkDec
062100011113     C                   IF        wrkDec > *zeros
062200011113     C                   IF        vabVAS = 'ITL'
062300011113     C                   EVAL      vabIAS = *zeros
062400011113     C                   ENDIF
062500011113     C                   ENDIF
062600011113     C*
062700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062800011113     C                   Z-ADD     vabCAS        wrkDec
062900011113     C                   IF        wrkDec > *zeros
063000011113     C                   IF        vabVCA = 'ITL'
063100011113     C                   EVAL      vabCAS = *zeros
063200011113     C                   ENDIF
063300011113     C                   ENDIF
063400011113     C*
063500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063600011113     C                   Z-ADD     vabVMD        wrkDec
063700011113     C                   IF        wrkDec > *zeros
063800011113     C                   IF        vabVAD = 'ITL'
063900011113     C                   EVAL      vabVMD = *zeros
064000011113     C                   ENDIF
064100011113     C                   ENDIF
064200011113     C*
064300011113     C                   ENDSR
064400011113     C***
064500021017
064600000801
064700000801
064800990920      /TITLE Invio dei dati al punto operativo.
064900010202     C     invio         BEGSR
065000990920     C*
065100021017     C* 1° invio FIVAT
065200010201     C                   reset                   dscmz
065300021017     C                   move      vlrpoi        cmzdst
065400021017     C                   eval      cmzfld = 'FIVATWWR'
065500010201     C                   eval      cmzmbd = vlrhdl
065600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065700021009     C***                if        prmfir = *blanks
065800021017     C                   eval      cmzfla = 'FIVAT00F'
065900021017     C                   eval      cmzmba = 'FIVAT00F'
066000021009     C***                else
066100021009     C***                eval      cmzfla = prmfir
066200021009     C***                eval      cmzmba = prmfir
066300021009     C***                endif
066400010201     C                   eval      cmznrr = *zeros
066500020305     C                   move      §ctrokvt      cmznrr
066600021018     C                   eval      cmzlba = vlrfl1
066700010201     C                   call(e)   'TIS711C'
066800010201     C                   parm                    dscmz
066900010201     C                   parm      *blanks       esito
067000010205     C                   if        %error
067100010205     C                             or cmzerr = '1'
067200010205     C                             or esito  = '1'
067300010205     C                   eval      wrkesito = '3'
067400010205     C                   else
067500010201     C*
067600021017     C* 2° invio FIVAB
067700010201     C                   reset                   dscmz
067800021017     C                   move      vlrpoi        cmzdst
067900010201     C                   eval      cmzfld = vlrfou
068000010201     C                   eval      cmzmbd = vlrhdl
068100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068200021009     C***                if        prmfir = *blanks
068300021017     C                   eval      cmzfla = 'FIVAB00F'
068400021017     C                   eval      cmzmba = 'FIVAB00F'
068500021009     C***                else
068600021009     C***                eval      cmzfla = prmfir
068700021009     C***                eval      cmzmba = prmfir
068800021009     C***                endif
068900010201     C                   eval      cmznrr = *zeros
069000010201     C                   move      §ctrokvb      cmznrr
069100021018     C                   eval      cmzlba = vlrfl1
069200010201     C                   call(e)   'TIS711C'
069300010201     C                   parm                    dscmz
069400010201     C                   parm      *blanks       esito
069500010201     C                   if        %error
069600010201     C                             or cmzerr = '1'
069700010201     C                             or esito  = '1'
069800010201     C                   eval      wrkesito = '3'
069900010201     C                   endif
070000010205     C                   endif
070100990920     C*
070200000613     C                   ENDSR
070300000613     C***
070400070411
070500070411     C     *pssr         BEGSR
070600070411     C*
070700070411     C                   if        %open(tivin00r)
070800070411     C                   close     tivin00r
070900070411     C                   endif
071000070411     C                   if        %open(fivabwwr)
071100070411     C                   close     fivabwwr
071200070411     C                   endif
071300070411     C                   if        %open(fivatwwr)
071400070411     C                   close     fivatwwr
071500070411     C                   endif
071600070411     C*
071700070411     C* Effettuo la chiamata al CLLE preposto
071800070411     C                   call(e)   'TITVVTC'
071900070411     C                   parm                    parccm
072000070411     C                   parm                    parmbr
072100070411     C                   parm      '2'           paropz
072200070411     C*
072300070411     C                   eval      wrkesito = '2'
072400070411     C*
072500070411     C                   seton                                        LR
072600070411     C*
072700070411     C                   ENDSR     '*CANCL'
072800070411     C***
072900070411
073000990910
073100000613     C     *inzsr        BEGSR
073200990910     C*
073300990910     C     *entry        plist
073400990920     C                   parm                    tivlrds
073500990921     C                   parm      wrkesito      esito
073600000724     C                   parm                    prmlit
073700000710     C                   parm                    prmfir
073800000613     C*
073900000830     C* CALCOLA LA DATA CORRENTE
074000000830     C                   time                    wn14             14 0
074100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
074200000830     C                   z-add     wn8           g08dat
074300000830     C                   z-add     *zeros        g08inv
074400000830     C                   movel     '0'           g08err
074500000830     C                   call      'XSRDA8'
074600000830     C                   parm                    wlbda8
074700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
074800000830     C*
074900000613     C                   ENDSR
075000000613     C***
075100990908
075200030310     Otitv40p   E            riepilogo         2
075300990915     O                                              'Upload via Internet'
075400990915     O                                           +1 'Traduzione TIVIN00R -'
075500021017     O                                           +1 'FIVABWWR/FIVATWWR'
075600010201     O                                           +1 'Report testate bolle'
075700990915     O          E            riepilogo   2
075800990915     O                       wrkdata
075900990915     O                       wrkora              +1
076000990915     O                       procname            +1
076100990915     O          E            riepilogo   2
076200990915     O                                              'Cliente..................:'
076300990915     O                       VABCCM        z     +1
076400990915     O          E            riepilogo   2
076500990920     O                                              'Riferimento Strategi.....:'
076600990920     O                       vlrhdl              +1
076700990915     O          E            riepilogo   2
076800030423     O                                              'N°rec. giusti ...........:'
076900010201     O                       §CTROKVB      2   +  1
077000990915     O          E            riepilogo   2
077100030423     O                                              'N°rec. sbagliati e corr. :'
077200971022     O                       §CTRMO        2   +  1
077300990915     O          E            riepilogo   2
077400030423     O                                              'N°rec. sbagliati e scart.:'
077500971022     O                       §CTRNO        2   +  1
077600030423     O          E            riepilogo   2
077700030423     O                                              'Totale peso spedizioni ..:'
077800030423     O                       §CTRPKB       2   +  1
077900030423     O          E            dettT       3
078000030423     O                                           +2 'N° rec'
078100030423     O                                           +3 'Anteprima contenuto'
078200030423     O          E            dettR1      2
078300030423     O                       rrnum               +2
078400030423     O                       dettRec             +3
078500030423     O          E            dettR2      2
078600030423     O                       dettRec            +11
078700030423     O          E            dettR3      2
078800030423     O                       dettRec            +11
078900000613
079000030310     Otitv40ps  E            testdett          2
079100000613     O                                              'Upload via Internet'
079200000613     O                                           +1 'Traduzione TIVIN00R -'
079300021017     O                                           +1 'FIVABWWR/FIVATWWR'
079400010201     O                                           +1 'Report testate bolle'
079500000616     O          E            testdett    3
079600000613     O                                           +2 'N° rec'
079700000613     O                                           +3 'Anteprima contenuto'
079800000616     O          E            rigadett    2
079900000613     O                       rrnum               +2
080000000621     O                       recko               +3
080100000616     O          E            findett     2
080200000613     O                       wrkdata
080300000613     O                       wrkora              +1
080400000613     O                       procname            +1
080500000616     O          E            findett     2
080600030423     O                                              'Cliente .................:'
080700000613     O                       VABCCM        z     +1
080800000616     O          E            findett     2
080900030423     O                                              'Riferimento Strategi ....:'
081000000613     O                       vlrhdl              +1
081100000616     O          E            findett     2
081200030423     O                                              'N°rec. giusti ...........:'
081300010201     O                       §CTROKVB      2   +  1
081400000616     O          E            findett     2
081500030423     O                                              'N°rec. sbagliati e corr. :'
081600000613     O                       §CTRMO        2   +  1
081700000616     O          E            findett     2
081800030423     O                                              'N°rec. sbagliati e scart.:'
081900000613     O                       §CTRNO        2   +  1
082000030423     O          E            findett     2
082100030423     O                                              'Totale peso spedizioni ..:'
082200030423     O                       §CTRPKB       2   +  1
082300000512** CMD - COMANDI CL
082400030423OVRPRTF FILE(TITV40P)  TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
082500030310OVRPRTF FILE(TITV40PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
082600030310DLTOVR FILE(TITV40P TITV40PS) LVL(*)
082700030424
082800000512
