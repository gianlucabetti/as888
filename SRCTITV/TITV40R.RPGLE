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
028400111130     C*
028500111130     C                   MOVEL     *blanks       wVATNOT_A        35
028600111130     C                   MOVEL     *blanks       wVATNOT_B        35
028700000801     C*
028800000801     C                   ENDSR
028900000801     C*----------------------------------------------------*
029000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029100000801     C*----------------------------------------------------*
029200000801     C     DEFCAM        BEGSR
029300000801     C*
029400020619     C* Imposto i valori di default...
029500030310     C                   Z-ADD     0501331       VABCCM
029600030310     C                   Z-ADD     0501331       VATCCM
029700021017     C                   Z-ADD     vlrpoi        VABFGS
029800021017     C                   Z-ADD     vlrpoi        VATFGS
029900030226     C                   Z-ADD     050           VABLNP
030000030226     C                   Z-ADD     050           VATLNP
030100030226     C                   Z-ADD     000           VABCTR
030200021017     C                   MOVEL     '7Q'          VABCTM
030300020305     C                   MOVEL     'E'           VATTRC
030400020619     C* ... e poi verifico se sono stati passati come parametri
030500020619     C                   IF        vlrppt > *blanks
030600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030700020619     C                   EXSR      CHKNUM
030800020619     C                   IF        PiInt=*on
030900020619     C                   Z-ADD     PiVal         VABCCM
031000020619     C                   Z-ADD     PiVal         VATCCM
031100020619     C                   ENDIF
031200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031300020619     C                   EXSR      CHKNUM
031400020619     C                   IF        PiInt=*on
031500020619     C                   Z-ADD     PiVal         VABLNP
031600020619     C                   Z-ADD     PiVal         VATLNP
031700020619     C                   ENDIF
031800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031900020619     C                   EXSR      CHKNUM
032000020619     C                   IF        PiInt=*on
032100020619     C                   Z-ADD     PiVal         VABCTR
032200020619     C                   ENDIF
032300020619     C                   ENDIF
032400000801     C*
032500000801     C                   ENDSR
032600000801     C*----------------------------------------------------*
032700021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032800000801     C*----------------------------------------------------*
032900010201     C     IMPVAB        BEGSR
033000000801     C*
033100020305     C                   EXSR      INZVAR
033200020305     C                   EXSR      DEFCAM
033300010305     C*
033400030226     C                   Z-ADD     *zeros        errore            1 0
033500000830     C                   MOVEL     datcor        VABAAS
033600000830     C                   MOVE      datcor        VABMGS
033700030226     C*
033800111130     C                   IF        %subst(vindta:82:35) <> *blanks
033900111130     C                   EVAL      VABRSD=%trim(%subst(vindta:82:35))
034000111130     C                   EVAL      wVATNOT_A = %trim(%subst(vindta:47:35))
034100111130     C                   ELSE
034200111130     C                   EVAL      VABRSD=%trim(%subst(vindta:47:35))
034300111130     C                   ENDIF
034400111130     C*
034500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034700020117     C* ==
034800111130     C                   EVAL      VABRD2=%trim(%subst(vindta:152:35))
034900111130     C                   EVAL      VABIND=%trim(%subst(vindta:117:35))
035000111130     C                   EVAL      VABLOD=%trim(%subst(vindta:187:35))
035100030423     C*
035200030310     C                   EVAL      VABPRD=%trim(%subst(vindta:222:2))
035300030317     C                   EVAL      VABRMA=%trim(%subst(vindta:25:9))
035400030310     C                   EVAL      VABNAS=%trim(%subst(vindta:266:70))
035500030410     C                   IF        %subst(vindta:234:15) <> *blanks
035600030410     C                   EVAL      VABNOT='tel'+%trim(%subst(vindta:234:15))
035700030410     C                   ENDIF
035800030410     C                   IF        %subst(vindta:382:1) = 'T'
035900030410     C                   EVAL      VABNOT=%trim(VABNOT)+' '+
036000030410     C                                    'T1'+%subst(vindta:391:8)+' del'+
036100030410     C                                    %subst(vindta:385:6)
036200030410     C                   ENDIF
036300030311     C                   EVAL      VABVCA='EUR'
036400030226     C* CAP
036500030310     C                   EVAL      PiStr=%trim(%subst(vindta:224:5))
036600010201     C                   EXSR      CHKNUM
036700010201     C                   IF        PiInt=*on
036800010201     C                   Z-ADD     PiVal         Num5_0
036900021017     C                   MOVEL     Num5_0        VABCAD
037000010201     C                   ELSE
037100010201     C                   ADD       1             errore
037200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037300021017     C                             + ' ' + 'VABCAD'
037400010201     C                   ENDIF
037500030311     C* NSP
037600030310     C                   EVAL      PiStr=%trim(%subst(vindta:25:9))
037700010201     C                   EXSR      CHKNUM
037800010201     C                   IF        PiInt=*on
037900030107     C                   Z-ADD     PiVal         Num7_0
038000030107     C                   MOVE(P)   Num7_0        VABNSP
038100030107     C                   MOVE(P)   Num7_0        VATNSP
038200010201     C                   ELSE
038300010201     C                   ADD       1             errore
038400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038500030311     C                             + ' ' + 'VABNSP VATNSP'
038600010201     C                   ENDIF
038700030314     C* RMN
038800030311     C                   EVAL      PiStr=%trim(%subst(vindta:1:12))
038900030311     C                   EXSR      CHKNUM
039000030311     C                   IF        PiInt=*on
039100030311     C                   Z-ADD     PiVal         VABRMN
039200030311     C                   ELSE
039300030311     C                   ADD       1             errore
039400030311     C                   EVAL      vinmsg = %trimr(vinmsg)
039500030311     C                             + ' ' + 'VABRMN'
039600030311     C                   ENDIF
039700030226     C* NCL
039800030310     C                   EVAL      PiStr=%trim(%subst(vindta:249:5))
039900030107     C                   EXSR      CHKNUM
040000030107     C                   IF        PiInt=*on
040100030107     C                   Z-ADD     PiVal         VABNCL
040200030107     C                   ELSE
040300030107     C                   ADD       1             errore
040400030107     C                   EVAL      vinmsg = %trimr(vinmsg)
040500030107     C                             + ' ' + 'VABNCL'
040600030107     C                   ENDIF
040700030226     C* PKB
040800030310     C                   EVAL      PiStr=%trim(%subst(vindta:255:11))
040900030107     C                   EXSR      CHKNUM
041000030107     C                   IF        PiNum=*on
041100030310     C                   EVAL      PiVal = PiVal / 100                          *gestisco 2 decimali
041200030310     C                   IF        %subst(vindta:254:1) = 'K'
041300030310     C                   Z-ADD(H)  PiVal         VABPKB                         * se Kg.
041400030423     C                   ADD(H)    PiVal         §CTRPKB
041500030310     C                   ELSE
041600120216     C                   EVAL      PiVal = (PiVal * 0,450) / 100
041700030310     C                   Z-ADD(H)  PiVal         VABPKB                         * se Libbre
041800030423     C                   ADD(H)    PiVal         §CTRPKB
041900030310     C                   ENDIF
042000030107     C                   ELSE
042100030107     C                   ADD       1             errore
042200030107     C                   EVAL      vinmsg = %trimr(vinmsg)
042300030107     C                             + ' ' + 'VABPKB'
042400030107     C                   ENDIF
042500030410     C* VLB
042600030410     C                   Z-ADD     *zeros        wLen              3 0
042700030410     C                   Z-ADD     *zeros        wWidth            3 0
042800030410     C                   Z-ADD     *zeros        wHeight           3 0
042900030410     C                   Z-ADD     *zeros        wVLB             30 9
043000030410     C*
043100030410     C                   EVAL      PiStr=%trim(%subst(vindta:399:3))
043200030410     C                   EXSR      CHKNUM
043300030410     C                   IF        PiInt=*on
043400030410     C                   Z-ADD     PiVal         wLen
043500030410     C                   ELSE
043600030410     C                   ADD       1             errore
043700030410     C                   EVAL      vinmsg = %trimr(vinmsg)
043800030410     C                             + ' ' + 'VABVLB'
043900030410     C                   ENDIF
044000030410     C*
044100030410     C                   EVAL      PiStr=%trim(%subst(vindta:402:3))
044200030410     C                   EXSR      CHKNUM
044300030410     C                   IF        PiInt=*on
044400030410     C                   Z-ADD     PiVal         wWidth
044500030410     C                   ELSE
044600030410     C                   ADD       1             errore
044700030410     C                   EVAL      vinmsg = %trimr(vinmsg)
044800030410     C                             + ' ' + 'VABVLB'
044900030410     C                   ENDIF
045000030410     C*
045100030410     C                   EVAL      PiStr=%trim(%subst(vindta:405:3))
045200030410     C                   EXSR      CHKNUM
045300030410     C                   IF        PiInt=*on
045400030410     C                   Z-ADD     PiVal         wHeight
045500030410     C                   ELSE
045600030410     C                   ADD       1             errore
045700030410     C                   EVAL      vinmsg = %trimr(vinmsg)
045800030410     C                             + ' ' + 'VABVLB'
045900030410     C                   ENDIF
046000030410     C* => x cui volume = wLen * wWidth * wHeight
046100030410     C                   EVAL      wVLB = (wLen * wWidth * wHeight)/1000000
046200030411     C                   ADD       wVLB          VABVLB
046300030310     C*
046400030310     C* Se richiesto pagamento oneri doganali
046500030310     C                   IF        %subst(vindta:348:1) = 'R'
046600051213     C*
046700051213     C* Se trattasi d spedizione in contrassegno imposto differente codice cliente mittente
046800051213     C* e tariffa specifica in quanto il cliente necessita d fatturazione separata
046900060102     C*                  Z-ADD     0502924       VABCCM
047000060102     C*                  Z-ADD     0502924       VATCCM
047100060102     C*                  Z-ADD     000           VABCTR
047200051213     C*
047300030310     C                   MOVEL     '1'           FlgCAS
047400030311     C                   EVAL      PiStr=%trim(%subst(vindta:356:11))
047500030310     C*
047600030310     C                   EXSR      CHKNUM
047700030310     C                   IF        PiNum=*on
047800030310     C                   EVAL      PiVal = PiVal / 100                          *gestisco 2 decimali
047900030310     C                   Z-ADD     PiVal         VABCAS
048000030310     C                   MOVEL     'BM'          VABTIC
048100030310     C                   ELSE
048200030310     C                   SETON                                        32
048300030310     C                   Z-ADD     *zeros        VABCAS
048400030310     C                   EVAL      vinmsg = %trimr(vinmsg)
048500030310     C                             + ' ' + 'VABCAS'
048600030310     C                   ENDIF
048700030310     C                   ENDIF
048800030310     C*
048900030310     C* Considerazioni finali su CBO/CAS
049000030310     C                   IF        FlgCAS = '1'
049100010205     C                   EVAL      VABCBO = '4'
049200010205     C                   ELSE
049300010205     C                   EVAL      VABCBO = '1'
049400010205     C                   ENDIF
049500020305     C*
049600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049700011113     C                   EXSR      CHKIMPDIV
049800010202     C*
049900000801     C* Ebbene...
050000000801     C                   ADD       1             §CTRMO
050100010201     C                   IF        errore <> *zeros
050200000801     C                   ADD       1             §CTRNO
050300000801     C                   EVAL      recko = vindta
050400000801     C                   EXCEPT    rigadett
050500000801     C                   EVAL      vinflg = '2'
050600000801     C                   ELSE
050700010201     C                   ADD       1             §CTROKVB
050800000801     C                   ENDIF
050900000801     C*
051000000801     C                   ENDSR
051100030226     C*----------------------------------------------------*
051200030226     C*  SCARICAMENTO BUFFER RECORDS VAB
051300030226     C*----------------------------------------------------*
051400030226     C     WRIVAB        BEGSR
051500030310     C*
051600030310     C                   write     FIVAB000
051700030226     C*
051800030226     C                   ENDSR
051900030226     C*----------------------------------------------------*
052000030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
052100030226     C*----------------------------------------------------*
052200030226     C     IMPVAT        BEGSR
052300030226     C*
052400030226     C                   EXSR      INZVAR
052500030226     C                   EXSR      DEFCAM
052600030226     C*
052700030226     C                   MOVEL     datcor        VATAAS
052800030226     C*
052900030310     C* Verifico se prendere come barcode l'AWB o l'MPS
053000030310     C                   IF        %subst(vindta:13:12) > *zeros
053100030310     C                   EVAL      VATNOT=%trim(%subst(vindta:13:12))
053200030310     C                   ELSE
053300030310     C                   EVAL      VATNOT=%trim(%subst(vindta:1:12))
053400030310     C                   ENDIF
053500030226     C*
053600030226     C                   ENDSR
053700010201     C*----------------------------------------------------*
053800021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
053900010201     C*----------------------------------------------------*
054000020305     C     WRIVAT        BEGSR
054100010201     C*
054200021017     C                   write     FIVAT000
054300010201     C*
054400010201     C                   ENDSR
054500010202     C*----------------------------------------------------*
054600021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054700010202     C*----------------------------------------------------*
054800020305     C     PREVAT        BEGSR
054900010202     C*
055000021017     C* Compongo il nome del membro da dare al FIVATWWR
055100010202     C                   eval      parmbr = vlrhdl
055200010202     C                   movel     'M'           parmbr
055300030415     C                   eval      parccm = '00501331'
055400010202     C                   eval      paropz = '1'
055500010202     C* Effettuo la chiamata al CLLE preposto
055600050128     C                   call(e)   'TITVVTC'
055700010202     C                   parm                    parccm
055800010202     C                   parm                    parmbr
055900010202     C                   parm                    paropz
056000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056100010202     C                   if        %error
056200010202     C                   movel     '1'           chkcall
056300010202     C                   else
056400010202     C                   movel     '0'           chkcall
056500010202     C                   endif
056600010202     C*
056700010202     C                   ENDSR
056800000801     C*----------------------------------------------------*
056900000801     C*  CONTROLLO NUMERICITA' CAMPI
057000000801     C*----------------------------------------------------*
057100000801     C     CHKNUM        BEGSR
057200000801     C*
057300000801     C                   call(e)   'ISNUMERIC'
057400000801     C                   PARM                    PiStr            30
057500030107     C                   PARM      ','           PiDecChr          1
057600000801     C                   PARM      *ZEROS        PiVal            30 9
057700000801     C                   PARM      '0'           PiInt             1
057800000801     C                   PARM      '0'           PiNum             1
057900000801     C                   IF        %error
058000000801     C                   EVAL      PiInt=*off
058100030107     C                   EVAL      PiNum=*off
058200000801     C                   ENDIF
058300000801     C*
058400000801     C                   ENDSR
058500000801     C***
058600000801
058700011113
058800011113     C*----------------------------------------------------*
058900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059000011113     C*----------------------------------------------------*
059100011113     C     CHKIMPDIV     BEGSR
059200011113     C*
059300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059400011113     C                   Z-ADD     *zeros        wrkDec            9 9
059500011113     C*
059600011113     C* Come prima cosa effettuo considerazioni sulla divisa
059700011113     C                   IF        vabIAS > *zeros
059800011113     C                   IF        vabVAS <> 'EUR'
059900011113     C                   EVAL      vabVAS =  'ITL'
060000011113     C                   ENDIF
060100011113     C                   ENDIF
060200011113     C*
060300011113     C                   IF        vabCAS > *zeros
060400011113     C                   IF        vabVCA <> 'EUR'
060500011113     C                   EVAL      vabVCA =  'ITL'
060600011113     C                   ENDIF
060700011113     C                   ENDIF
060800011113     C*
060900011113     C                   IF        vabVMD > *zeros
061000020305     C                   IF        vabVAD <> 'EUR'
061100011113     C                   EVAL      vabVAD =  'ITL'
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061600011113     C                   Z-ADD     vabIAS        wrkDec
061700011113     C                   IF        wrkDec > *zeros
061800011113     C                   IF        vabVAS = 'ITL'
061900011113     C                   EVAL      vabIAS = *zeros
062000011113     C                   ENDIF
062100011113     C                   ENDIF
062200011113     C*
062300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062400011113     C                   Z-ADD     vabCAS        wrkDec
062500011113     C                   IF        wrkDec > *zeros
062600011113     C                   IF        vabVCA = 'ITL'
062700011113     C                   EVAL      vabCAS = *zeros
062800011113     C                   ENDIF
062900011113     C                   ENDIF
063000011113     C*
063100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063200011113     C                   Z-ADD     vabVMD        wrkDec
063300011113     C                   IF        wrkDec > *zeros
063400011113     C                   IF        vabVAD = 'ITL'
063500011113     C                   EVAL      vabVMD = *zeros
063600011113     C                   ENDIF
063700011113     C                   ENDIF
063800011113     C*
063900011113     C                   ENDSR
064000011113     C***
064100021017
064200000801
064300000801
064400990920      /TITLE Invio dei dati al punto operativo.
064500010202     C     invio         BEGSR
064600990920     C*
064700021017     C* 1° invio FIVAT
064800010201     C                   reset                   dscmz
064900021017     C                   move      vlrpoi        cmzdst
065000021017     C                   eval      cmzfld = 'FIVATWWR'
065100010201     C                   eval      cmzmbd = vlrhdl
065200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065300021009     C***                if        prmfir = *blanks
065400021017     C                   eval      cmzfla = 'FIVAT00F'
065500021017     C                   eval      cmzmba = 'FIVAT00F'
065600021009     C***                else
065700021009     C***                eval      cmzfla = prmfir
065800021009     C***                eval      cmzmba = prmfir
065900021009     C***                endif
066000010201     C                   eval      cmznrr = *zeros
066100020305     C                   move      §ctrokvt      cmznrr
066200021018     C                   eval      cmzlba = vlrfl1
066300010201     C                   call(e)   'TIS711C'
066400010201     C                   parm                    dscmz
066500010201     C                   parm      *blanks       esito
066600010205     C                   if        %error
066700010205     C                             or cmzerr = '1'
066800010205     C                             or esito  = '1'
066900010205     C                   eval      wrkesito = '3'
067000010205     C                   else
067100010201     C*
067200021017     C* 2° invio FIVAB
067300010201     C                   reset                   dscmz
067400021017     C                   move      vlrpoi        cmzdst
067500010201     C                   eval      cmzfld = vlrfou
067600010201     C                   eval      cmzmbd = vlrhdl
067700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067800021009     C***                if        prmfir = *blanks
067900021017     C                   eval      cmzfla = 'FIVAB00F'
068000021017     C                   eval      cmzmba = 'FIVAB00F'
068100021009     C***                else
068200021009     C***                eval      cmzfla = prmfir
068300021009     C***                eval      cmzmba = prmfir
068400021009     C***                endif
068500010201     C                   eval      cmznrr = *zeros
068600010201     C                   move      §ctrokvb      cmznrr
068700021018     C                   eval      cmzlba = vlrfl1
068800010201     C                   call(e)   'TIS711C'
068900010201     C                   parm                    dscmz
069000010201     C                   parm      *blanks       esito
069100010201     C                   if        %error
069200010201     C                             or cmzerr = '1'
069300010201     C                             or esito  = '1'
069400010201     C                   eval      wrkesito = '3'
069500010201     C                   endif
069600010205     C                   endif
069700990920     C*
069800000613     C                   ENDSR
069900000613     C***
070000070411
070100070411     C     *pssr         BEGSR
070200070411     C*
070300070411     C                   if        %open(tivin00r)
070400070411     C                   close     tivin00r
070500070411     C                   endif
070600070411     C                   if        %open(fivabwwr)
070700070411     C                   close     fivabwwr
070800070411     C                   endif
070900070411     C                   if        %open(fivatwwr)
071000070411     C                   close     fivatwwr
071100070411     C                   endif
071200070411     C*
071300070411     C* Effettuo la chiamata al CLLE preposto
071400070411     C                   call(e)   'TITVVTC'
071500070411     C                   parm                    parccm
071600070411     C                   parm                    parmbr
071700070411     C                   parm      '2'           paropz
071800070411     C*
071900070411     C                   eval      wrkesito = '2'
072000070411     C*
072100070411     C                   seton                                        LR
072200070411     C*
072300070411     C                   ENDSR     '*CANCL'
072400070411     C***
072500070411
072600990910
072700000613     C     *inzsr        BEGSR
072800990910     C*
072900990910     C     *entry        plist
073000990920     C                   parm                    tivlrds
073100990921     C                   parm      wrkesito      esito
073200000724     C                   parm                    prmlit
073300000710     C                   parm                    prmfir
073400000613     C*
073500000830     C* CALCOLA LA DATA CORRENTE
073600000830     C                   time                    wn14             14 0
073700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
073800000830     C                   z-add     wn8           g08dat
073900000830     C                   z-add     *zeros        g08inv
074000000830     C                   movel     '0'           g08err
074100000830     C                   call      'XSRDA8'
074200000830     C                   parm                    wlbda8
074300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
074400000830     C*
074500000613     C                   ENDSR
074600000613     C***
074700990908
074800030310     Otitv40p   E            riepilogo         2
074900990915     O                                              'Upload via Internet'
075000990915     O                                           +1 'Traduzione TIVIN00R -'
075100021017     O                                           +1 'FIVABWWR/FIVATWWR'
075200010201     O                                           +1 'Report testate bolle'
075300990915     O          E            riepilogo   2
075400990915     O                       wrkdata
075500990915     O                       wrkora              +1
075600990915     O                       procname            +1
075700990915     O          E            riepilogo   2
075800990915     O                                              'Cliente..................:'
075900990915     O                       VABCCM        z     +1
076000990915     O          E            riepilogo   2
076100990920     O                                              'Riferimento Strategi.....:'
076200990920     O                       vlrhdl              +1
076300990915     O          E            riepilogo   2
076400030423     O                                              'N°rec. giusti ...........:'
076500010201     O                       §CTROKVB      2   +  1
076600990915     O          E            riepilogo   2
076700030423     O                                              'N°rec. sbagliati e corr. :'
076800971022     O                       §CTRMO        2   +  1
076900990915     O          E            riepilogo   2
077000030423     O                                              'N°rec. sbagliati e scart.:'
077100971022     O                       §CTRNO        2   +  1
077200030423     O          E            riepilogo   2
077300030423     O                                              'Totale peso spedizioni ..:'
077400030423     O                       §CTRPKB       2   +  1
077500030423     O          E            dettT       3
077600030423     O                                           +2 'N° rec'
077700030423     O                                           +3 'Anteprima contenuto'
077800030423     O          E            dettR1      2
077900030423     O                       rrnum               +2
078000030423     O                       dettRec             +3
078100030423     O          E            dettR2      2
078200030423     O                       dettRec            +11
078300030423     O          E            dettR3      2
078400030423     O                       dettRec            +11
078500000613
078600030310     Otitv40ps  E            testdett          2
078700000613     O                                              'Upload via Internet'
078800000613     O                                           +1 'Traduzione TIVIN00R -'
078900021017     O                                           +1 'FIVABWWR/FIVATWWR'
079000010201     O                                           +1 'Report testate bolle'
079100000616     O          E            testdett    3
079200000613     O                                           +2 'N° rec'
079300000613     O                                           +3 'Anteprima contenuto'
079400000616     O          E            rigadett    2
079500000613     O                       rrnum               +2
079600000621     O                       recko               +3
079700000616     O          E            findett     2
079800000613     O                       wrkdata
079900000613     O                       wrkora              +1
080000000613     O                       procname            +1
080100000616     O          E            findett     2
080200030423     O                                              'Cliente .................:'
080300000613     O                       VABCCM        z     +1
080400000616     O          E            findett     2
080500030423     O                                              'Riferimento Strategi ....:'
080600000613     O                       vlrhdl              +1
080700000616     O          E            findett     2
080800030423     O                                              'N°rec. giusti ...........:'
080900010201     O                       §CTROKVB      2   +  1
081000000616     O          E            findett     2
081100030423     O                                              'N°rec. sbagliati e corr. :'
081200000613     O                       §CTRMO        2   +  1
081300000616     O          E            findett     2
081400030423     O                                              'N°rec. sbagliati e scart.:'
081500000613     O                       §CTRNO        2   +  1
081600030423     O          E            findett     2
081700030423     O                                              'Totale peso spedizioni ..:'
081800030423     O                       §CTRPKB       2   +  1
081900000512** CMD - COMANDI CL
082000030423OVRPRTF FILE(TITV40P)  TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
082100030310OVRPRTF FILE(TITV40PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
082200030310DLTOVR FILE(TITV40P TITV40PS) LVL(*)
082300030424
082400000512
