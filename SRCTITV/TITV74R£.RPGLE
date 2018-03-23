000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800031117     Ftitv74p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100031117     Ftitv74ps  O    f  198        PRINTER usropn
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
003500031117     D depspe          s              6    INZ(*blanks)
003600031117     D curspe          s              6    INZ(*blanks)
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
009200031117     C                   if        not %open(titv74ps)
009300031117     C                   open      titv74ps
009400000616     C                   except    testdett
009500000613     C                   endif
009600000613     C*
009700031117     C                   EXSR      RWLFSPA                                      LETT/SCR. VAB/VAT
009800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009900000613     C*
010000010202     C* Effettuo la chiamata al CLLE preposto
010100030711     C                   call(e)   'TITVVTC'
010200010202     C                   parm                    parccm
010300010202     C                   parm                    parmbr
010400010202     C                   parm      '2'           paropz
010500010202     C*
010600031117     C                   if        %open(titv74ps)
010700000616     C                   except    findett
010800031117     C                   close     titv74ps
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
012600031117     C                   if        not %open(titv74p)
012700031117     C                   open      titv74p
012800990915     C                   endif
012900990915     C*
013000990915     C                   except    riepilogo
013100990915     C*
013200031117     C                   if        %open(titv74p)
013300031117     C                   close     titv74p
013400990914     C                   endif
013500990910     C*
013600000613     C                   ENDSR
013700000613     C***
013800990908
013900000801
014000910830     C*--------------------------------------------------------
014100031117     C* RWLFSPA   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
014200910830     C*--------------------------------------------------------
014300031117     C     RWLFSPA       BEGSR
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
017800010305     C*
017900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018000031117     C                   EVAL      PiStr=%trim(%subst(vindta:384:6))
018100020305     C                   MOVEL(p)  PiStr         curspe
018200010305     C*
018300010305     C                   if        depspe = *blanks                             => 1° giro
018400010305     C                   eval      depspe = curspe                              => memorizz. spediz
018500020305     C                   exsr      impvab
018600020305     C                   exsr      wrivat                                       => carico VAT
018700010305     C                   else
018800010305     C                   if        curspe <> depspe                             => rottura di spediz
018900010305     C                   eval      depspe = curspe                              => memorizz. spediz
019000020305     C                   exsr      wrivab
019100020305     C                   exsr      impvab
019200020305     C                   exsr      wrivat                                       => carico VAT
019300020305     C                   else                                                   => x stessa spediz
019400020305     C                   exsr      impvab
019500020305     C                   exsr      wrivat                                       => carico VAT
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
025200111220     C* Forzatura codice tariffa x sped. no Italia
025300111221     C                   if        %subst(vindta:785:1) <> '1' AND
025400111220     C                             vabpkb <= 30
025500111220     C                   eval      vabctr = 100
025600111220     C                   endif
025700111220     C*
025800021017     C                   write     fivab000                                     => scarico il VAB
025900010305     C*
026000010305     C                   ENDSR
026100990920
026200000801     C*----------------------------------------------------*
026300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026400000801     C*----------------------------------------------------*
026500010201     C     INZVAR        BEGSR
026600000801     C*
026700010201     C                   Z-ADD     *zeros        Num5_0
026800030107     C                   Z-ADD     *zeros        Num7_0
026900030711     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027000030711     C                   MOVEL     '0'           FlgCAS            1
027100000801     C*
027200000801     C                   ENDSR
027300000801     C*----------------------------------------------------*
027400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027500000801     C*----------------------------------------------------*
027600000801     C     DEFCAM        BEGSR
027700000801     C*
027800021017     C                   CLEAR                   FIVAB000
027900020619     C* Imposto i valori di default...
028000031117     C                   Z-ADD     1310015       VABCCM
028100031117     C                   Z-ADD     1310015       VATCCM
028200021017     C                   Z-ADD     vlrpoi        VABFGS
028300021017     C                   Z-ADD     vlrpoi        VATFGS
028400031117     C                   Z-ADD     131           VABLNP
028500031117     C                   Z-ADD     131           VATLNP
028600030711     C                   Z-ADD     000           VABCTR
028700021017     C                   MOVEL     '7Q'          VABCTM
028800020305     C                   MOVEL     'E'           VATTRC
028900020619     C* ... e poi verifico se sono stati passati come parametri
029000020619     C                   IF        vlrppt > *blanks
029100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029200020619     C                   EXSR      CHKNUM
029300020619     C                   IF        PiInt=*on
029400020619     C                   Z-ADD     PiVal         VABCCM
029500020619     C                   Z-ADD     PiVal         VATCCM
029600020619     C                   ENDIF
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABLNP
030100020619     C                   Z-ADD     PiVal         VATLNP
030200020619     C                   ENDIF
030300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030400020619     C                   EXSR      CHKNUM
030500020619     C                   IF        PiInt=*on
030600020619     C                   Z-ADD     PiVal         VABCTR
030700020619     C                   ENDIF
030800020619     C                   ENDIF
030900000801     C*
031000000801     C                   ENDSR
031100000801     C*----------------------------------------------------*
031200021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031300000801     C*----------------------------------------------------*
031400010201     C     IMPVAB        BEGSR
031500000801     C*
031600020305     C                   EXSR      INZVAR
031700020305     C                   EXSR      DEFCAM
031800010305     C*
031900000801     C                   Z-ADD     *zeros        errore            1 0
032000000830     C                   MOVEL     datcor        VABAAS
032100020305     C                   MOVEL     datcor        VATAAS
032200000830     C                   MOVE      datcor        VABMGS
032300031117     C*
032400031117     C* Verifico quali campi considerare x l'instradamento al destinatario
032500031117     C                   IF        %subst(vindta:259:40) <> *blanks
032600031117     C                   EVAL      VABRSD=%trim(%subst(vindta:259:40))
032700031117     C                   ELSE
032800031117     C                   EVAL      VABRSD=%trim(%subst(vindta:142:40))
032900031117     C                   ENDIF
033000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033200020117     C* ==
033300031117     C                   IF        %subst(vindta:299:40) <> *blanks
033400031117     C                   EVAL      VABIND=%trim(%subst(vindta:299:40))
033500031117     C                   ELSE
033600031117     C                   EVAL      VABIND=%trim(%subst(vindta:182:40))
033700031117     C                   ENDIF
033800031117     C                   IF        %subst(vindta:344:30) <> *blanks
033900031117     C                   EVAL      VABLOD=%trim(%subst(vindta:344:30))
034000031117     C                   ELSE
034100031117     C                   EVAL      VABLOD=%trim(%subst(vindta:227:30))
034200031117     C                   ENDIF
034300031117     C                   IF        %subst(vindta:374:2) <> *blanks
034400031121     C                   EVAL      VABPRD=%trim(%subst(vindta:374:2))
034500031117     C                   ELSE
034600031121     C                   EVAL      VABPRD=%trim(%subst(vindta:257:2))
034700031117     C                   ENDIF
034800031117     C*
034900060306     C* VABTIC
035000060306     C                   IF        %subst(vindta:583:50) <> *blanks
035100060306     C                   EVAL      VABTIC = 'BM'
035200060306     C                   ENDIF
035300060306     C*
035400041129     C                   EVAL      VABNZD=%trim(%subst(vindta:782:3))
035500031117     C                   EVAL      VABRMA=%trim(%subst(vindta:376:14))
035600031117     C                   EVAL      VABNAS=%trim(%subst(vindta:411:15))
035700031117     C                   EVAL      VABNOT=%trim(%subst(vindta:463:40))
035800031117     C                   EVAL      VABNT2=%trim(%subst(vindta:503:40))
035900041129     C*
036000041129     C* Verifico il "tipo circuito" indicato dal cliente: 0=Bartolini 1=DPD 2=EuroExpress
036100041129     C                   IF        %subst(vindta:785:1) = '1'
036200041129     C                   EVAL      VABCTR=353
036300041129     C                   EVAL      VABCTM='7R'
036400041129     C                   ENDIF
036500041129     C*
036600030711     C* CAD
036700031117     C                   IF        %subst(vindta:339:5) <> *blanks
036800031117     C                   EVAL      PiStr=%trim(%subst(vindta:339:5))
036900031117     C                   ELSE
037000031117     C                   EVAL      PiStr=%trim(%subst(vindta:222:5))
037100031117     C                   ENDIF
037200010201     C                   EXSR      CHKNUM
037300010201     C                   IF        PiInt=*on
037400010201     C                   Z-ADD     PiVal         Num5_0
037500021017     C                   MOVEL     Num5_0        VABCAD
037600010201     C                   ELSE
037700010201     C                   ADD       1             errore
037800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037900021017     C                             + ' ' + 'VABCAD'
038000010201     C                   ENDIF
038100030711     C* RMN
038200050119     C***                EVAL      PiStr=%trim(%subst(vindta:384:6))
038300050119     C                   EVAL      PiStr=%trim(%subst(vindta:376:14))
038400010201     C                   EXSR      CHKNUM
038500010201     C                   IF        PiInt=*on
038600021017     C                   Z-ADD     PiVal         VABRMN
038700010201     C                   ELSE
038800010201     C                   ADD       1             errore
038900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
039000030711     C                             + ' ' + 'VABRMN'
039100010201     C                   ENDIF
039200030711     C* NSP
039300031117     C                   EVAL      PiStr=%trim(%subst(vindta:384:6))
039400030711     C                   EXSR      CHKNUM
039500030711     C                   IF        PiInt=*on
039600030711     C                   Z-ADD     PiVal         Num7_0
039700030711     C                   MOVE(P)   Num7_0        VABNSP
039800030711     C                   MOVE(P)   Num7_0        VATNSP
039900030711     C                   ELSE
040000030711     C                   ADD       1             errore
040100030711     C                   EVAL      vinmsg = %trimr(vinmsg)
040200030711     C                             + ' ' + 'VABNSP VATNSP'
040300030711     C                   ENDIF
040400030711     C* NCL
040500031117     C                   EVAL      PiStr=%trim(%subst(vindta:426:5))
040600030107     C                   EXSR      CHKNUM
040700030107     C                   IF        PiInt=*on
040800030107     C                   Z-ADD     PiVal         VABNCL
040900030107     C                   ELSE
041000030107     C                   ADD       1             errore
041100030107     C                   EVAL      vinmsg = %trimr(vinmsg)
041200030107     C                             + ' ' + 'VABNCL'
041300030107     C                   ENDIF
041400030711     C* PKB
041500031117     C                   EVAL      PiStr=%trim(%subst(vindta:431:10))
041600030107     C                   EXSR      CHKNUM
041700030107     C                   IF        PiNum=*on
041800031117     C                   EVAL      VABPKB = PiVal/10000                         * gestisco 4 decim.
041900030107     C                   ELSE
042000030107     C                   ADD       1             errore
042100030107     C                   EVAL      vinmsg = %trimr(vinmsg)
042200030107     C                             + ' ' + 'VABPKB'
042300030107     C                   ENDIF
042400031117     C* VLB
042500031117     C                   EVAL      PiStr=%trim(%subst(vindta:441:9))
042600031117     C                   EXSR      CHKNUM
042700031117     C                   IF        PiNum=*on
042800050111     C                   EVAL      PiVal = PiVal/10000                          * gestisco 4 dec.
042900050111     C                   Z-ADD     PiVal         VABVLB
043000031117     C                   ELSE
043100031117     C                   ADD       1             errore
043200031117     C                   EVAL      vinmsg = %trimr(vinmsg)
043300031117     C                             + ' ' + 'VABVLB'
043400031117     C                   ENDIF
043500030711     C* CAS
043600031117     C                   IF        %subst(vindta:450:13) <> *zeros
043700030711     C                   MOVEL     '1'           FlgCAS
043800031117     C                   EVAL      PiStr=%trim(%subst(vindta:450:13))
043900030107     C                   EXSR      CHKNUM
044000030107     C                   IF        PiNum=*on
044100031117     C                   EVAL      VABCAS = PiVal/100                           * gestisco 2 decim.
044200030107     C                   ELSE
044300030107     C                   ADD       1             errore
044400030107     C                   EVAL      vinmsg = %trimr(vinmsg)
044500030107     C                             + ' ' + 'VABCAS'
044600030107     C                   ENDIF
044700030107     C                   ENDIF
044800010205     C*
044900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045000031117     C                   EVAL      VABCBO = '1'                                 * default
045100031117     C                   IF        FlgCAS = '0' AND %subst(vindta:410:1) = 'F'
045200031117     C                   EVAL      VABCBO = '1'                                 * franco  NO contr.
045300031117     C                   ENDIF
045400031117     C                   IF        FlgCAS = '1' AND %subst(vindta:410:1) = 'F'
045500031117     C                   EVAL      VABCBO = '4'                                 * franco  + contras.
045600031117     C                   EVAL      VABVCA='EUR'
045700031117     C                   ENDIF
045800031117     C                   IF        FlgCAS = '0' AND %subst(vindta:410:1) = 'A'
045900031117     C                   EVAL      VABCBO = '2'                                 * assegn. NO contr.
046000031117     C                   ENDIF
046100031117     C                   IF        FlgCAS = '1' AND %subst(vindta:410:1) = 'A'
046200031117     C                   EVAL      VABCBO = '6'                                 * assegn. + contras.
046300031117     C                   EVAL      VABVCA='EUR'
046400031117     C                   ENDIF
046500020305     C*
046600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046700011113     C                   EXSR      CHKIMPDIV
046800010202     C*
046900000801     C* Ebbene...
047000000801     C                   ADD       1             §CTRMO
047100010201     C                   IF        errore <> *zeros
047200000801     C                   ADD       1             §CTRNO
047300000801     C                   EVAL      recko = vindta
047400000801     C                   EXCEPT    rigadett
047500000801     C                   EVAL      vinflg = '2'
047600000801     C                   ELSE
047700010201     C                   ADD       1             §CTROKVB
047800000801     C                   ENDIF
047900000801     C*
048000000801     C                   ENDSR
048100010201     C*----------------------------------------------------*
048200021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048300010201     C*----------------------------------------------------*
048400020305     C     WRIVAT        BEGSR
048500010201     C*
048600050926     C* Valorizzo il buffer di scrittura del FIVAT x il "CHI SONO" (barcode)
048700050926     C                   eval      VATTRC = 'E'
048800031117     C                   eval      VATNOT = %triml(%subst(vindta:648:18))
048900021017     C                   write     FIVAT000
049000050926     C*
049100050926     C* Valorizzo il buffer di scrittura del FIVAT x il telefono destinatario
049200050926     C                   eval      VATTRC = 'B'
049300050926     C                   eval      VATNOT = %triml(%subst(vindta:633:15))
049400050926     C                   write     FIVAT000
049500010201     C*
049600010201     C                   ENDSR
049700010202     C*----------------------------------------------------*
049800021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049900010202     C*----------------------------------------------------*
050000020305     C     PREVAT        BEGSR
050100010202     C*
050200021017     C* Compongo il nome del membro da dare al FIVATWWR
050300010202     C                   eval      parmbr = vlrhdl
050400010202     C                   movel     'M'           parmbr
050500031117     C                   eval      parccm = '01310015'
050600010202     C                   eval      paropz = '1'
050700010202     C* Effettuo la chiamata al CLLE preposto
050800030711     C                   call(e)   'TITVVTC'
050900010202     C                   parm                    parccm
051000010202     C                   parm                    parmbr
051100010202     C                   parm                    paropz
051200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051300010202     C                   if        %error
051400010202     C                   movel     '1'           chkcall
051500010202     C                   else
051600010202     C                   movel     '0'           chkcall
051700010202     C                   endif
051800010202     C*
051900010202     C                   ENDSR
052000000801     C*----------------------------------------------------*
052100000801     C*  CONTROLLO NUMERICITA' CAMPI
052200000801     C*----------------------------------------------------*
052300000801     C     CHKNUM        BEGSR
052400000801     C*
052500000801     C                   call(e)   'ISNUMERIC'
052600000801     C                   PARM                    PiStr            30
052700030711     C                   PARM      '.'           PiDecChr          1
052800000801     C                   PARM      *ZEROS        PiVal            30 9
052900000801     C                   PARM      '0'           PiInt             1
053000000801     C                   PARM      '0'           PiNum             1
053100000801     C                   IF        %error
053200000801     C                   EVAL      PiInt=*off
053300030107     C                   EVAL      PiNum=*off
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
059300021017
059400000801
059500000801
059600990920      /TITLE Invio dei dati al punto operativo.
059700010202     C     invio         BEGSR
059800990920     C*
059900021017     C* 1° invio FIVAT
060000010201     C                   reset                   dscmz
060100021017     C                   move      vlrpoi        cmzdst
060200021017     C                   eval      cmzfld = 'FIVATWWR'
060300010201     C                   eval      cmzmbd = vlrhdl
060400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060500021009     C***                if        prmfir = *blanks
060600021017     C                   eval      cmzfla = 'FIVAT00F'
060700021017     C                   eval      cmzmba = 'FIVAT00F'
060800021009     C***                else
060900021009     C***                eval      cmzfla = prmfir
061000021009     C***                eval      cmzmba = prmfir
061100021009     C***                endif
061200010201     C                   eval      cmznrr = *zeros
061300020305     C                   move      §ctrokvt      cmznrr
061400021018     C                   eval      cmzlba = vlrfl1
061500010201     C                   call(e)   'TIS711C'
061600010201     C                   parm                    dscmz
061700010201     C                   parm      *blanks       esito
061800010205     C                   if        %error
061900010205     C                             or cmzerr = '1'
062000010205     C                             or esito  = '1'
062100010205     C                   eval      wrkesito = '3'
062200010205     C                   else
062300010201     C*
062400021017     C* 2° invio FIVAB
062500010201     C                   reset                   dscmz
062600021017     C                   move      vlrpoi        cmzdst
062700010201     C                   eval      cmzfld = vlrfou
062800010201     C                   eval      cmzmbd = vlrhdl
062900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063000021009     C***                if        prmfir = *blanks
063100021017     C                   eval      cmzfla = 'FIVAB00F'
063200021017     C                   eval      cmzmba = 'FIVAB00F'
063300021009     C***                else
063400021009     C***                eval      cmzfla = prmfir
063500021009     C***                eval      cmzmba = prmfir
063600021009     C***                endif
063700010201     C                   eval      cmznrr = *zeros
063800010201     C                   move      §ctrokvb      cmznrr
063900021018     C                   eval      cmzlba = vlrfl1
064000010201     C                   call(e)   'TIS711C'
064100010201     C                   parm                    dscmz
064200010201     C                   parm      *blanks       esito
064300010201     C                   if        %error
064400010201     C                             or cmzerr = '1'
064500010201     C                             or esito  = '1'
064600010201     C                   eval      wrkesito = '3'
064700010201     C                   endif
064800010205     C                   endif
064900990920     C*
065000000613     C                   ENDSR
065100000613     C***
065200070411
065300070411     C     *pssr         BEGSR
065400070411     C*
065500070411     C                   if        %open(tivin00r)
065600070411     C                   close     tivin00r
065700070411     C                   endif
065800070411     C                   if        %open(fivabwwr)
065900070411     C                   close     fivabwwr
066000070411     C                   endif
066100070411     C                   if        %open(fivatwwr)
066200070411     C                   close     fivatwwr
066300070411     C                   endif
066400070411     C*
066500070411     C* Effettuo la chiamata al CLLE preposto
066600070411     C                   call(e)   'TITVVTC'
066700070411     C                   parm                    parccm
066800070411     C                   parm                    parmbr
066900070411     C                   parm      '2'           paropz
067000070411     C*
067100070411     C                   eval      wrkesito = '2'
067200070411     C*
067300070411     C                   seton                                        LR
067400070411     C*
067500070411     C                   ENDSR     '*CANCL'
067600070411     C***
067700070411
067800990910
067900000613     C     *inzsr        BEGSR
068000990910     C*
068100990910     C     *entry        plist
068200990920     C                   parm                    tivlrds
068300990921     C                   parm      wrkesito      esito
068400000724     C                   parm                    prmlit
068500000710     C                   parm                    prmfir
068600000613     C*
068700000830     C* CALCOLA LA DATA CORRENTE
068800000830     C                   time                    wn14             14 0
068900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069000000830     C                   z-add     wn8           g08dat
069100000830     C                   z-add     *zeros        g08inv
069200000830     C                   movel     '0'           g08err
069300000830     C                   call      'XSRDA8'
069400000830     C                   parm                    wlbda8
069500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069600000830     C*
069700000613     C                   ENDSR
069800000613     C***
069900990908
070000031117     Otitv74p   E            riepilogo         2
070100990915     O                                              'Upload via Internet'
070200990915     O                                           +1 'Traduzione TIVIN00R -'
070300021017     O                                           +1 'FIVABWWR/FIVATWWR'
070400010201     O                                           +1 'Report testate bolle'
070500990915     O          E            riepilogo   2
070600990915     O                       wrkdata
070700990915     O                       wrkora              +1
070800990915     O                       procname            +1
070900990915     O          E            riepilogo   2
071000990915     O                                              'Cliente..................:'
071100990915     O                       VABCCM        z     +1
071200990915     O          E            riepilogo   2
071300990920     O                                              'Riferimento Strategi.....:'
071400990920     O                       vlrhdl              +1
071500990915     O          E            riepilogo   2
071600990915     O                                              'Giusti...................:'
071700010201     O                       §CTROKVB      2   +  1
071800990915     O          E            riepilogo   2
071900990915     O                                              'Sbagliati e corretti.....:'
072000971022     O                       §CTRMO        2   +  1
072100990915     O          E            riepilogo   2
072200990915     O                                              'Sbagliati e scartati.....:'
072300971022     O                       §CTRNO        2   +  1
072400000613
072500031117     Otitv74ps  E            testdett          2
072600000613     O                                              'Upload via Internet'
072700000613     O                                           +1 'Traduzione TIVIN00R -'
072800021017     O                                           +1 'FIVABWWR/FIVATWWR'
072900010201     O                                           +1 'Report testate bolle'
073000000616     O          E            testdett    3
073100000613     O                                           +2 'N° rec'
073200000613     O                                           +3 'Anteprima contenuto'
073300000616     O          E            rigadett    2
073400000613     O                       rrnum               +2
073500000621     O                       recko               +3
073600000616     O          E            findett     2
073700000613     O                       wrkdata
073800000613     O                       wrkora              +1
073900000613     O                       procname            +1
074000000616     O          E            findett     2
074100000613     O                                              'Cliente..................:'
074200000613     O                       VABCCM        z     +1
074300000616     O          E            findett     2
074400000613     O                                              'Riferimento Strategi.....:'
074500000613     O                       vlrhdl              +1
074600000616     O          E            findett     2
074700000613     O                                              'Giusti...................:'
074800010201     O                       §CTROKVB      2   +  1
074900000616     O          E            findett     2
075000000613     O                                              'Sbagliati e corretti.....:'
075100000613     O                       §CTRMO        2   +  1
075200000616     O          E            findett     2
075300000613     O                                              'Sbagliati e scartati.....:'
075400000613     O                       §CTRNO        2   +  1
075500000512** CMD - COMANDI CL
075600031117OVRPRTF FILE(TITV74P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
075700031117OVRPRTF FILE(TITV74PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075800031117DLTOVR FILE(TITV74P TITV74PS) LVL(*)
075900000512
076000000512
