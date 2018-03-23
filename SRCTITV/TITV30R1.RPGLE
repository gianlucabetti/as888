000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030114     Ftitv30p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030114     Ftitv30ps  O    f  198        PRINTER usropn
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
003500030114     D depspe          s             10    INZ(*blanks)
003600010213     D depdat          s              8  0 INZ(*zeros)
003700030114     D curspe          s             10    INZ(*blanks)
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
009300030114     C                   if        not %open(titv30ps)
009400030114     C                   open      titv30ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700000613     C*
009800030114     C                   EXSR      RWHSE                                        LETT/SCR. VAB
009900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200050128     C                   call(e)   'TITVVTC'
010300010202     C                   parm                    parccm
010400010202     C                   parm                    parmbr
010500010202     C                   parm      '2'           paropz
010600010202     C*
010700030114     C                   if        %open(titv30ps)
010800000616     C                   except    findett
010900030114     C                   close     titv30ps
011000000613     C                   endif
011100000616     C*
011200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011300011113     C                   Z-ADD     150           LENGH            15 5
011400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011500000616     C                   PARM                    CMD(3)
011600000616     C                   PARM                    LENGH
011700000616     C*
011800000801     C
011900010201     C                   seton                                        LR
012000000613
012100000613
012200000613     C*--------------------------------------------------------
012300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012400000613     C*--------------------------------------------------------
012500000621     C     STPR          BEGSR
012600000613     C*
012700030114     C                   if        not %open(titv30p)
012800030114     C                   open      titv30p
012900990915     C                   endif
013000990915     C*
013100990915     C                   except    riepilogo
013200990915     C*
013300030114     C                   if        %open(titv30p)
013400030114     C                   close     titv30p
013500990914     C                   endif
013600990910     C*
013700000613     C                   ENDSR
013800000613     C***
013900990908
014000000801
014100910830     C*--------------------------------------------------------
014200030114     C* RWHSE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR     *
014300910830     C*--------------------------------------------------------
014400030114     C     RWHSE         BEGSR
014500990910     C*
014600990914     C                   if        not %open(tivin00r)
014700990908     C                   open      tivin00r
014800990914     C                   endif
014900021017     C                   if        not %open(fivabwwr)
015000021017     C                   open      fivabwwr
015100990914     C                   endif
015200021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015300020305     C                   exsr      prevat
015400010201     C*
015500010202     C                   if        chkcall = '0'
015600010202     C*
015700021017     C                   if        not %open(fivatwwr)
015800021017     C                   open      fivatwwr
015900010201     C                   endif
016000990910     C*
016100010201     C                   clear                   §CTROKVB          5 0
016200020305     C                   clear                   §CTROKVT          5 0
016300000801     C                   clear                   §CTRMO            5 0
016400000801     C                   clear                   §CTRNO            5 0
016500990910     C*
016600921023     C                   DO        *HIVAL
016700990913     C*
016800990915     C                   READ      tivin00r                               70
016900000905     C                   if        vindta > *blanks
017000000613     C                   add       1             rrnum
017100000801     C*
017200000801     C                   if        *in70 = *off
017300000801     C                             and
017400000801     C                             (vinflg = *blanks
017500000801     C                              or vinflg = '0'
017600000801     C                              or vinflg = '2')
017700000801     C*
017800000801     C                   clear                   vinmsg
017900010305     C*
018000010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018100030107     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
018200030114     C                   MOVEL(p)  PiStr         curspe
018300010305     C*
018400010305     C                   if        depspe = *blanks                             => 1° giro
018500010305     C                   eval      depspe = curspe                              => memorizz. spediz
018600020305     C                   exsr      impvab
018700020305     C                   exsr      wrivat                                       => carico VAT
018800010305     C                   else
018900010305     C                   if        curspe <> depspe                             => rottura di spediz
019000010305     C                   eval      depspe = curspe                              => memorizz. spediz
019100020305     C                   exsr      wrivab
019200020305     C                   exsr      impvab
019300020305     C                   exsr      wrivat                                       => carico VAT
019400020305     C                   else                                                   => x stessa spediz
019500020305     C                   exsr      impvab
019600020305     C                   exsr      wrivat                                       => carico VAT
019700010305     C                   endif
019800010305     C                   endif
019900010305     C                   endif
020000000905     C*
020100000905     C                   else
020200000905     C                   eval      vinflg = '1'
020300000905     C                   endif
020400000905     C*
020500000905     C  N70              update    tivin000
020600000905     C*
020700991022     C  N70              ENDdo
020800010202     C*
020900020305     C* Scarico i VAB rimasti "in sospeso"
021000020305     C                   exsr      wrivab
021100010202     C*
021200010202     C                   endif
021300990910
021400990910     C* Se non ci sono record con errori ...
021500000710     C                   if        §ctrno = 0
021600990910     C* ... restituisco esito OK.
021700990921     C                   eval      wrkesito = '0'
021800990910     C                   else
021900010201     C                   if        §ctrokvb > 0
022000990921     C                   eval      wrkesito = '1'
022100000710     C                   else
022200000710     C                   eval      wrkesito = '2'
022300990910     C                   endif
022400000710     C                   endif
022500990910     C*
022600990914     C                   if        %open(tivin00r)
022700990908     C                   close     tivin00r
022800990914     C                   endif
022900021017     C                   if        %open(fivabwwr)
023000021017     C                   close     fivabwwr
023100990914     C                   endif
023200021017     C                   if        %open(fivatwwr)
023300021017     C                   close     fivatwwr
023400010201     C                   endif
023500021017     C*
023600021017     C                   if        vlrpoi <> 999
023700021017     C                   eval      vlrpoi = vlrpoi
023800021017     C                   endif
023900990910     C*
024000010201     C                   if        §ctrokvb > 0
024100021017     C                             and vlrpoi > *zeros
024200010202     C                   exsr      invio
024300990920     C                   endif
024400990920     C*
024500910830     C                   ENDSR
024600000613     C***
024700010305
024800010305     C*----------------------------------------------------*
024900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025000010305     C*----------------------------------------------------*
025100020305     C     WRIVAB        BEGSR
025200010305     C*
025300021017     C                   write     fivab000                                     => scarico il VAB
025400010305     C*
025500010305     C                   ENDSR
025600990920
025700000801     C*----------------------------------------------------*
025800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025900000801     C*----------------------------------------------------*
026000010201     C     INZVAR        BEGSR
026100000801     C*
026200010201     C                   Z-ADD     *zeros        Num5_0
026300030107     C                   Z-ADD     *zeros        Num7_0
026400000801     C*
026500000801     C                   ENDSR
026600000801     C*----------------------------------------------------*
026700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026800000801     C*----------------------------------------------------*
026900000801     C     DEFCAM        BEGSR
027000000801     C*
027100021017     C                   CLEAR                   FIVAB000
027200020619     C* Imposto i valori di default...
027300070605     C                   Z-ADD     2551050       VABCCM
027400070605     C                   Z-ADD     2551050       VATCCM
027500021017     C                   Z-ADD     vlrpoi        VABFGS
027600021017     C                   Z-ADD     vlrpoi        VATFGS
027700070605     C                   Z-ADD     255           VABLNP
027800070605     C                   Z-ADD     255           VATLNP
027900070605     C                   Z-ADD     000           VABCTR
028000021017     C                   MOVEL     '7Q'          VABCTM
028100070605     C                   MOVEL     'C'           VABTSP
028200020305     C                   MOVEL     'E'           VATTRC
028300020619     C* ... e poi verifico se sono stati passati come parametri
028400020619     C                   IF        vlrppt > *blanks
028500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028600020619     C                   EXSR      CHKNUM
028700020619     C                   IF        PiInt=*on
028800020619     C                   Z-ADD     PiVal         VABCCM
028900020619     C                   Z-ADD     PiVal         VATCCM
029000020619     C                   ENDIF
029100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029200020619     C                   EXSR      CHKNUM
029300020619     C                   IF        PiInt=*on
029400020619     C                   Z-ADD     PiVal         VABLNP
029500020619     C                   Z-ADD     PiVal         VATLNP
029600020619     C                   ENDIF
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABCTR
030100020619     C                   ENDIF
030200020619     C                   ENDIF
030300000801     C*
030400000801     C                   ENDSR
030500000801     C*----------------------------------------------------*
030600021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030700000801     C*----------------------------------------------------*
030800010201     C     IMPVAB        BEGSR
030900000801     C*
031000020305     C                   EXSR      INZVAR
031100020305     C                   EXSR      DEFCAM
031200010305     C*
031300000801     C                   Z-ADD     *zeros        errore            1 0
031400000830     C                   MOVEL     datcor        VABAAS
031500020305     C                   MOVEL     datcor        VATAAS
031600000830     C                   MOVE      datcor        VABMGS
031700030107     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
031800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032000020117     C* ==
032100030107     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
032200030107     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
032300030107     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
032400030114     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
032500090514     C                   MOVEL     *blanks       wNOTE            80
032600090514     C                   EVAL      wNOTE =%trim(%subst(vindta:197:80))
032700090514     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
032800090514     C                   EVAL      VABNT2=%trim(%subst(wNOTE:1+35:35))
032900040405     C***                EVAL      VABRMO='Tel:' + %trim(%subst(vindta:92:15))
033000030114     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
033100030107     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
033200030114     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
033300030114     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
033400030107     C                   EVAL      VABTSP='E'
033500030107     C                   ENDIF
033600030114     C                   IF        %trim(%subst(vindta:16:1)) = '1'
033700030114     C                   EVAL      VABFFD='S'
033800030114     C                   ENDIF
033900100705     C                   EVAL      VABGMA=%trim(%subst(vindta:416:2))
034000100713     C*
034100100713     C                   IF        VABGMA = 'RC'
034200121109     C                   IF        VABCCM <> 2552551
034300100713     C                   EVAL      VABGMA = 'R6'
034400121109     C                   ENDIF
034500100713     C                   ENDIF
034600020305     C*
034700030107     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
034800010201     C                   EXSR      CHKNUM
034900010201     C                   IF        PiInt=*on
035000010201     C                   Z-ADD     PiVal         Num5_0
035100021017     C                   MOVEL     Num5_0        VABCAD
035200010201     C                   ELSE
035300010201     C                   ADD       1             errore
035400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035500021017     C                             + ' ' + 'VABCAD'
035600010201     C                   ENDIF
035700020619     C*
035800030107     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
035900010201     C                   EXSR      CHKNUM
036000010201     C                   IF        PiInt=*on
036100021017     C                   Z-ADD     PiVal         VABRMN
036200030107     C                   Z-ADD     PiVal         Num7_0
036300030107     C                   MOVE(P)   Num7_0        VABNSP
036400030107     C                   MOVE(P)   Num7_0        VATNSP
036500010201     C                   ELSE
036600010201     C                   ADD       1             errore
036700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036800021017     C                             + ' ' + 'VABNSP VATNSP VABRMN'
036900010201     C                   ENDIF
037000030107     C*
037100030107     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
037200030107     C                   EXSR      CHKNUM
037300030107     C                   IF        PiInt=*on
037400030107     C                   Z-ADD     PiVal         VABNCL
037500030107     C                   ELSE
037600030107     C                   ADD       1             errore
037700030107     C                   EVAL      vinmsg = %trimr(vinmsg)
037800030107     C                             + ' ' + 'VABNCL'
037900030107     C                   ENDIF
038000070504     C*
038100070504     C* Se numero colli > 1 => importo come Disk A poi occorre etichettare manualmente
038200070504     C                   IF        VABNCL > 1
038300070504     C                   MOVEL     *blanks       VABCTM
038400070504     C                   ENDIF
038500030107     C*
038600030107     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
038700030114     C                   EVAL      PiDecChr = '.'
038800030107     C                   EXSR      CHKNUM
038900030107     C                   IF        PiNum=*on
039000030114     C                   Z-ADD(H)  PiVal         VABPKB
039100030107     C                   ELSE
039200030107     C                   ADD       1             errore
039300030107     C                   EVAL      vinmsg = %trimr(vinmsg)
039400030107     C                             + ' ' + 'VABPKB'
039500030107     C                   ENDIF
039600030114     C*
039700030114     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
039800030114     C                   EVAL      PiDecChr = '.'
039900030114     C                   EXSR      CHKNUM
040000030114     C                   IF        PiNum=*on
040100151019     C***                EVAL      PiVal = PiVal / 1000                         * da DM3 a M3
040200030114     C                   Z-ADD(H)  PiVal         VABVLB
040300030114     C                   ELSE
040400030114     C                   ADD       1             errore
040500030114     C                   EVAL      vinmsg = %trimr(vinmsg)
040600030114     C                             + ' ' + 'VABVLB'
040700030114     C                   ENDIF
040800030107     C*
040900030107     C                   IF        %trim(%subst(vindta:280:12)) <> *blanks
041000050614     C                   IF        %trim(%subst(vindta:315:3)) = 'BSC'
041100050614     C                   EVAL      VABTIC=*blanks
041200050614     C                   ENDIF
041300050614     C                   IF        %trim(%subst(vindta:315:3)) = 'CSC'
041400050614     C                   EVAL      VABTIC='C'
041500050614     C                   ENDIF
041600050614     C                   IF        %trim(%subst(vindta:315:3)) = 'CON'
041700091116     C***                EVAL      VABTIC='SC'
041800091116     C                   EVAL      VABTIC=*blanks
041900050614     C                   ENDIF
042000070504     C                   IF        %trim(%subst(vindta:315:3)) = 'ACS'
042100050614     C                   EVAL      VABTIC='C'
042200050614     C                   ENDIF
042300121109     C*
042400121109     C* Forzatura x codice mittente 2552551
042500121109     C                   IF        VABCCM = 2552551
042600121109     C                   EVAL      VABTIC='SC'
042700121109     C                   ENDIF
042800121109     C*
042900030107     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
043000030114     C                   EVAL      PiDecChr = ','
043100030107     C                   EXSR      CHKNUM
043200030107     C                   IF        PiNum=*on
043300030107     C                   Z-ADD     PiVal         VABCAS
043400030107     C                   ELSE
043500030107     C                   ADD       1             errore
043600030107     C                   EVAL      vinmsg = %trimr(vinmsg)
043700030107     C                             + ' ' + 'VABCAS'
043800030107     C                   ENDIF
043900030107     C                   ENDIF
044000030114     C*
044100030114     C                   IF        %trim(%subst(vindta:295:12)) <> *blanks
044200030114     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
044300030114     C                   EVAL      PiDecChr = ','
044400030114     C                   EXSR      CHKNUM
044500030114     C                   IF        PiNum=*on
044600030114     C                   Z-ADD     PiVal         VABIAS
044700030114     C                   ELSE
044800030114     C                   ADD       1             errore
044900030114     C                   EVAL      vinmsg = %trimr(vinmsg)
045000030114     C                             + ' ' + 'VABIAS'
045100030114     C                   ENDIF
045200030114     C                   ENDIF
045300010205     C*
045400010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045500010205     C                   IF        VABCAS > *zeros
045600010205     C                   EVAL      VABCBO = '4'
045700010205     C                   ELSE
045800010205     C                   EVAL      VABCBO = '1'
045900010205     C                   ENDIF
046000020305     C*
046100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046200011113     C                   EXSR      CHKIMPDIV
046300010202     C*
046400000801     C* Ebbene...
046500000801     C                   ADD       1             §CTRMO
046600010201     C                   IF        errore <> *zeros
046700000801     C                   ADD       1             §CTRNO
046800000801     C                   EVAL      recko = vindta
046900000801     C                   EXCEPT    rigadett
047000000801     C                   EVAL      vinflg = '2'
047100000801     C                   ELSE
047200010201     C                   ADD       1             §CTROKVB
047300000801     C                   ENDIF
047400000801     C*
047500000801     C                   ENDSR
047600010201     C*----------------------------------------------------*
047700021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047800010201     C*----------------------------------------------------*
047900020305     C     WRIVAT        BEGSR
048000010201     C*
048100021017     C* Valorizzo l buffer di scrittura del FIVAT
048200070504     C*
048300070504     C* - barcode "chi sono" (solo x numero colli = 1)
048400070504     C                   if        VABNCL = 1
048500050614     C                   EVAL      VATNOT = %triml(%subst(vindta:331:15))
048600021017     C                   write     FIVAT000
048700070504     C                   endif
048800070504     C*
048900070504     C* - telefono destinatario
049000040224     C                   EVAL      VATTRC = 'B'
049100040224     C                   EVAL      VATNOT=%trim(%subst(vindta:92:15))
049200040224     C                   write     FIVAT000
049300010201     C*
049400010201     C                   ENDSR
049500010202     C*----------------------------------------------------*
049600021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049700010202     C*----------------------------------------------------*
049800020305     C     PREVAT        BEGSR
049900010202     C*
050000021017     C* Compongo il nome del membro da dare al FIVATWWR
050100010202     C                   eval      parmbr = vlrhdl
050200010202     C                   movel     'M'           parmbr
050300030114     C                   eval      parccm = '00632410'
050400010202     C                   eval      paropz = '1'
050500010202     C* Effettuo la chiamata al CLLE preposto
050600050128     C                   call(e)   'TITVVTC'
050700010202     C                   parm                    parccm
050800010202     C                   parm                    parmbr
050900010202     C                   parm                    paropz
051000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051100010202     C                   if        %error
051200010202     C                   movel     '1'           chkcall
051300010202     C                   else
051400010202     C                   movel     '0'           chkcall
051500010202     C                   endif
051600010202     C*
051700010202     C                   ENDSR
051800000801     C*----------------------------------------------------*
051900000801     C*  CONTROLLO NUMERICITA' CAMPI
052000000801     C*----------------------------------------------------*
052100000801     C     CHKNUM        BEGSR
052200000801     C*
052300030114     C                   IF        PiDecChr = *blanks
052400030114     C                   EVAL      PiDecChr = ','
052500030114     C                   ENDIF
052600030114     C*
052700000801     C                   call(e)   'ISNUMERIC'
052800000801     C                   PARM                    PiStr            30
052900030114     C                   PARM                    PiDecChr          1
053000000801     C                   PARM      *ZEROS        PiVal            30 9
053100000801     C                   PARM      '0'           PiInt             1
053200000801     C                   PARM      '0'           PiNum             1
053300000801     C                   IF        %error
053400000801     C                   EVAL      PiInt=*off
053500030107     C                   EVAL      PiNum=*off
053600000801     C                   ENDIF
053700000801     C*
053800000801     C                   ENDSR
053900000801     C***
054000000801
054100011113
054200011113     C*----------------------------------------------------*
054300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054400011113     C*----------------------------------------------------*
054500011113     C     CHKIMPDIV     BEGSR
054600011113     C*
054700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054800011113     C                   Z-ADD     *zeros        wrkDec            9 9
054900011113     C*
055000011113     C* Come prima cosa effettuo considerazioni sulla divisa
055100011113     C                   IF        vabIAS > *zeros
055200011113     C                   IF        vabVAS <> 'EUR'
055300011113     C                   EVAL      vabVAS =  'ITL'
055400011113     C                   ENDIF
055500011113     C                   ENDIF
055600011113     C*
055700011113     C                   IF        vabCAS > *zeros
055800011113     C                   IF        vabVCA <> 'EUR'
055900011113     C                   EVAL      vabVCA =  'ITL'
056000011113     C                   ENDIF
056100011113     C                   ENDIF
056200011113     C*
056300011113     C                   IF        vabVMD > *zeros
056400020305     C                   IF        vabVAD <> 'EUR'
056500011113     C                   EVAL      vabVAD =  'ITL'
056600011113     C                   ENDIF
056700011113     C                   ENDIF
056800011113     C*
056900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057000011113     C                   Z-ADD     vabIAS        wrkDec
057100011113     C                   IF        wrkDec > *zeros
057200011113     C                   IF        vabVAS = 'ITL'
057300011113     C                   EVAL      vabIAS = *zeros
057400011113     C                   ENDIF
057500011113     C                   ENDIF
057600011113     C*
057700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057800011113     C                   Z-ADD     vabCAS        wrkDec
057900011113     C                   IF        wrkDec > *zeros
058000011113     C                   IF        vabVCA = 'ITL'
058100011113     C                   EVAL      vabCAS = *zeros
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058600011113     C                   Z-ADD     vabVMD        wrkDec
058700011113     C                   IF        wrkDec > *zeros
058800011113     C                   IF        vabVAD = 'ITL'
058900011113     C                   EVAL      vabVMD = *zeros
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C                   ENDSR
059400011113     C***
059500021017
059600000801
059700000801
059800990920      /TITLE Invio dei dati al punto operativo.
059900010202     C     invio         BEGSR
060000990920     C*
060100021017     C* 1° invio FIVAT
060200010201     C                   reset                   dscmz
060300021017     C                   move      vlrpoi        cmzdst
060400021017     C                   eval      cmzfld = 'FIVATWWR'
060500010201     C                   eval      cmzmbd = vlrhdl
060600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060700021009     C***                if        prmfir = *blanks
060800021017     C                   eval      cmzfla = 'FIVAT00F'
060900021017     C                   eval      cmzmba = 'FIVAT00F'
061000021009     C***                else
061100021009     C***                eval      cmzfla = prmfir
061200021009     C***                eval      cmzmba = prmfir
061300021009     C***                endif
061400010201     C                   eval      cmznrr = *zeros
061500020305     C                   move      §ctrokvt      cmznrr
061600021018     C                   eval      cmzlba = vlrfl1
061700010201     C                   call(e)   'TIS711C'
061800010201     C                   parm                    dscmz
061900010201     C                   parm      *blanks       esito
062000010205     C                   if        %error
062100010205     C                             or cmzerr = '1'
062200010205     C                             or esito  = '1'
062300010205     C                   eval      wrkesito = '3'
062400010205     C                   else
062500010201     C*
062600021017     C* 2° invio FIVAB
062700010201     C                   reset                   dscmz
062800021017     C                   move      vlrpoi        cmzdst
062900010201     C                   eval      cmzfld = vlrfou
063000010201     C                   eval      cmzmbd = vlrhdl
063100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063200021009     C***                if        prmfir = *blanks
063300021017     C                   eval      cmzfla = 'FIVAB00F'
063400021017     C                   eval      cmzmba = 'FIVAB00F'
063500021009     C***                else
063600021009     C***                eval      cmzfla = prmfir
063700021009     C***                eval      cmzmba = prmfir
063800021009     C***                endif
063900010201     C                   eval      cmznrr = *zeros
064000010201     C                   move      §ctrokvb      cmznrr
064100021018     C                   eval      cmzlba = vlrfl1
064200010201     C                   call(e)   'TIS711C'
064300010201     C                   parm                    dscmz
064400010201     C                   parm      *blanks       esito
064500010201     C                   if        %error
064600010201     C                             or cmzerr = '1'
064700010201     C                             or esito  = '1'
064800010201     C                   eval      wrkesito = '3'
064900010201     C                   endif
065000010205     C                   endif
065100990920     C*
065200000613     C                   ENDSR
065300000613     C***
065400070411
065500070411     C     *pssr         BEGSR
065600070411     C*
065700070411     C                   if        %open(tivin00r)
065800070411     C                   close     tivin00r
065900070411     C                   endif
066000070411     C                   if        %open(fivabwwr)
066100070411     C                   close     fivabwwr
066200070411     C                   endif
066300070411     C                   if        %open(fivatwwr)
066400070411     C                   close     fivatwwr
066500070411     C                   endif
066600070411     C*
066700070411     C* Effettuo la chiamata al CLLE preposto
066800070411     C                   call(e)   'TITVVTC'
066900070411     C                   parm                    parccm
067000070411     C                   parm                    parmbr
067100070411     C                   parm      '2'           paropz
067200070411     C*
067300070411     C                   eval      wrkesito = '2'
067400070411     C*
067500070411     C                   seton                                        LR
067600070411     C*
067700070411     C                   ENDSR     '*CANCL'
067800070411     C***
067900070411
068000990910
068100000613     C     *inzsr        BEGSR
068200990910     C*
068300990910     C     *entry        plist
068400990920     C                   parm                    tivlrds
068500990921     C                   parm      wrkesito      esito
068600000724     C                   parm                    prmlit
068700000710     C                   parm                    prmfir
068800000613     C*
068900000830     C* CALCOLA LA DATA CORRENTE
069000000830     C                   time                    wn14             14 0
069100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069200000830     C                   z-add     wn8           g08dat
069300000830     C                   z-add     *zeros        g08inv
069400000830     C                   movel     '0'           g08err
069500000830     C                   call      'XSRDA8'
069600000830     C                   parm                    wlbda8
069700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069800000830     C*
069900000613     C                   ENDSR
070000000613     C***
070100990908
070200030114     Otitv30p   E            riepilogo         2
070300990915     O                                              'Upload via Internet'
070400990915     O                                           +1 'Traduzione TIVIN00R -'
070500021017     O                                           +1 'FIVABWWR/FIVATWWR'
070600010201     O                                           +1 'Report testate bolle'
070700990915     O          E            riepilogo   2
070800990915     O                       wrkdata
070900990915     O                       wrkora              +1
071000990915     O                       procname            +1
071100990915     O          E            riepilogo   2
071200990915     O                                              'Cliente..................:'
071300990915     O                       VABCCM        z     +1
071400990915     O          E            riepilogo   2
071500990920     O                                              'Riferimento Strategi.....:'
071600990920     O                       vlrhdl              +1
071700990915     O          E            riepilogo   2
071800990915     O                                              'Giusti...................:'
071900010201     O                       §CTROKVB      2   +  1
072000990915     O          E            riepilogo   2
072100990915     O                                              'Sbagliati e corretti.....:'
072200971022     O                       §CTRMO        2   +  1
072300990915     O          E            riepilogo   2
072400990915     O                                              'Sbagliati e scartati.....:'
072500971022     O                       §CTRNO        2   +  1
072600000613
072700030114     Otitv30ps  E            testdett          2
072800000613     O                                              'Upload via Internet'
072900000613     O                                           +1 'Traduzione TIVIN00R -'
073000021017     O                                           +1 'FIVABWWR/FIVATWWR'
073100010201     O                                           +1 'Report testate bolle'
073200000616     O          E            testdett    3
073300000613     O                                           +2 'N° rec'
073400000613     O                                           +3 'Anteprima contenuto'
073500000616     O          E            rigadett    2
073600000613     O                       rrnum               +2
073700000621     O                       recko               +3
073800000616     O          E            findett     2
073900000613     O                       wrkdata
074000000613     O                       wrkora              +1
074100000613     O                       procname            +1
074200000616     O          E            findett     2
074300000613     O                                              'Cliente..................:'
074400000613     O                       VABCCM        z     +1
074500000616     O          E            findett     2
074600000613     O                                              'Riferimento Strategi.....:'
074700000613     O                       vlrhdl              +1
074800000616     O          E            findett     2
074900000613     O                                              'Giusti...................:'
075000010201     O                       §CTROKVB      2   +  1
075100000616     O          E            findett     2
075200000613     O                                              'Sbagliati e corretti.....:'
075300000613     O                       §CTRMO        2   +  1
075400000616     O          E            findett     2
075500000613     O                                              'Sbagliati e scartati.....:'
075600000613     O                       §CTRNO        2   +  1
075700000512** CMD - COMANDI CL
075800030114OVRPRTF FILE(TITV30P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
075900030114OVRPRTF FILE(TITV30PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076000030114DLTOVR FILE(TITV30P TITV30PS) LVL(*)
076100000512
076200000512
