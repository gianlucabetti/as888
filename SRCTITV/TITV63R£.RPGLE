000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030804     Ftitv63p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030804     Ftitv63ps  O    f  198        PRINTER usropn
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
002600030804     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600030415     D depspe          s              7    INZ(*blanks)
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
005300070103     D*------------------
005400070103     D* DS REPERIMENTO NUMERATORE
005500070103     D*------------------
005600070103     D trul33ds      e ds                  inz
005700070103     D*------------------
005800070103     D* DS ARCHITETTURA
005900070103     D*------------------
006000070103     D kpjba         e ds                  inz
006100000830     D*
006200990908
006300010201
006400010201
006500990915     C                   time                    wrkdata
006600990915     C                   time                    wrkora
006700000913     C                   reset                   rrnum
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000000724     C*
007100000724     C* SE OCCORRE SPEDIRE IN FILIALE
007200021017     C                   if        vlrpoi <> *zeros
007300000724     C*
007400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007600021017     C     vlrpoi        chain     azorg01l
007700000724     C                   if        %found
007800000616     C                   movel(p)  CMD(1)        depcmd
007900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008000000616     C*
008100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008200011113     C                   Z-ADD     150           LENGH            15 5
008300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400000616     C                   PARM                    depcmd
008500000616     C                   PARM                    LENGH
008600000724     C*
008700000724     C                   endif
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000616     C*
010000030804     C                   if        not %open(titv63ps)
010100030804     C                   open      titv63ps
010200000616     C                   except    testdett
010300000613     C                   endif
010400000613     C*
010500030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010700000613     C*
010800010202     C* Effettuo la chiamata al CLLE preposto
010900030730     C                   call(e)   'TITVVTC'
011000010202     C                   parm                    parccm
011100010202     C                   parm                    parmbr
011200010202     C                   parm      '2'           paropz
011300050201     C*
011400050201     C* Effettuo lancio TISI95 solo x chiusura
011500050201     C                   CLEAR                   TISI95DS
011600050201     C                   EVAL      I95TLA = 'C'
011700050201     C                   CALL      'TISI95R'
011800050201     C                   PARM                    TISI95DS
011900010202     C*
012000030804     C                   if        %open(titv63ps)
012100000616     C                   except    findett
012200030804     C                   close     titv63ps
012300000613     C                   endif
012400000616     C*
012500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012600011113     C                   Z-ADD     150           LENGH            15 5
012700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    CMD(3)
012900000616     C                   PARM                    LENGH
013000000616     C*
013100010201     C                   seton                                        LR
013200000613
013300000613
013400000613     C*--------------------------------------------------------
013500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013600000613     C*--------------------------------------------------------
013700000621     C     STPR          BEGSR
013800000613     C*
013900030804     C                   if        not %open(titv63p)
014000030804     C                   open      titv63p
014100990915     C                   endif
014200990915     C*
014300990915     C                   except    riepilogo
014400990915     C*
014500030804     C                   if        %open(titv63p)
014600030804     C                   close     titv63p
014700990914     C                   endif
014800990910     C*
014900000613     C                   ENDSR
015000000613     C***
015100990908
015200000801
015300910830     C*--------------------------------------------------------
015400030730     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
015500910830     C*--------------------------------------------------------
015600030226     C     RWFILE        BEGSR
015700990910     C*
015800990914     C                   if        not %open(tivin00r)
015900990908     C                   open      tivin00r
016000990914     C                   endif
016100021017     C                   if        not %open(fivabwwr)
016200021017     C                   open      fivabwwr
016300990914     C                   endif
016400021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016500020305     C                   exsr      prevat
016600010201     C*
016700010202     C                   if        chkcall = '0'
016800010202     C*
016900021017     C                   if        not %open(fivatwwr)
017000021017     C                   open      fivatwwr
017100010201     C                   endif
017200990910     C*
017300010201     C                   clear                   §CTROKVB          5 0
017400020305     C                   clear                   §CTROKVT          5 0
017500000801     C                   clear                   §CTRMO            5 0
017600000801     C                   clear                   §CTRNO            5 0
017700990910     C*
017800921023     C                   DO        *HIVAL
017900990913     C*
018000990915     C                   READ      tivin00r                               70
018100000905     C                   if        vindta > *blanks
018200000613     C                   add       1             rrnum
018300000801     C*
018400000801     C                   if        *in70 = *off
018500000801     C                             and
018600000801     C                             (vinflg = *blanks
018700000801     C                              or vinflg = '0'
018800000801     C                              or vinflg = '2')
018900000801     C*
019000000801     C                   clear                   vinmsg
019100030310     C                   eval      vinflg = '1'
019200030310     C*
019300030506     C* Determino se il record corrente è di tipo testata o di tipo dettaglio
019400030804     C                   if        %subst(vindta:1:2) = '11'                    *** TESTATA ***
019500030804     C                   clear                   fivab000
019600030804     C                   exsr      impvab                                       => carico  VAB
019700030804     C                   endif
019800030804     C                   if        %subst(vindta:1:2) = '12'                    *** TESTATA ***
019900030804     C                   exsr      impvab                                       => carico  VAB
020000030804     C                   endif
020100030804     C                   if        %subst(vindta:1:2) = '13'
020200030506     C                   exsr      impvab                                       => carico  VAB
020300030804     C                   exsr      wrivab                                       => scarico VAB
020400030506     C                   endif
020500030804     C                   if        %subst(vindta:1:2) = '14'                    *** DETTAGLIO ***
020600030506     C                   exsr      impvat                                       => carico  VAT
020700030506     C                   exsr      wrivat                                       => scarico VAT
020800030506     C                   endif
020900000905     C*
021000000905     C                   else
021100000905     C                   eval      vinflg = '1'
021200000905     C                   endif
021300030411     C                   endif
021400000905     C*
021500000905     C  N70              update    tivin000
021600000905     C*
021700991022     C  N70              ENDdo
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
026200030804     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026300030804     C                   MOVEL     '0'           FlgCAS            1
026400000801     C*
026500000801     C                   ENDSR
026600000801     C*----------------------------------------------------*
026700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026800000801     C*----------------------------------------------------*
026900000801     C     DEFCAM        BEGSR
027000000801     C*
027100020619     C* Imposto i valori di default...
027200120221     C***                Z-ADD     0013179       VABCCM
027300120221     C***                Z-ADD     0013179       VATCCM
027400120221     C                   Z-ADD     0015896       VABCCM
027500120221     C                   Z-ADD     0015896       VATCCM
027600021017     C                   Z-ADD     vlrpoi        VABFGS
027700021017     C                   Z-ADD     vlrpoi        VATFGS
027800030804     C                   Z-ADD     001           VABLNP
027900030804     C                   Z-ADD     001           VATLNP
028000070312     C                   Z-ADD     300           VABCTR
028100021017     C                   MOVEL     '7Q'          VABCTM
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
030600030730     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030700000801     C*----------------------------------------------------*
030800010201     C     IMPVAB        BEGSR
030900030730     C*
031000030804     C* Elaboro il primo tipo record d TESTATA
031100030804     C                   IF        %subst(vindta:1:2) = '11'
031200030804     C*
031300020305     C                   EXSR      INZVAR
031400020305     C                   EXSR      DEFCAM
031500010305     C*
031600030226     C                   Z-ADD     *zeros        errore            1 0
031700000830     C                   MOVEL     datcor        VABAAS
031800000830     C                   MOVE      datcor        VABMGS
031900030226     C*
032000030804     C                   EVAL      VABRSD=%trim(%subst(vindta:28:30))
032100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032300020117     C* ==
032400030804     C                   EVAL      VABRD2=%trim(%subst(vindta:58:30))
032500030804     C                   ENDIF
032600030804     C*
032700030804     C* Elaboro il secondo tipo record d TESTATA
032800030804     C                   IF        %subst(vindta:1:2) = '12'
032900030804     C*
033000030804     C                   EVAL      VABIND=%trim(%subst(vindta:57:30))
033100030804     C                   EVAL      VABLOD=%trim(%subst(vindta:100:25))
033200030804     C* CAP
033300030804     C                   EVAL      PiStr=%trim(%subst(vindta:95:5))
033400030804     C                   EXSR      CHKNUM
033500030804     C                   IF        PiInt=*on
033600030804     C                   Z-ADD     PiVal         Num5_0
033700030804     C                   MOVEL     Num5_0        VABCAD
033800030804     C                   ELSE
033900030804     C                   ADD       1             errore
034000030804     C                   EVAL      vinmsg = %trimr(vinmsg)
034100030804     C                             + ' ' + 'VABCAD'
034200030804     C                   ENDIF
034300030804     C* Reperisco la provincia dal CAP e dalla località
034400030804     C                   IF        VABCAD <> *blanks
034500030804     C                   CLEAR                   TISI95DS
034600030804     C                   EVAL      I95TCN = '3'
034700030804     C                   Z-ADD     datcor        I95DAT
034800030804     C                   EVAL      I95CAP = VABCAD
034900030804     C                   EVAL      I95LOC = VABLOD
035000030804     C                   CALL      'TISI95R'
035100030804     C                   PARM                    TISI95DS
035200030804     C                   EVAL      VABPRD = O95PRV
035300030804     C                   ENDIF
035400030804     C                   ENDIF
035500030804     C*
035600030804     C* Elaboro il terzo tipo record d TESTATA
035700030804     C                   IF        %subst(vindta:1:2) = '13'
035800030804     C* NCL
035900030804     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))
036000030804     C                   EXSR      CHKNUM
036100030804     C                   IF        PiInt=*on
036200030804     C                   Z-ADD     PiVal         VABNCL
036300030804     C                   ELSE
036400030804     C                   ADD       1             errore
036500030804     C                   EVAL      vinmsg = %trimr(vinmsg)
036600030804     C                             + ' ' + 'VABNCL'
036700030804     C                   ENDIF
036800030804     C* PKB
036900030804     C                   EVAL      PiStr=%trim(%subst(vindta:34:7))
037000030804     C                   EXSR      CHKNUM
037100030804     C                   IF        PiNum=*on
037200030804     C                   EVAL(H)   VABPKB = PiVal/100
037300030804     C                   ELSE
037400030804     C                   ADD       1             errore
037500030804     C                   EVAL      vinmsg = %trimr(vinmsg)
037600030804     C                             + ' ' + 'VABPKB'
037700030804     C                   ENDIF
037800030804     C* CAS
037900030804     C                   IF        %subst(vindta:57:9) <> *blanks AND
038000030804     C                             %subst(vindta:57:9) <> *zeros
038100030804     C                   MOVEL     '1'           FlgCAS
038200030804     C                   EVAL      PiStr=%trim(%subst(vindta:57:9))
038300030804     C                   EXSR      CHKNUM
038400030804     C                   IF        PiNum=*on
038500030804     C                   EVAL      VABCAS = PiVal/100
038600090312     C                   EVAL      VABTIC='BM'
038700030804     C                   EVAL      VABVCA='EUR'
038800030804     C                   ELSE
038900030804     C                   ADD       1             errore
039000030804     C                   EVAL      vinmsg = %trimr(vinmsg)
039100030804     C                             + ' ' + 'VABCAS'
039200030804     C                   ENDIF
039300030804     C                   ENDIF
039400070103     C* RMA/RMN
039500051213     C                   EVAL      VABRMA=%trim(%subst(vindta:71:15))
039600051213     C                   EVAL      PiStr=%trim(%subst(vindta:71:15))
039700010201     C                   EXSR      CHKNUM
039800010201     C                   IF        PiInt=*on
039900030804     C                   Z-ADD     PiVal         VABRMN
040000010201     C                   ELSE
040100010201     C                   ADD       1             errore
040200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040300070103     C                             + ' ' + 'VABRMA VABNSP'
040400010201     C                   ENDIF
040500070103     C* NSP => Stacco un numeratore da AZNUM
040600070103     C                   clear                   TRUL33DS
040700070103     C                   eval      I33OPE = *zeros
040800070103     C                   eval      I33CNU = 302
040900070103     C                   eval      I33NUM = 1
041000070103     C                   movel     TRUL33DS      KPJBU
041100070103     C                   call      'TRUL33R'
041200070103     C                   parm                    KPJBA
041300070103     C                   movel     KPJBU         TRUL33DS
041400070103     C                   if        O33ERR = *zeros
041500070103     C                   z-add     O33NRF        VABNSP
041600070103     C                   z-add     O33NRF        VATNSP
041700070103     C                   else
041800070103     C                   Z-ADD     1             errore
041900070103     C                   EVAL      vinmsg = %trimr(vinmsg)
042000070103     C                             + ' ' + 'VABNSP VATNSP'
042100070103     C                   endif
042200030804     C*
042300030804     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042400030804     C                   IF        FlgCAS = '1'
042500030804     C                   EVAL      VABCBO = '4'
042600030804     C                   ELSE
042700030804     C                   EVAL      VABCBO = '1'
042800030804     C                   ENDIF
042900020305     C*
043000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043100011113     C                   EXSR      CHKIMPDIV
043200010202     C*
043300000801     C* Ebbene...
043400000801     C                   ADD       1             §CTRMO
043500010201     C                   IF        errore <> *zeros
043600000801     C                   ADD       1             §CTRNO
043700000801     C                   EVAL      recko = vindta
043800000801     C                   EXCEPT    rigadett
043900000801     C                   EVAL      vinflg = '2'
044000000801     C                   ELSE
044100010201     C                   ADD       1             §CTROKVB
044200000801     C                   ENDIF
044300030804     C                   ENDIF
044400000801     C*
044500000801     C                   ENDSR
044600030226     C*----------------------------------------------------*
044700030226     C*  SCARICAMENTO BUFFER RECORDS VAB
044800030226     C*----------------------------------------------------*
044900030226     C     WRIVAB        BEGSR
045000030310     C*
045100030310     C                   write     FIVAB000
045200030226     C*
045300030226     C                   ENDSR
045400030226     C*----------------------------------------------------*
045500030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
045600030226     C*----------------------------------------------------*
045700030226     C     IMPVAT        BEGSR
045800030226     C*
045900030226     C                   MOVEL     datcor        VATAAS
046000030226     C*
046100030415     C* Valorizzo il barcode
046200030804     C                   EVAL      VATNOT=%trim(%subst(vindta:29:24))
046300030226     C*
046400030226     C                   ENDSR
046500010201     C*----------------------------------------------------*
046600021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046700010201     C*----------------------------------------------------*
046800020305     C     WRIVAT        BEGSR
046900010201     C*
047000021017     C                   write     FIVAT000
047100010201     C*
047200010201     C                   ENDSR
047300010202     C*----------------------------------------------------*
047400021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047500010202     C*----------------------------------------------------*
047600020305     C     PREVAT        BEGSR
047700010202     C*
047800021017     C* Compongo il nome del membro da dare al FIVATWWR
047900010202     C                   eval      parmbr = vlrhdl
048000010202     C                   movel     'M'           parmbr
048100070103     C                   eval      parccm = '00013179'
048200010202     C                   eval      paropz = '1'
048300010202     C* Effettuo la chiamata al CLLE preposto
048400030730     C                   call(e)   'TITVVTC'
048500010202     C                   parm                    parccm
048600010202     C                   parm                    parmbr
048700010202     C                   parm                    paropz
048800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048900010202     C                   if        %error
049000010202     C                   movel     '1'           chkcall
049100010202     C                   else
049200010202     C                   movel     '0'           chkcall
049300010202     C                   endif
049400010202     C*
049500010202     C                   ENDSR
049600000801     C*----------------------------------------------------*
049700000801     C*  CONTROLLO NUMERICITA' CAMPI
049800000801     C*----------------------------------------------------*
049900000801     C     CHKNUM        BEGSR
050000000801     C*
050100000801     C                   call(e)   'ISNUMERIC'
050200000801     C                   PARM                    PiStr            30
050300030430     C                   PARM      '.'           PiDecChr          1
050400000801     C                   PARM      *ZEROS        PiVal            30 9
050500000801     C                   PARM      '0'           PiInt             1
050600000801     C                   PARM      '0'           PiNum             1
050700000801     C                   IF        %error
050800000801     C                   EVAL      PiInt=*off
050900030107     C                   EVAL      PiNum=*off
051000000801     C                   ENDIF
051100000801     C*
051200000801     C                   ENDSR
051300000801     C***
051400000801
051500011113
051600011113     C*----------------------------------------------------*
051700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051800011113     C*----------------------------------------------------*
051900011113     C     CHKIMPDIV     BEGSR
052000011113     C*
052100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052200011113     C                   Z-ADD     *zeros        wrkDec            9 9
052300011113     C*
052400011113     C* Come prima cosa effettuo considerazioni sulla divisa
052500011113     C                   IF        vabIAS > *zeros
052600011113     C                   IF        vabVAS <> 'EUR'
052700011113     C                   EVAL      vabVAS =  'ITL'
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C                   IF        vabCAS > *zeros
053200011113     C                   IF        vabVCA <> 'EUR'
053300011113     C                   EVAL      vabVCA =  'ITL'
053400011113     C                   ENDIF
053500011113     C                   ENDIF
053600011113     C*
053700011113     C                   IF        vabVMD > *zeros
053800020305     C                   IF        vabVAD <> 'EUR'
053900011113     C                   EVAL      vabVAD =  'ITL'
054000011113     C                   ENDIF
054100011113     C                   ENDIF
054200011113     C*
054300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054400011113     C                   Z-ADD     vabIAS        wrkDec
054500011113     C                   IF        wrkDec > *zeros
054600011113     C                   IF        vabVAS = 'ITL'
054700011113     C                   EVAL      vabIAS = *zeros
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055200011113     C                   Z-ADD     vabCAS        wrkDec
055300011113     C                   IF        wrkDec > *zeros
055400011113     C                   IF        vabVCA = 'ITL'
055500011113     C                   EVAL      vabCAS = *zeros
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056000011113     C                   Z-ADD     vabVMD        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVAD = 'ITL'
056300011113     C                   EVAL      vabVMD = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C                   ENDSR
056800011113     C***
056900021017
057000000801
057100000801
057200990920      /TITLE Invio dei dati al punto operativo.
057300010202     C     invio         BEGSR
057400990920     C*
057500021017     C* 1° invio FIVAT
057600010201     C                   reset                   dscmz
057700021017     C                   move      vlrpoi        cmzdst
057800021017     C                   eval      cmzfld = 'FIVATWWR'
057900010201     C                   eval      cmzmbd = vlrhdl
058000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058100021009     C***                if        prmfir = *blanks
058200021017     C                   eval      cmzfla = 'FIVAT00F'
058300021017     C                   eval      cmzmba = 'FIVAT00F'
058400021009     C***                else
058500021009     C***                eval      cmzfla = prmfir
058600021009     C***                eval      cmzmba = prmfir
058700021009     C***                endif
058800010201     C                   eval      cmznrr = *zeros
058900020305     C                   move      §ctrokvt      cmznrr
059000021018     C                   eval      cmzlba = vlrfl1
059100010201     C                   call(e)   'TIS711C'
059200010201     C                   parm                    dscmz
059300010201     C                   parm      *blanks       esito
059400010205     C                   if        %error
059500010205     C                             or cmzerr = '1'
059600010205     C                             or esito  = '1'
059700010205     C                   eval      wrkesito = '3'
059800010205     C                   else
059900010201     C*
060000021017     C* 2° invio FIVAB
060100010201     C                   reset                   dscmz
060200021017     C                   move      vlrpoi        cmzdst
060300010201     C                   eval      cmzfld = vlrfou
060400010201     C                   eval      cmzmbd = vlrhdl
060500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060600021009     C***                if        prmfir = *blanks
060700021017     C                   eval      cmzfla = 'FIVAB00F'
060800021017     C                   eval      cmzmba = 'FIVAB00F'
060900021009     C***                else
061000021009     C***                eval      cmzfla = prmfir
061100021009     C***                eval      cmzmba = prmfir
061200021009     C***                endif
061300010201     C                   eval      cmznrr = *zeros
061400010201     C                   move      §ctrokvb      cmznrr
061500021018     C                   eval      cmzlba = vlrfl1
061600010201     C                   call(e)   'TIS711C'
061700010201     C                   parm                    dscmz
061800010201     C                   parm      *blanks       esito
061900010201     C                   if        %error
062000010201     C                             or cmzerr = '1'
062100010201     C                             or esito  = '1'
062200010201     C                   eval      wrkesito = '3'
062300010201     C                   endif
062400010205     C                   endif
062500990920     C*
062600000613     C                   ENDSR
062700000613     C***
062800070411
062900070411     C     *pssr         BEGSR
063000070411     C*
063100070411     C                   if        %open(tivin00r)
063200070411     C                   close     tivin00r
063300070411     C                   endif
063400070411     C                   if        %open(fivabwwr)
063500070411     C                   close     fivabwwr
063600070411     C                   endif
063700070411     C                   if        %open(fivatwwr)
063800070411     C                   close     fivatwwr
063900070411     C                   endif
064000070411     C*
064100070411     C* Effettuo la chiamata al CLLE preposto
064200070411     C                   call(e)   'TITVVTC'
064300070411     C                   parm                    parccm
064400070411     C                   parm                    parmbr
064500070411     C                   parm      '2'           paropz
064600070411     C*
064700070411     C                   eval      wrkesito = '2'
064800070411     C*
064900070411     C                   seton                                        LR
065000070411     C*
065100070411     C                   ENDSR     '*CANCL'
065200070411     C***
065300070411
065400990910
065500000613     C     *inzsr        BEGSR
065600990910     C*
065700990910     C     *entry        plist
065800990920     C                   parm                    tivlrds
065900990921     C                   parm      wrkesito      esito
066000000724     C                   parm                    prmlit
066100000710     C                   parm                    prmfir
066200000613     C*
066300000830     C* CALCOLA LA DATA CORRENTE
066400000830     C                   time                    wn14             14 0
066500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066600000830     C                   z-add     wn8           g08dat
066700000830     C                   z-add     *zeros        g08inv
066800000830     C                   movel     '0'           g08err
066900000830     C                   call      'XSRDA8'
067000000830     C                   parm                    wlbda8
067100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067200000830     C*
067300000613     C                   ENDSR
067400000613     C***
067500990908
067600030804     Otitv63p   E            riepilogo         2
067700990915     O                                              'Upload via Internet'
067800990915     O                                           +1 'Traduzione TIVIN00R -'
067900021017     O                                           +1 'FIVABWWR/FIVATWWR'
068000010201     O                                           +1 'Report testate bolle'
068100990915     O          E            riepilogo   2
068200990915     O                       wrkdata
068300990915     O                       wrkora              +1
068400990915     O                       procname            +1
068500990915     O          E            riepilogo   2
068600990915     O                                              'Cliente..................:'
068700990915     O                       VABCCM        z     +1
068800990915     O          E            riepilogo   2
068900990920     O                                              'Riferimento Strategi.....:'
069000990920     O                       vlrhdl              +1
069100990915     O          E            riepilogo   2
069200990915     O                                              'Giusti...................:'
069300010201     O                       §CTROKVB      2   +  1
069400990915     O          E            riepilogo   2
069500990915     O                                              'Sbagliati e corretti.....:'
069600971022     O                       §CTRMO        2   +  1
069700990915     O          E            riepilogo   2
069800990915     O                                              'Sbagliati e scartati.....:'
069900971022     O                       §CTRNO        2   +  1
070000000613
070100030804     Otitv63ps  E            testdett          2
070200000613     O                                              'Upload via Internet'
070300000613     O                                           +1 'Traduzione TIVIN00R -'
070400021017     O                                           +1 'FIVABWWR/FIVATWWR'
070500010201     O                                           +1 'Report testate bolle'
070600000616     O          E            testdett    3
070700000613     O                                           +2 'N° rec'
070800000613     O                                           +3 'Anteprima contenuto'
070900000616     O          E            rigadett    2
071000000613     O                       rrnum               +2
071100000621     O                       recko               +3
071200000616     O          E            findett     2
071300000613     O                       wrkdata
071400000613     O                       wrkora              +1
071500000613     O                       procname            +1
071600000616     O          E            findett     2
071700000613     O                                              'Cliente..................:'
071800000613     O                       VABCCM        z     +1
071900000616     O          E            findett     2
072000000613     O                                              'Riferimento Strategi.....:'
072100000613     O                       vlrhdl              +1
072200000616     O          E            findett     2
072300000613     O                                              'Giusti...................:'
072400010201     O                       §CTROKVB      2   +  1
072500000616     O          E            findett     2
072600000613     O                                              'Sbagliati e corretti.....:'
072700000613     O                       §CTRMO        2   +  1
072800000616     O          E            findett     2
072900000613     O                                              'Sbagliati e scartati.....:'
073000000613     O                       §CTRNO        2   +  1
073100000512** CMD - COMANDI CL
073200030804OVRPRTF FILE(TITV63P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
073300030804OVRPRTF FILE(TITV63PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
073400030804DLTOVR FILE(TITV63P TITV63PS) LVL(*)
073500000512
073600000512
