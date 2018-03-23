000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030506     Ftitv52p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030506     Ftitv52ps  O    f  198        PRINTER usropn
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
009100030506     C                   if        not %open(titv52ps)
009200030506     C                   open      titv52ps
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
010500030506     C                   if        %open(titv52ps)
010600000616     C                   except    findett
010700030506     C                   close     titv52ps
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
012400030506     C                   if        not %open(titv52p)
012500030506     C                   open      titv52p
012600990915     C                   endif
012700990915     C*
012800990915     C                   except    riepilogo
012900990915     C*
013000030506     C                   if        %open(titv52p)
013100030506     C                   close     titv52p
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
017800030506     C* Determino se il record corrente è di tipo testata o di tipo dettaglio
017900030506     C                   if        %subst(vindta:1:3) = 'EBE'                   *** TESTATA ***
018000030506     C                   clear                   fivab000
018100030506     C                   clear                   fivat000
018200030506     C                   exsr      impvab                                       => carico  VAB
018300030506     C                   exsr      wrivab                                       => scarico VAB
018400030506     C                   endif
018500030506     C                   if        %subst(vindta:1:3) = 'EBC'                   *** DETTAGLIO ***
018600030506     C                   exsr      impvat                                       => carico  VAT
018700030506     C                   exsr      wrivat                                       => scarico VAT
018800030506     C                   endif
018900000905     C*
019000000905     C                   else
019100000905     C                   eval      vinflg = '1'
019200000905     C                   endif
019300030411     C                   endif
019400000905     C*
019500000905     C  N70              update    tivin000
019600000905     C*
019700991022     C  N70              ENDdo
019800010202     C*
019900010202     C                   endif
020000990910
020100990910     C* Se non ci sono record con errori ...
020200000710     C                   if        §ctrno = 0
020300990910     C* ... restituisco esito OK.
020400990921     C                   eval      wrkesito = '0'
020500990910     C                   else
020600010201     C                   if        §ctrokvb > 0
020700990921     C                   eval      wrkesito = '1'
020800000710     C                   else
020900000710     C                   eval      wrkesito = '2'
021000990910     C                   endif
021100000710     C                   endif
021200990910     C*
021300990914     C                   if        %open(tivin00r)
021400990908     C                   close     tivin00r
021500990914     C                   endif
021600021017     C                   if        %open(fivabwwr)
021700021017     C                   close     fivabwwr
021800990914     C                   endif
021900021017     C                   if        %open(fivatwwr)
022000021017     C                   close     fivatwwr
022100010201     C                   endif
022200021017     C*
022300021017     C                   if        vlrpoi <> 999
022400021017     C                   eval      vlrpoi = vlrpoi
022500021017     C                   endif
022600990910     C*
022700010201     C                   if        §ctrokvb > 0
022800021017     C                             and vlrpoi > *zeros
022900010202     C                   exsr      invio
023000990920     C                   endif
023100990920     C*
023200910830     C                   ENDSR
023300000613     C***
023400990920
023500000801     C*----------------------------------------------------*
023600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023700000801     C*----------------------------------------------------*
023800010201     C     INZVAR        BEGSR
023900000801     C*
024000010201     C                   Z-ADD     *zeros        Num5_0
024100030107     C                   Z-ADD     *zeros        Num7_0
024200000801     C*
024300000801     C                   ENDSR
024400000801     C*----------------------------------------------------*
024500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024600000801     C*----------------------------------------------------*
024700000801     C     DEFCAM        BEGSR
024800000801     C*
024900020619     C* Imposto i valori di default...
025000030506     C                   Z-ADD     0501798       VABCCM
025100030506     C                   Z-ADD     0501798       VATCCM
025200021017     C                   Z-ADD     vlrpoi        VABFGS
025300021017     C                   Z-ADD     vlrpoi        VATFGS
025400030506     C                   Z-ADD     050           VABLNP
025500030506     C                   Z-ADD     050           VATLNP
025600030506     C                   Z-ADD     000           VABCTR
025700021017     C                   MOVEL     '7Q'          VABCTM
025800020305     C                   MOVEL     'E'           VATTRC
025900020619     C* ... e poi verifico se sono stati passati come parametri
026000020619     C                   IF        vlrppt > *blanks
026100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026200020619     C                   EXSR      CHKNUM
026300020619     C                   IF        PiInt=*on
026400020619     C                   Z-ADD     PiVal         VABCCM
026500020619     C                   Z-ADD     PiVal         VATCCM
026600020619     C                   ENDIF
026700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026800020619     C                   EXSR      CHKNUM
026900020619     C                   IF        PiInt=*on
027000020619     C                   Z-ADD     PiVal         VABLNP
027100020619     C                   Z-ADD     PiVal         VATLNP
027200020619     C                   ENDIF
027300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027400020619     C                   EXSR      CHKNUM
027500020619     C                   IF        PiInt=*on
027600020619     C                   Z-ADD     PiVal         VABCTR
027700020619     C                   ENDIF
027800020619     C                   ENDIF
027900000801     C*
028000000801     C                   ENDSR
028100000801     C*----------------------------------------------------*
028200030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028300000801     C*----------------------------------------------------*
028400010201     C     IMPVAB        BEGSR
028500000801     C*
028600020305     C                   EXSR      INZVAR
028700020305     C                   EXSR      DEFCAM
028800010305     C*
028900030226     C                   Z-ADD     *zeros        errore            1 0
029000000830     C                   MOVEL     datcor        VABAAS
029100000830     C                   MOVE      datcor        VABMGS
029200030226     C*
029300030506     C                   EVAL      VABRSD=%trim(%subst(vindta:53:35))
029400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029600020117     C* ==
029700030520     C                   IF        %subst(vindta:123:35) <> *blanks
029800030506     C                   EVAL      VABIND=%trim(%subst(vindta:123:35))
029900030520     C                   ELSE
030000030520     C                   EVAL      VABIND=%trim(%subst(vindta:88:35))
030100030520     C                   ENDIF
030200030506     C                   EVAL      VABLOD=%trim(%subst(vindta:238:35))
030300030506     C*
030400030506     C* Effettuo alcune considerazioni sulla località x estrapolare la provincia racchiusa tra ()
030500030506     C                   Z-ADD     *zeros        pos               3 0
030600030506     C                   EVAL      pos = %scan('(':VABLOD)
030700030506     C                   IF        pos > *zeros
030800030506     C                   EVAL      VABPRD=%trim(%subst(vindta:(238+pos):2))
030900030506     C                   EVAL      VABLOD=%trim(%subst(vindta:238:(pos-1)))
031000030506     C                   ENDIF
031100030506     C*
031200030506     C                   EVAL      VABRMA=%trim(%subst(vindta:22:7))
031300030506     C                   EVAL      VABVAS=%trim(%subst(vindta:296:3))
031400050530     C                   EVAL      VABVCA=%trim(%subst(vindta:296:3))
031500030506     C                   EVAL      VABCBO = '1'
031600030226     C* CAP
031700030506     C                   EVAL      PiStr=%trim(%subst(vindta:228:5))
031800010201     C                   EXSR      CHKNUM
031900010201     C                   IF        PiInt=*on
032000010201     C                   Z-ADD     PiVal         Num5_0
032100021017     C                   MOVEL     Num5_0        VABCAD
032200010201     C                   ELSE
032300010201     C                   ADD       1             errore
032400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032500021017     C                             + ' ' + 'VABCAD'
032600010201     C                   ENDIF
032700030506     C* NSP/RMN
032800030506     C                   EVAL      PiStr=%trim(%subst(vindta:22:7))
032900010201     C                   EXSR      CHKNUM
033000010201     C                   IF        PiInt=*on
033100030107     C                   Z-ADD     PiVal         Num7_0
033200030107     C                   MOVE(P)   Num7_0        VABNSP
033300030506     C                   MOVE(P)   Num7_0        VABRMN
033400030107     C                   MOVE(P)   Num7_0        VATNSP
033500010201     C                   ELSE
033600010201     C                   ADD       1             errore
033700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033800030506     C                             + ' ' + 'VABNSP VABRMN VATNSP'
033900010201     C                   ENDIF
034000030226     C* NCL
034100030506     C                   EVAL      PiStr=%trim(%subst(vindta:316:5))
034200030430     C                   EXSR      CHKNUM
034300030430     C                   IF        PiInt=*on
034400030430     C                   Z-ADD     PiVal         VABNCL
034500030430     C                   ELSE
034600030430     C                   ADD       1             errore
034700030430     C                   EVAL      vinmsg = %trimr(vinmsg)
034800030430     C                             + ' ' + 'VABNCL'
034900030430     C                   ENDIF
035000030506     C* PKB
035100030506     C                   EVAL      PiStr=%trim(%subst(vindta:321:9))
035200030107     C                   EXSR      CHKNUM
035300030107     C                   IF        PiNum=*on
035400030506     C                   EVAL(H)   VABPKB = PiVal/1000
035500030107     C                   ELSE
035600030107     C                   ADD       1             errore
035700030107     C                   EVAL      vinmsg = %trimr(vinmsg)
035800030107     C                             + ' ' + 'VABPKB'
035900030107     C                   ENDIF
036000050530     C*
036100050530     C* Reperisco valore importo valore merce da assicurare e contrassegno e il relativo
036200050530     C* tipo incasso, nonchè la divisa
036300050530     C                   IF        %len(%trim(%subst(vindta:276:10))) > 2
036400050530     C                   EVAL      VABTIC=%subst(%subst(vindta:276:10):
036500050530     C                                    %len(%trim(%subst(vindta:276:10)))-2+1
036600050530     C                                    :2)
036700050530     C                   IF        VABTIC='BM' OR VABTIC='TM'
036800050530     C                   EVAL      PiStr=%subst(%subst(vindta:276:10):1:
036900050530     C                                   %len(%trim(%subst(vindta:276:10)))-2)
037000050530     C                   ELSE
037100050530     C                   EVAL      PiStr=%trim(%subst(vindta:276:10))
037200050530     C                   EVAL      VABTIC=*blanks
037300050530     C                   ENDIF
037400050530     C                   ELSE
037500050530     C                   EVAL      PiStr=%trim(%subst(vindta:276:10))
037600050530     C                   ENDIF
037700050530     C* IAS/CAS
037800030430     C                   EXSR      CHKNUM
037900030430     C                   IF        PiNum=*on
038000030506     C                   Z-ADD     PiVal         VABIAS
038100050530     C* Gestisco il contrassegno solo se indicato un TIPO INCASSO CONTRASSEGNO
038200050530     C                   IF        VABTIC <> *blanks
038300050530     C                   Z-ADD     PiVal         VABCAS
038400050530     C                   ENDIF
038500050530     C*
038600030430     C                   ELSE
038700030430     C                   SETON                                        32
038800030430     C                   Z-ADD     *zeros        VABIAS
038900050530     C                   Z-ADD     *zeros        VABCAS
039000030430     C                   EVAL      vinmsg = %trimr(vinmsg)
039100050530     C                             + ' ' + 'VABIAS'
039200050530     C* Gestisco il contrassegno solo se indicato un TIPO INCASSO CONTRASSEGNO
039300050530     C                   IF        VABTIC <> *blanks
039400050530     C                   EVAL      vinmsg = %trimr(vinmsg)
039500050530     C                             + ' ' + 'VABCAS'
039600050530     C                   ENDIF
039700030430     C                   ENDIF
039800050530     C*
039900050530     C* Stabilisco li tipo bolla
040000050530     C                   IF        VABCAS > 0
040100050530     C                   IF        VABCBO = '1'
040200050530     C                   EVAL      VABCBO = '4'
040300050530     C                   ENDIF
040400050530     C                   IF        VABCBO = '2'
040500050530     C                   EVAL      VABCBO = '6'
040600050530     C                   ENDIF
040700050530     C                   ENDIF
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
042000010201     C                   ADD       1             §CTROKVB
042100000801     C                   ENDIF
042200000801     C*
042300000801     C                   ENDSR
042400030226     C*----------------------------------------------------*
042500030226     C*  SCARICAMENTO BUFFER RECORDS VAB
042600030226     C*----------------------------------------------------*
042700030226     C     WRIVAB        BEGSR
042800030310     C*
042900030310     C                   write     FIVAB000
043000030226     C*
043100030226     C                   ENDSR
043200030226     C*----------------------------------------------------*
043300030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
043400030226     C*----------------------------------------------------*
043500030226     C     IMPVAT        BEGSR
043600030226     C*
043700030226     C                   MOVEL     datcor        VATAAS
043800030226     C*
043900030415     C* Valorizzo il barcode
044000030506     C                   EVAL      VATNOT=%trim(%subst(vindta:45:15))
044100030226     C*
044200030226     C                   ENDSR
044300010201     C*----------------------------------------------------*
044400021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044500010201     C*----------------------------------------------------*
044600020305     C     WRIVAT        BEGSR
044700010201     C*
044800021017     C                   write     FIVAT000
044900010201     C*
045000010201     C                   ENDSR
045100010202     C*----------------------------------------------------*
045200021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045300010202     C*----------------------------------------------------*
045400020305     C     PREVAT        BEGSR
045500010202     C*
045600021017     C* Compongo il nome del membro da dare al FIVATWWR
045700010202     C                   eval      parmbr = vlrhdl
045800010202     C                   movel     'M'           parmbr
045900030506     C                   eval      parccm = '00501798'
046000010202     C                   eval      paropz = '1'
046100010202     C* Effettuo la chiamata al CLLE preposto
046200050128     C                   call(e)   'TITVVTC'
046300010202     C                   parm                    parccm
046400010202     C                   parm                    parmbr
046500010202     C                   parm                    paropz
046600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046700010202     C                   if        %error
046800010202     C                   movel     '1'           chkcall
046900010202     C                   else
047000010202     C                   movel     '0'           chkcall
047100010202     C                   endif
047200010202     C*
047300010202     C                   ENDSR
047400000801     C*----------------------------------------------------*
047500000801     C*  CONTROLLO NUMERICITA' CAMPI
047600000801     C*----------------------------------------------------*
047700000801     C     CHKNUM        BEGSR
047800000801     C*
047900000801     C                   call(e)   'ISNUMERIC'
048000000801     C                   PARM                    PiStr            30
048100030430     C                   PARM      '.'           PiDecChr          1
048200000801     C                   PARM      *ZEROS        PiVal            30 9
048300000801     C                   PARM      '0'           PiInt             1
048400000801     C                   PARM      '0'           PiNum             1
048500000801     C                   IF        %error
048600000801     C                   EVAL      PiInt=*off
048700030107     C                   EVAL      PiNum=*off
048800000801     C                   ENDIF
048900000801     C*
049000000801     C                   ENDSR
049100000801     C***
049200000801
049300011113
049400011113     C*----------------------------------------------------*
049500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049600011113     C*----------------------------------------------------*
049700011113     C     CHKIMPDIV     BEGSR
049800011113     C*
049900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050000011113     C                   Z-ADD     *zeros        wrkDec            9 9
050100011113     C*
050200011113     C* Come prima cosa effettuo considerazioni sulla divisa
050300011113     C                   IF        vabIAS > *zeros
050400011113     C                   IF        vabVAS <> 'EUR'
050500011113     C                   EVAL      vabVAS =  'ITL'
050600011113     C                   ENDIF
050700011113     C                   ENDIF
050800011113     C*
050900011113     C                   IF        vabCAS > *zeros
051000011113     C                   IF        vabVCA <> 'EUR'
051100011113     C                   EVAL      vabVCA =  'ITL'
051200011113     C                   ENDIF
051300011113     C                   ENDIF
051400011113     C*
051500011113     C                   IF        vabVMD > *zeros
051600020305     C                   IF        vabVAD <> 'EUR'
051700011113     C                   EVAL      vabVAD =  'ITL'
051800011113     C                   ENDIF
051900011113     C                   ENDIF
052000011113     C*
052100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052200011113     C                   Z-ADD     vabIAS        wrkDec
052300011113     C                   IF        wrkDec > *zeros
052400011113     C                   IF        vabVAS = 'ITL'
052500011113     C                   EVAL      vabIAS = *zeros
052600011113     C                   ENDIF
052700011113     C                   ENDIF
052800011113     C*
052900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
053000011113     C                   Z-ADD     vabCAS        wrkDec
053100011113     C                   IF        wrkDec > *zeros
053200011113     C                   IF        vabVCA = 'ITL'
053300011113     C                   EVAL      vabCAS = *zeros
053400011113     C                   ENDIF
053500011113     C                   ENDIF
053600011113     C*
053700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053800011113     C                   Z-ADD     vabVMD        wrkDec
053900011113     C                   IF        wrkDec > *zeros
054000011113     C                   IF        vabVAD = 'ITL'
054100011113     C                   EVAL      vabVMD = *zeros
054200011113     C                   ENDIF
054300011113     C                   ENDIF
054400011113     C*
054500011113     C                   ENDSR
054600011113     C***
054700021017
054800000801
054900000801
055000990920      /TITLE Invio dei dati al punto operativo.
055100010202     C     invio         BEGSR
055200990920     C*
055300021017     C* 1° invio FIVAT
055400010201     C                   reset                   dscmz
055500021017     C                   move      vlrpoi        cmzdst
055600021017     C                   eval      cmzfld = 'FIVATWWR'
055700010201     C                   eval      cmzmbd = vlrhdl
055800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055900021009     C***                if        prmfir = *blanks
056000021017     C                   eval      cmzfla = 'FIVAT00F'
056100021017     C                   eval      cmzmba = 'FIVAT00F'
056200021009     C***                else
056300021009     C***                eval      cmzfla = prmfir
056400021009     C***                eval      cmzmba = prmfir
056500021009     C***                endif
056600010201     C                   eval      cmznrr = *zeros
056700020305     C                   move      §ctrokvt      cmznrr
056800021018     C                   eval      cmzlba = vlrfl1
056900010201     C                   call(e)   'TIS711C'
057000010201     C                   parm                    dscmz
057100010201     C                   parm      *blanks       esito
057200010205     C                   if        %error
057300010205     C                             or cmzerr = '1'
057400010205     C                             or esito  = '1'
057500010205     C                   eval      wrkesito = '3'
057600010205     C                   else
057700010201     C*
057800021017     C* 2° invio FIVAB
057900010201     C                   reset                   dscmz
058000021017     C                   move      vlrpoi        cmzdst
058100010201     C                   eval      cmzfld = vlrfou
058200010201     C                   eval      cmzmbd = vlrhdl
058300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058400021009     C***                if        prmfir = *blanks
058500021017     C                   eval      cmzfla = 'FIVAB00F'
058600021017     C                   eval      cmzmba = 'FIVAB00F'
058700021009     C***                else
058800021009     C***                eval      cmzfla = prmfir
058900021009     C***                eval      cmzmba = prmfir
059000021009     C***                endif
059100010201     C                   eval      cmznrr = *zeros
059200010201     C                   move      §ctrokvb      cmznrr
059300021018     C                   eval      cmzlba = vlrfl1
059400010201     C                   call(e)   'TIS711C'
059500010201     C                   parm                    dscmz
059600010201     C                   parm      *blanks       esito
059700010201     C                   if        %error
059800010201     C                             or cmzerr = '1'
059900010201     C                             or esito  = '1'
060000010201     C                   eval      wrkesito = '3'
060100010201     C                   endif
060200010205     C                   endif
060300990920     C*
060400000613     C                   ENDSR
060500000613     C***
060600070411
060700070411     C     *pssr         BEGSR
060800070411     C*
060900070411     C                   if        %open(tivin00r)
061000070411     C                   close     tivin00r
061100070411     C                   endif
061200070411     C                   if        %open(fivabwwr)
061300070411     C                   close     fivabwwr
061400070411     C                   endif
061500070411     C                   if        %open(fivatwwr)
061600070411     C                   close     fivatwwr
061700070411     C                   endif
061800070411     C*
061900070411     C* Effettuo la chiamata al CLLE preposto
062000070411     C                   call(e)   'TITVVTC'
062100070411     C                   parm                    parccm
062200070411     C                   parm                    parmbr
062300070411     C                   parm      '2'           paropz
062400070411     C*
062500070411     C                   eval      wrkesito = '2'
062600070411     C*
062700070411     C                   seton                                        LR
062800070411     C*
062900070411     C                   ENDSR     '*CANCL'
063000070411     C***
063100070411
063200990910
063300000613     C     *inzsr        BEGSR
063400990910     C*
063500990910     C     *entry        plist
063600990920     C                   parm                    tivlrds
063700990921     C                   parm      wrkesito      esito
063800000724     C                   parm                    prmlit
063900000710     C                   parm                    prmfir
064000000613     C*
064100000830     C* CALCOLA LA DATA CORRENTE
064200000830     C                   time                    wn14             14 0
064300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064400000830     C                   z-add     wn8           g08dat
064500000830     C                   z-add     *zeros        g08inv
064600000830     C                   movel     '0'           g08err
064700000830     C                   call      'XSRDA8'
064800000830     C                   parm                    wlbda8
064900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065000000830     C*
065100000613     C                   ENDSR
065200000613     C***
065300990908
065400030506     Otitv52p   E            riepilogo         2
065500990915     O                                              'Upload via Internet'
065600990915     O                                           +1 'Traduzione TIVIN00R -'
065700021017     O                                           +1 'FIVABWWR/FIVATWWR'
065800010201     O                                           +1 'Report testate bolle'
065900990915     O          E            riepilogo   2
066000990915     O                       wrkdata
066100990915     O                       wrkora              +1
066200990915     O                       procname            +1
066300990915     O          E            riepilogo   2
066400990915     O                                              'Cliente..................:'
066500990915     O                       VABCCM        z     +1
066600990915     O          E            riepilogo   2
066700990920     O                                              'Riferimento Strategi.....:'
066800990920     O                       vlrhdl              +1
066900990915     O          E            riepilogo   2
067000990915     O                                              'Giusti...................:'
067100010201     O                       §CTROKVB      2   +  1
067200990915     O          E            riepilogo   2
067300990915     O                                              'Sbagliati e corretti.....:'
067400971022     O                       §CTRMO        2   +  1
067500990915     O          E            riepilogo   2
067600990915     O                                              'Sbagliati e scartati.....:'
067700971022     O                       §CTRNO        2   +  1
067800000613
067900030506     Otitv52ps  E            testdett          2
068000000613     O                                              'Upload via Internet'
068100000613     O                                           +1 'Traduzione TIVIN00R -'
068200021017     O                                           +1 'FIVABWWR/FIVATWWR'
068300010201     O                                           +1 'Report testate bolle'
068400000616     O          E            testdett    3
068500000613     O                                           +2 'N° rec'
068600000613     O                                           +3 'Anteprima contenuto'
068700000616     O          E            rigadett    2
068800000613     O                       rrnum               +2
068900000621     O                       recko               +3
069000000616     O          E            findett     2
069100000613     O                       wrkdata
069200000613     O                       wrkora              +1
069300000613     O                       procname            +1
069400000616     O          E            findett     2
069500000613     O                                              'Cliente..................:'
069600000613     O                       VABCCM        z     +1
069700000616     O          E            findett     2
069800000613     O                                              'Riferimento Strategi.....:'
069900000613     O                       vlrhdl              +1
070000000616     O          E            findett     2
070100000613     O                                              'Giusti...................:'
070200010201     O                       §CTROKVB      2   +  1
070300000616     O          E            findett     2
070400000613     O                                              'Sbagliati e corretti.....:'
070500000613     O                       §CTRMO        2   +  1
070600000616     O          E            findett     2
070700000613     O                                              'Sbagliati e scartati.....:'
070800000613     O                       §CTRNO        2   +  1
070900000512** CMD - COMANDI CL
071000030506OVRPRTF FILE(TITV52P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
071100030506OVRPRTF FILE(TITV52PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071200030506DLTOVR FILE(TITV52P TITV52PS) LVL(*)
071300000512
071400000512
