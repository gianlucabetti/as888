000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030415     Ftitv50p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030415     Ftitv50ps  O    f  198        PRINTER usropn
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
009300030415     C                   if        not %open(titv50ps)
009400030415     C                   open      titv50ps
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
010700030415     C                   if        %open(titv50ps)
010800000616     C                   except    findett
010900030415     C                   close     titv50ps
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
012600030415     C                   if        not %open(titv50p)
012700030415     C                   open      titv50p
012800990915     C                   endif
012900990915     C*
013000990915     C                   except    riepilogo
013100990915     C*
013200030415     C                   if        %open(titv50p)
013300030415     C                   close     titv50p
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
018100030415     C                   eval      PiStr=%trim(%subst(vindta:7:7))
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
027500030415     C                   Z-ADD     0492593       VABCCM
027600030415     C                   Z-ADD     0492593       VATCCM
027700021017     C                   Z-ADD     vlrpoi        VABFGS
027800021017     C                   Z-ADD     vlrpoi        VATFGS
027900030415     C                   Z-ADD     049           VABLNP
028000030415     C                   Z-ADD     049           VATLNP
028100030226     C                   Z-ADD     000           VABCTR
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
031800030415     C                   EVAL      VABRSD=%trim(%subst(vindta:32:30))
031900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032100020117     C* ==
032200030415     C                   IF        %subst(vindta:62:3) = 'IT'
032300030415     C                   ELSE
032400030415     C                   EVAL      VABNZD=%trim(%subst(vindta:62:3))
032500030415     C                   ENDIF
032600030415     C                   EVAL      VABIND=%trim(%subst(vindta:65:30))
032700030415     C                   EVAL      VABLOD=%trim(%subst(vindta:155:30))
032800030415     C                   EVAL      VABPRD=%trim(%subst(vindta:95:30))
032900030415     C                   EVAL      VABRMA=%trim(%subst(vindta:22:10))
033000030311     C                   EVAL      VABVCA='EUR'
033100030226     C* CAP
033200030415     C                   EVAL      PiStr=%trim(%subst(vindta:185:10))
033300010201     C                   EXSR      CHKNUM
033400010201     C                   IF        PiInt=*on
033500010201     C                   Z-ADD     PiVal         Num5_0
033600021017     C                   MOVEL     Num5_0        VABCAD
033700010201     C                   ELSE
033800010201     C                   ADD       1             errore
033900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034000021017     C                             + ' ' + 'VABCAD'
034100010201     C                   ENDIF
034200030311     C* NSP
034300030415     C                   EVAL      PiStr=%trim(%subst(vindta:7:7))
034400010201     C                   EXSR      CHKNUM
034500010201     C                   IF        PiInt=*on
034600030107     C                   Z-ADD     PiVal         Num7_0
034700030107     C                   MOVE(P)   Num7_0        VABNSP
034800030107     C                   MOVE(P)   Num7_0        VATNSP
034900010201     C                   ELSE
035000010201     C                   ADD       1             errore
035100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035200030311     C                             + ' ' + 'VABNSP VATNSP'
035300010201     C                   ENDIF
035400030314     C* RMN
035500030415     C                   EVAL      PiStr=%trim(%subst(vindta:1:13))
035600030311     C                   EXSR      CHKNUM
035700030311     C                   IF        PiInt=*on
035800030311     C                   Z-ADD     PiVal         VABRMN
035900030311     C                   ELSE
036000030311     C                   ADD       1             errore
036100030311     C                   EVAL      vinmsg = %trimr(vinmsg)
036200030311     C                             + ' ' + 'VABRMN'
036300030311     C                   ENDIF
036400030226     C* NCL
036500030415     C                   EVAL      VABNCL = VABNCL + 1
036600030226     C* PKB
036700030415     C                   EVAL      PiStr=%trim(%subst(vindta:203:7))
036800030107     C                   EXSR      CHKNUM
036900030107     C                   IF        PiNum=*on
037000030415     C                   ADD       PiVal         VABPKB
037100030107     C                   ELSE
037200030107     C                   ADD       1             errore
037300030107     C                   EVAL      vinmsg = %trimr(vinmsg)
037400030107     C                             + ' ' + 'VABPKB'
037500030107     C                   ENDIF
037600030415     C* CAS
037700030415     C                   IF        %subst(vindta:210:13) <> '0000000000,00'
037800030310     C                   MOVEL     '1'           FlgCAS
037900030804     C                   EVAL      VABTIC = 'OM'
038000040628     C                   EVAL      VABRMO = 'YAMAHA MOTOR ITALIA SPA'
038100030804     C                   EVAL      VABCMO = '20050'
038200030415     C                   EVAL      PiStr=%trim(%subst(vindta:210:13))
038300030310     C                   EXSR      CHKNUM
038400030310     C                   IF        PiNum=*on
038500030714     C                   ADD       PiVal         VABCAS
038600030310     C                   ELSE
038700030310     C                   SETON                                        32
038800030310     C                   Z-ADD     *zeros        VABCAS
038900030310     C                   EVAL      vinmsg = %trimr(vinmsg)
039000030310     C                             + ' ' + 'VABCAS'
039100030310     C                   ENDIF
039200030310     C                   ENDIF
039300030310     C*
039400030310     C* Considerazioni finali su CBO/CAS
039500030310     C                   IF        FlgCAS = '1'
039600010205     C                   EVAL      VABCBO = '4'
039700010205     C                   ELSE
039800010205     C                   EVAL      VABCBO = '1'
039900010205     C                   ENDIF
040000020305     C*
040100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040200011113     C                   EXSR      CHKIMPDIV
040300010202     C*
040400000801     C* Ebbene...
040500000801     C                   ADD       1             §CTRMO
040600010201     C                   IF        errore <> *zeros
040700000801     C                   ADD       1             §CTRNO
040800000801     C                   EVAL      recko = vindta
040900000801     C                   EXCEPT    rigadett
041000000801     C                   EVAL      vinflg = '2'
041100000801     C                   ELSE
041200010201     C                   ADD       1             §CTROKVB
041300000801     C                   ENDIF
041400000801     C*
041500000801     C                   ENDSR
041600030226     C*----------------------------------------------------*
041700030226     C*  SCARICAMENTO BUFFER RECORDS VAB
041800030226     C*----------------------------------------------------*
041900030226     C     WRIVAB        BEGSR
042000030310     C*
042100030310     C                   write     FIVAB000
042200030226     C*
042300030226     C                   ENDSR
042400030226     C*----------------------------------------------------*
042500030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
042600030226     C*----------------------------------------------------*
042700030226     C     IMPVAT        BEGSR
042800030226     C*
042900030226     C                   EXSR      INZVAR
043000030226     C                   EXSR      DEFCAM
043100030226     C*
043200030226     C                   MOVEL     datcor        VATAAS
043300030226     C*
043400030415     C* Valorizzo il barcode
043500030415     C                   EVAL      VATNOT=%trim(%subst(vindta:14:8))
043600030226     C*
043700030226     C                   ENDSR
043800010201     C*----------------------------------------------------*
043900021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044000010201     C*----------------------------------------------------*
044100020305     C     WRIVAT        BEGSR
044200010201     C*
044300021017     C                   write     FIVAT000
044400010201     C*
044500010201     C                   ENDSR
044600010202     C*----------------------------------------------------*
044700021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044800010202     C*----------------------------------------------------*
044900020305     C     PREVAT        BEGSR
045000010202     C*
045100021017     C* Compongo il nome del membro da dare al FIVATWWR
045200010202     C                   eval      parmbr = vlrhdl
045300010202     C                   movel     'M'           parmbr
045400030415     C                   eval      parccm = '00492593'
045500010202     C                   eval      paropz = '1'
045600010202     C* Effettuo la chiamata al CLLE preposto
045700050128     C                   call(e)   'TITVVTC'
045800010202     C                   parm                    parccm
045900010202     C                   parm                    parmbr
046000010202     C                   parm                    paropz
046100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046200010202     C                   if        %error
046300010202     C                   movel     '1'           chkcall
046400010202     C                   else
046500010202     C                   movel     '0'           chkcall
046600010202     C                   endif
046700010202     C*
046800010202     C                   ENDSR
046900000801     C*----------------------------------------------------*
047000000801     C*  CONTROLLO NUMERICITA' CAMPI
047100000801     C*----------------------------------------------------*
047200000801     C     CHKNUM        BEGSR
047300000801     C*
047400000801     C                   call(e)   'ISNUMERIC'
047500000801     C                   PARM                    PiStr            30
047600030107     C                   PARM      ','           PiDecChr          1
047700000801     C                   PARM      *ZEROS        PiVal            30 9
047800000801     C                   PARM      '0'           PiInt             1
047900000801     C                   PARM      '0'           PiNum             1
048000000801     C                   IF        %error
048100000801     C                   EVAL      PiInt=*off
048200030107     C                   EVAL      PiNum=*off
048300000801     C                   ENDIF
048400000801     C*
048500000801     C                   ENDSR
048600000801     C***
048700000801
048800011113
048900011113     C*----------------------------------------------------*
049000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049100011113     C*----------------------------------------------------*
049200011113     C     CHKIMPDIV     BEGSR
049300011113     C*
049400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049500011113     C                   Z-ADD     *zeros        wrkDec            9 9
049600011113     C*
049700011113     C* Come prima cosa effettuo considerazioni sulla divisa
049800011113     C                   IF        vabIAS > *zeros
049900011113     C                   IF        vabVAS <> 'EUR'
050000011113     C                   EVAL      vabVAS =  'ITL'
050100011113     C                   ENDIF
050200011113     C                   ENDIF
050300011113     C*
050400011113     C                   IF        vabCAS > *zeros
050500011113     C                   IF        vabVCA <> 'EUR'
050600011113     C                   EVAL      vabVCA =  'ITL'
050700011113     C                   ENDIF
050800011113     C                   ENDIF
050900011113     C*
051000011113     C                   IF        vabVMD > *zeros
051100020305     C                   IF        vabVAD <> 'EUR'
051200011113     C                   EVAL      vabVAD =  'ITL'
051300011113     C                   ENDIF
051400011113     C                   ENDIF
051500011113     C*
051600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051700011113     C                   Z-ADD     vabIAS        wrkDec
051800011113     C                   IF        wrkDec > *zeros
051900011113     C                   IF        vabVAS = 'ITL'
052000011113     C                   EVAL      vabIAS = *zeros
052100011113     C                   ENDIF
052200011113     C                   ENDIF
052300011113     C*
052400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052500011113     C                   Z-ADD     vabCAS        wrkDec
052600011113     C                   IF        wrkDec > *zeros
052700011113     C                   IF        vabVCA = 'ITL'
052800011113     C                   EVAL      vabCAS = *zeros
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053300011113     C                   Z-ADD     vabVMD        wrkDec
053400011113     C                   IF        wrkDec > *zeros
053500011113     C                   IF        vabVAD = 'ITL'
053600011113     C                   EVAL      vabVMD = *zeros
053700011113     C                   ENDIF
053800011113     C                   ENDIF
053900011113     C*
054000011113     C                   ENDSR
054100011113     C***
054200021017
054300000801
054400000801
054500990920      /TITLE Invio dei dati al punto operativo.
054600010202     C     invio         BEGSR
054700990920     C*
054800021017     C* 1° invio FIVAT
054900010201     C                   reset                   dscmz
055000021017     C                   move      vlrpoi        cmzdst
055100021017     C                   eval      cmzfld = 'FIVATWWR'
055200010201     C                   eval      cmzmbd = vlrhdl
055300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055400021009     C***                if        prmfir = *blanks
055500021017     C                   eval      cmzfla = 'FIVAT00F'
055600021017     C                   eval      cmzmba = 'FIVAT00F'
055700021009     C***                else
055800021009     C***                eval      cmzfla = prmfir
055900021009     C***                eval      cmzmba = prmfir
056000021009     C***                endif
056100010201     C                   eval      cmznrr = *zeros
056200020305     C                   move      §ctrokvt      cmznrr
056300021018     C                   eval      cmzlba = vlrfl1
056400010201     C                   call(e)   'TIS711C'
056500010201     C                   parm                    dscmz
056600010201     C                   parm      *blanks       esito
056700010205     C                   if        %error
056800010205     C                             or cmzerr = '1'
056900010205     C                             or esito  = '1'
057000010205     C                   eval      wrkesito = '3'
057100010205     C                   else
057200010201     C*
057300021017     C* 2° invio FIVAB
057400010201     C                   reset                   dscmz
057500021017     C                   move      vlrpoi        cmzdst
057600010201     C                   eval      cmzfld = vlrfou
057700010201     C                   eval      cmzmbd = vlrhdl
057800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057900021009     C***                if        prmfir = *blanks
058000021017     C                   eval      cmzfla = 'FIVAB00F'
058100021017     C                   eval      cmzmba = 'FIVAB00F'
058200021009     C***                else
058300021009     C***                eval      cmzfla = prmfir
058400021009     C***                eval      cmzmba = prmfir
058500021009     C***                endif
058600010201     C                   eval      cmznrr = *zeros
058700010201     C                   move      §ctrokvb      cmznrr
058800021018     C                   eval      cmzlba = vlrfl1
058900010201     C                   call(e)   'TIS711C'
059000010201     C                   parm                    dscmz
059100010201     C                   parm      *blanks       esito
059200010201     C                   if        %error
059300010201     C                             or cmzerr = '1'
059400010201     C                             or esito  = '1'
059500010201     C                   eval      wrkesito = '3'
059600010201     C                   endif
059700010205     C                   endif
059800990920     C*
059900000613     C                   ENDSR
060000000613     C***
060100070411
060200070411     C     *pssr         BEGSR
060300070411     C*
060400070411     C                   if        %open(tivin00r)
060500070411     C                   close     tivin00r
060600070411     C                   endif
060700070411     C                   if        %open(fivabwwr)
060800070411     C                   close     fivabwwr
060900070411     C                   endif
061000070411     C                   if        %open(fivatwwr)
061100070411     C                   close     fivatwwr
061200070411     C                   endif
061300070411     C*
061400070411     C* Effettuo la chiamata al CLLE preposto
061500070411     C                   call(e)   'TITVVTC'
061600070411     C                   parm                    parccm
061700070411     C                   parm                    parmbr
061800070411     C                   parm      '2'           paropz
061900070411     C*
062000070411     C                   eval      wrkesito = '2'
062100070411     C*
062200070411     C                   seton                                        LR
062300070411     C*
062400070411     C                   ENDSR     '*CANCL'
062500070411     C***
062600070411
062700990910
062800000613     C     *inzsr        BEGSR
062900990910     C*
063000990910     C     *entry        plist
063100990920     C                   parm                    tivlrds
063200990921     C                   parm      wrkesito      esito
063300000724     C                   parm                    prmlit
063400000710     C                   parm                    prmfir
063500000613     C*
063600000830     C* CALCOLA LA DATA CORRENTE
063700000830     C                   time                    wn14             14 0
063800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063900000830     C                   z-add     wn8           g08dat
064000000830     C                   z-add     *zeros        g08inv
064100000830     C                   movel     '0'           g08err
064200000830     C                   call      'XSRDA8'
064300000830     C                   parm                    wlbda8
064400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064500000830     C*
064600000613     C                   ENDSR
064700000613     C***
064800990908
064900030415     Otitv50p   E            riepilogo         2
065000990915     O                                              'Upload via Internet'
065100990915     O                                           +1 'Traduzione TIVIN00R -'
065200021017     O                                           +1 'FIVABWWR/FIVATWWR'
065300010201     O                                           +1 'Report testate bolle'
065400990915     O          E            riepilogo   2
065500990915     O                       wrkdata
065600990915     O                       wrkora              +1
065700990915     O                       procname            +1
065800990915     O          E            riepilogo   2
065900990915     O                                              'Cliente..................:'
066000990915     O                       VABCCM        z     +1
066100990915     O          E            riepilogo   2
066200990920     O                                              'Riferimento Strategi.....:'
066300990920     O                       vlrhdl              +1
066400990915     O          E            riepilogo   2
066500990915     O                                              'Giusti...................:'
066600010201     O                       §CTROKVB      2   +  1
066700990915     O          E            riepilogo   2
066800990915     O                                              'Sbagliati e corretti.....:'
066900971022     O                       §CTRMO        2   +  1
067000990915     O          E            riepilogo   2
067100990915     O                                              'Sbagliati e scartati.....:'
067200971022     O                       §CTRNO        2   +  1
067300000613
067400030415     Otitv50ps  E            testdett          2
067500000613     O                                              'Upload via Internet'
067600000613     O                                           +1 'Traduzione TIVIN00R -'
067700021017     O                                           +1 'FIVABWWR/FIVATWWR'
067800010201     O                                           +1 'Report testate bolle'
067900000616     O          E            testdett    3
068000000613     O                                           +2 'N° rec'
068100000613     O                                           +3 'Anteprima contenuto'
068200000616     O          E            rigadett    2
068300000613     O                       rrnum               +2
068400000621     O                       recko               +3
068500000616     O          E            findett     2
068600000613     O                       wrkdata
068700000613     O                       wrkora              +1
068800000613     O                       procname            +1
068900000616     O          E            findett     2
069000000613     O                                              'Cliente..................:'
069100000613     O                       VABCCM        z     +1
069200000616     O          E            findett     2
069300000613     O                                              'Riferimento Strategi.....:'
069400000613     O                       vlrhdl              +1
069500000616     O          E            findett     2
069600000613     O                                              'Giusti...................:'
069700010201     O                       §CTROKVB      2   +  1
069800000616     O          E            findett     2
069900000613     O                                              'Sbagliati e corretti.....:'
070000000613     O                       §CTRMO        2   +  1
070100000616     O          E            findett     2
070200000613     O                                              'Sbagliati e scartati.....:'
070300000613     O                       §CTRNO        2   +  1
070400000512** CMD - COMANDI CL
070500030415OVRPRTF FILE(TITV50P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
070600030415OVRPRTF FILE(TITV50PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
070700030415DLTOVR FILE(TITV50P TITV50PS) LVL(*)
070800000512
070900000512
