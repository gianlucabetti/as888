000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030711     Ftitv61p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030711     Ftitv61ps  O    f  198        PRINTER usropn
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
003500020305     D depspe          s             14    INZ(*blanks)
003600010213     D depdat          s              8  0 INZ(*zeros)
003700020305     D curspe          s             14    INZ(*blanks)
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
009300030711     C                   if        not %open(titv61ps)
009400030711     C                   open      titv61ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700000613     C*
009800030107     C                   EXSR      RWFISCHER                                    LETT/SCR. VAB
009900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200030711     C                   call(e)   'TITVVTC'
010300010202     C                   parm                    parccm
010400010202     C                   parm                    parmbr
010500010202     C                   parm      '2'           paropz
010600010202     C*
010700030711     C                   if        %open(titv61ps)
010800000616     C                   except    findett
010900030711     C                   close     titv61ps
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
012700030711     C                   if        not %open(titv61p)
012800030711     C                   open      titv61p
012900990915     C                   endif
013000990915     C*
013100990915     C                   except    riepilogo
013200990915     C*
013300030711     C                   if        %open(titv61p)
013400030711     C                   close     titv61p
013500990914     C                   endif
013600990910     C*
013700000613     C                   ENDSR
013800000613     C***
013900990908
014000000801
014100910830     C*--------------------------------------------------------
014200030107     C* RWFISCHER LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
014300910830     C*--------------------------------------------------------
014400030107     C     RWFISCHER     BEGSR
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
018200020305     C                   MOVEL(p)  PiStr         curspe
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
026400030711     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026500030711     C                   MOVEL     '0'           FlgCAS            1
026600000801     C*
026700000801     C                   ENDSR
026800000801     C*----------------------------------------------------*
026900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027000000801     C*----------------------------------------------------*
027100000801     C     DEFCAM        BEGSR
027200000801     C*
027300021017     C                   CLEAR                   FIVAB000
027400020619     C* Imposto i valori di default...
027500030711     C                   Z-ADD     0600356       VABCCM
027600030711     C                   Z-ADD     0600356       VATCCM
027700021017     C                   Z-ADD     vlrpoi        VABFGS
027800021017     C                   Z-ADD     vlrpoi        VATFGS
027900030107     C                   Z-ADD     006           VABLNP
028000030107     C                   Z-ADD     006           VATLNP
028100030711     C                   Z-ADD     000           VABCTR
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
030700021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030800000801     C*----------------------------------------------------*
030900010201     C     IMPVAB        BEGSR
031000000801     C*
031100020305     C                   EXSR      INZVAR
031200020305     C                   EXSR      DEFCAM
031300010305     C*
031400000801     C                   Z-ADD     *zeros        errore            1 0
031500000830     C                   MOVEL     datcor        VABAAS
031600020305     C                   MOVEL     datcor        VATAAS
031700000830     C                   MOVE      datcor        VABMGS
031800030107     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
031900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032100020117     C* ==
032200030107     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
032300030107     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
032400030107     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
032500030711     C                   EVAL      VABRMA=%trim(%subst(vindta:320:7))
032600030107     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
032700030711     C                   EVAL      VABNOT='Tel. ' + %trim(%subst(vindta:92:15))
032800030711     C                   IF        %trim(VABNOT) = 'Tel.'
032900030711     C                   EVAL      VABNOT=*blanks
033000030711     C                   ENDIF
033100030711     C                   IF        %subst(vindta:16:1) = '1'
033200030711     C                   EVAL      VABFFD='S'
033300030711     C                   ENDIF
033400030711     C* CAD
033500030107     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
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
034600030711     C                   EVAL      PiStr=%trim(%subst(vindta:187:6))
034700010201     C                   EXSR      CHKNUM
034800010201     C                   IF        PiInt=*on
034900021017     C                   Z-ADD     PiVal         VABRMN
035000010201     C                   ELSE
035100010201     C                   ADD       1             errore
035200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035300030711     C                             + ' ' + 'VABRMN'
035400010201     C                   ENDIF
035500030711     C* NSP
035600030711     C                   EVAL      PiStr=%trim(%subst(vindta:320:7))
035700030711     C                   EXSR      CHKNUM
035800030711     C                   IF        PiInt=*on
035900030711     C                   Z-ADD     PiVal         Num7_0
036000030711     C                   MOVE(P)   Num7_0        VABNSP
036100030711     C                   MOVE(P)   Num7_0        VATNSP
036200030711     C                   ELSE
036300030711     C                   ADD       1             errore
036400030711     C                   EVAL      vinmsg = %trimr(vinmsg)
036500030711     C                             + ' ' + 'VABNSP VATNSP'
036600030711     C                   ENDIF
036700030711     C* NCL
036800030107     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
036900030107     C                   EXSR      CHKNUM
037000030107     C                   IF        PiInt=*on
037100030107     C                   Z-ADD     PiVal         VABNCL
037200030107     C                   ELSE
037300030107     C                   ADD       1             errore
037400030107     C                   EVAL      vinmsg = %trimr(vinmsg)
037500030107     C                             + ' ' + 'VABNCL'
037600030107     C                   ENDIF
037700030711     C* PKB
037800030107     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
037900030107     C                   EXSR      CHKNUM
038000030107     C                   IF        PiNum=*on
038100030107     C                   Z-ADD     PiVal         VABPKB
038200030107     C                   ELSE
038300030107     C                   ADD       1             errore
038400030107     C                   EVAL      vinmsg = %trimr(vinmsg)
038500030107     C                             + ' ' + 'VABPKB'
038600030107     C                   ENDIF
038700030711     C* CAS
038800030711     C                   IF        %subst(vindta:280:12) <> *blanks AND
038900030711     C                             %subst(vindta:280:12) <> *zeros
039000030711     C                   MOVEL     '1'           FlgCAS
039100030107     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
039200030107     C                   EXSR      CHKNUM
039300030107     C                   IF        PiNum=*on
039400030107     C                   Z-ADD     PiVal         VABCAS
039500030107     C                   ELSE
039600030107     C                   ADD       1             errore
039700030107     C                   EVAL      vinmsg = %trimr(vinmsg)
039800030107     C                             + ' ' + 'VABCAS'
039900030107     C                   ENDIF
040000030107     C                   ENDIF
040100010205     C*
040200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
040300030711     C                   IF        FlgCAS = '1'
040400010205     C                   EVAL      VABCBO = '4'
040500010205     C                   ELSE
040600010205     C                   EVAL      VABCBO = '1'
040700010205     C                   ENDIF
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
042400010201     C*----------------------------------------------------*
042500021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042600010201     C*----------------------------------------------------*
042700020305     C     WRIVAT        BEGSR
042800010201     C*
042900021017     C* Valorizzo l buffer di scrittura del FIVAT
043000030711     C                   EVAL      VATNOT = %triml(%subst(vindta:331:18))
043100021017     C                   write     FIVAT000
043200010201     C*
043300010201     C                   ENDSR
043400010202     C*----------------------------------------------------*
043500021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043600010202     C*----------------------------------------------------*
043700020305     C     PREVAT        BEGSR
043800010202     C*
043900021017     C* Compongo il nome del membro da dare al FIVATWWR
044000010202     C                   eval      parmbr = vlrhdl
044100010202     C                   movel     'M'           parmbr
044200030711     C                   eval      parccm = '00600356'
044300010202     C                   eval      paropz = '1'
044400010202     C* Effettuo la chiamata al CLLE preposto
044500030711     C                   call(e)   'TITVVTC'
044600010202     C                   parm                    parccm
044700010202     C                   parm                    parmbr
044800010202     C                   parm                    paropz
044900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045000010202     C                   if        %error
045100010202     C                   movel     '1'           chkcall
045200010202     C                   else
045300010202     C                   movel     '0'           chkcall
045400010202     C                   endif
045500010202     C*
045600010202     C                   ENDSR
045700000801     C*----------------------------------------------------*
045800000801     C*  CONTROLLO NUMERICITA' CAMPI
045900000801     C*----------------------------------------------------*
046000000801     C     CHKNUM        BEGSR
046100000801     C*
046200000801     C                   call(e)   'ISNUMERIC'
046300000801     C                   PARM                    PiStr            30
046400030711     C                   PARM      '.'           PiDecChr          1
046500000801     C                   PARM      *ZEROS        PiVal            30 9
046600000801     C                   PARM      '0'           PiInt             1
046700000801     C                   PARM      '0'           PiNum             1
046800000801     C                   IF        %error
046900000801     C                   EVAL      PiInt=*off
047000030107     C                   EVAL      PiNum=*off
047100000801     C                   ENDIF
047200000801     C*
047300000801     C                   ENDSR
047400000801     C***
047500000801
047600011113
047700011113     C*----------------------------------------------------*
047800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047900011113     C*----------------------------------------------------*
048000011113     C     CHKIMPDIV     BEGSR
048100011113     C*
048200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048300011113     C                   Z-ADD     *zeros        wrkDec            9 9
048400011113     C*
048500011113     C* Come prima cosa effettuo considerazioni sulla divisa
048600011113     C                   IF        vabIAS > *zeros
048700011113     C                   IF        vabVAS <> 'EUR'
048800011113     C                   EVAL      vabVAS =  'ITL'
048900011113     C                   ENDIF
049000011113     C                   ENDIF
049100011113     C*
049200011113     C                   IF        vabCAS > *zeros
049300011113     C                   IF        vabVCA <> 'EUR'
049400011113     C                   EVAL      vabVCA =  'ITL'
049500011113     C                   ENDIF
049600011113     C                   ENDIF
049700011113     C*
049800011113     C                   IF        vabVMD > *zeros
049900020305     C                   IF        vabVAD <> 'EUR'
050000011113     C                   EVAL      vabVAD =  'ITL'
050100011113     C                   ENDIF
050200011113     C                   ENDIF
050300011113     C*
050400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050500011113     C                   Z-ADD     vabIAS        wrkDec
050600011113     C                   IF        wrkDec > *zeros
050700011113     C                   IF        vabVAS = 'ITL'
050800011113     C                   EVAL      vabIAS = *zeros
050900011113     C                   ENDIF
051000011113     C                   ENDIF
051100011113     C*
051200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051300011113     C                   Z-ADD     vabCAS        wrkDec
051400011113     C                   IF        wrkDec > *zeros
051500011113     C                   IF        vabVCA = 'ITL'
051600011113     C                   EVAL      vabCAS = *zeros
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052100011113     C                   Z-ADD     vabVMD        wrkDec
052200011113     C                   IF        wrkDec > *zeros
052300011113     C                   IF        vabVAD = 'ITL'
052400011113     C                   EVAL      vabVMD = *zeros
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C                   ENDSR
052900011113     C***
053000021017
053100000801
053200000801
053300990920      /TITLE Invio dei dati al punto operativo.
053400010202     C     invio         BEGSR
053500990920     C*
053600021017     C* 1° invio FIVAT
053700010201     C                   reset                   dscmz
053800021017     C                   move      vlrpoi        cmzdst
053900021017     C                   eval      cmzfld = 'FIVATWWR'
054000010201     C                   eval      cmzmbd = vlrhdl
054100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054200021009     C***                if        prmfir = *blanks
054300021017     C                   eval      cmzfla = 'FIVAT00F'
054400021017     C                   eval      cmzmba = 'FIVAT00F'
054500021009     C***                else
054600021009     C***                eval      cmzfla = prmfir
054700021009     C***                eval      cmzmba = prmfir
054800021009     C***                endif
054900010201     C                   eval      cmznrr = *zeros
055000020305     C                   move      §ctrokvt      cmznrr
055100021018     C                   eval      cmzlba = vlrfl1
055200010201     C                   call(e)   'TIS711C'
055300010201     C                   parm                    dscmz
055400010201     C                   parm      *blanks       esito
055500010205     C                   if        %error
055600010205     C                             or cmzerr = '1'
055700010205     C                             or esito  = '1'
055800010205     C                   eval      wrkesito = '3'
055900010205     C                   else
056000010201     C*
056100021017     C* 2° invio FIVAB
056200010201     C                   reset                   dscmz
056300021017     C                   move      vlrpoi        cmzdst
056400010201     C                   eval      cmzfld = vlrfou
056500010201     C                   eval      cmzmbd = vlrhdl
056600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056700021009     C***                if        prmfir = *blanks
056800021017     C                   eval      cmzfla = 'FIVAB00F'
056900021017     C                   eval      cmzmba = 'FIVAB00F'
057000021009     C***                else
057100021009     C***                eval      cmzfla = prmfir
057200021009     C***                eval      cmzmba = prmfir
057300021009     C***                endif
057400010201     C                   eval      cmznrr = *zeros
057500010201     C                   move      §ctrokvb      cmznrr
057600021018     C                   eval      cmzlba = vlrfl1
057700010201     C                   call(e)   'TIS711C'
057800010201     C                   parm                    dscmz
057900010201     C                   parm      *blanks       esito
058000010201     C                   if        %error
058100010201     C                             or cmzerr = '1'
058200010201     C                             or esito  = '1'
058300010201     C                   eval      wrkesito = '3'
058400010201     C                   endif
058500010205     C                   endif
058600990920     C*
058700000613     C                   ENDSR
058800000613     C***
058900070411
059000070411     C     *pssr         BEGSR
059100070411     C*
059200070411     C                   if        %open(tivin00r)
059300070411     C                   close     tivin00r
059400070411     C                   endif
059500070411     C                   if        %open(fivabwwr)
059600070411     C                   close     fivabwwr
059700070411     C                   endif
059800070411     C                   if        %open(fivatwwr)
059900070411     C                   close     fivatwwr
060000070411     C                   endif
060100070411     C*
060200070411     C* Effettuo la chiamata al CLLE preposto
060300070411     C                   call(e)   'TITVVTC'
060400070411     C                   parm                    parccm
060500070411     C                   parm                    parmbr
060600070411     C                   parm      '2'           paropz
060700070411     C*
060800070411     C                   eval      wrkesito = '2'
060900070411     C*
061000070411     C                   seton                                        LR
061100070411     C*
061200070411     C                   ENDSR     '*CANCL'
061300070411     C***
061400070411
061500990910
061600000613     C     *inzsr        BEGSR
061700990910     C*
061800990910     C     *entry        plist
061900990920     C                   parm                    tivlrds
062000990921     C                   parm      wrkesito      esito
062100000724     C                   parm                    prmlit
062200000710     C                   parm                    prmfir
062300000613     C*
062400000830     C* CALCOLA LA DATA CORRENTE
062500000830     C                   time                    wn14             14 0
062600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062700000830     C                   z-add     wn8           g08dat
062800000830     C                   z-add     *zeros        g08inv
062900000830     C                   movel     '0'           g08err
063000000830     C                   call      'XSRDA8'
063100000830     C                   parm                    wlbda8
063200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063300000830     C*
063400000613     C                   ENDSR
063500000613     C***
063600990908
063700030711     Otitv61p   E            riepilogo         2
063800990915     O                                              'Upload via Internet'
063900990915     O                                           +1 'Traduzione TIVIN00R -'
064000021017     O                                           +1 'FIVABWWR/FIVATWWR'
064100010201     O                                           +1 'Report testate bolle'
064200990915     O          E            riepilogo   2
064300990915     O                       wrkdata
064400990915     O                       wrkora              +1
064500990915     O                       procname            +1
064600990915     O          E            riepilogo   2
064700990915     O                                              'Cliente..................:'
064800990915     O                       VABCCM        z     +1
064900990915     O          E            riepilogo   2
065000990920     O                                              'Riferimento Strategi.....:'
065100990920     O                       vlrhdl              +1
065200990915     O          E            riepilogo   2
065300990915     O                                              'Giusti...................:'
065400010201     O                       §CTROKVB      2   +  1
065500990915     O          E            riepilogo   2
065600990915     O                                              'Sbagliati e corretti.....:'
065700971022     O                       §CTRMO        2   +  1
065800990915     O          E            riepilogo   2
065900990915     O                                              'Sbagliati e scartati.....:'
066000971022     O                       §CTRNO        2   +  1
066100000613
066200030711     Otitv61ps  E            testdett          2
066300000613     O                                              'Upload via Internet'
066400000613     O                                           +1 'Traduzione TIVIN00R -'
066500021017     O                                           +1 'FIVABWWR/FIVATWWR'
066600010201     O                                           +1 'Report testate bolle'
066700000616     O          E            testdett    3
066800000613     O                                           +2 'N° rec'
066900000613     O                                           +3 'Anteprima contenuto'
067000000616     O          E            rigadett    2
067100000613     O                       rrnum               +2
067200000621     O                       recko               +3
067300000616     O          E            findett     2
067400000613     O                       wrkdata
067500000613     O                       wrkora              +1
067600000613     O                       procname            +1
067700000616     O          E            findett     2
067800000613     O                                              'Cliente..................:'
067900000613     O                       VABCCM        z     +1
068000000616     O          E            findett     2
068100000613     O                                              'Riferimento Strategi.....:'
068200000613     O                       vlrhdl              +1
068300000616     O          E            findett     2
068400000613     O                                              'Giusti...................:'
068500010201     O                       §CTROKVB      2   +  1
068600000616     O          E            findett     2
068700000613     O                                              'Sbagliati e corretti.....:'
068800000613     O                       §CTRMO        2   +  1
068900000616     O          E            findett     2
069000000613     O                                              'Sbagliati e scartati.....:'
069100000613     O                       §CTRNO        2   +  1
069200000512** CMD - COMANDI CL
069300030711OVRPRTF FILE(TITV61P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
069400030711OVRPRTF FILE(TITV61PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
069500030711DLTOVR FILE(TITV61P TITV61PS) LVL(*)
069600000512
069700000512
