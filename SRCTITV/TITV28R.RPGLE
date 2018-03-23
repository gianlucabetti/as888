000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030107     Ftitv28p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030107     Ftitv28ps  O    f  198        PRINTER usropn
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
009300030107     C                   if        not %open(titv28ps)
009400030107     C                   open      titv28ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700000613     C*
009800030107     C                   EXSR      RWFISCHER                                    LETT/SCR. VAB
009900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200050128     C                   call(e)   'TITVVTC'
010300010202     C                   parm                    parccm
010400010202     C                   parm                    parmbr
010500010202     C                   parm      '2'           paropz
010600010202     C*
010700030107     C                   if        %open(titv28ps)
010800000616     C                   except    findett
010900030107     C                   close     titv28ps
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
012700030107     C                   if        not %open(titv28p)
012800030107     C                   open      titv28p
012900990915     C                   endif
013000990915     C*
013100990915     C                   except    riepilogo
013200990915     C*
013300030107     C                   if        %open(titv28p)
013400030107     C                   close     titv28p
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
026400000801     C*
026500000801     C                   ENDSR
026600000801     C*----------------------------------------------------*
026700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026800000801     C*----------------------------------------------------*
026900000801     C     DEFCAM        BEGSR
027000000801     C*
027100021017     C                   CLEAR                   FIVAB000
027200020619     C* Imposto i valori di default...
027300030107     C                   Z-ADD     0061640       VABCCM
027400030107     C                   Z-ADD     0061640       VATCCM
027500021017     C                   Z-ADD     vlrpoi        VABFGS
027600021017     C                   Z-ADD     vlrpoi        VATFGS
027700030107     C                   Z-ADD     006           VABLNP
027800030107     C                   Z-ADD     006           VATLNP
027900030107     C                   Z-ADD     005           VABCTR
028000050125     C                   MOVEL     '7T'          VABCTM
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
030500021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030600000801     C*----------------------------------------------------*
030700010201     C     IMPVAB        BEGSR
030800000801     C*
030900020305     C                   EXSR      INZVAR
031000020305     C                   EXSR      DEFCAM
031100010305     C*
031200000801     C                   Z-ADD     *zeros        errore            1 0
031300000830     C                   MOVEL     datcor        VABAAS
031400020305     C                   MOVEL     datcor        VATAAS
031500000830     C                   MOVE      datcor        VABMGS
031600030107     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
031700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031900020117     C* ==
032000040206     C                   EVAL      VABRD2=%trim(%subst(vindta:197:30))
032100030107     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
032200030107     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
032300030107     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
032400030107     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
032500030107     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
032600030107     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
032700030107     C                   EVAL      VABTSP='E'
032800030107     C                   ENDIF
032900020305     C*
033000030107     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033100010201     C                   EXSR      CHKNUM
033200010201     C                   IF        PiInt=*on
033300010201     C                   Z-ADD     PiVal         Num5_0
033400021017     C                   MOVEL     Num5_0        VABCAD
033500010201     C                   ELSE
033600010201     C                   ADD       1             errore
033700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033800021017     C                             + ' ' + 'VABCAD'
033900010201     C                   ENDIF
034000020619     C*
034100030107     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
034200010201     C                   EXSR      CHKNUM
034300010201     C                   IF        PiInt=*on
034400021017     C                   Z-ADD     PiVal         VABRMN
034500030107     C                   Z-ADD     PiVal         Num7_0
034600030107     C                   MOVE(P)   Num7_0        VABNSP
034700030107     C                   MOVE(P)   Num7_0        VATNSP
034800010201     C                   ELSE
034900010201     C                   ADD       1             errore
035000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035100021017     C                             + ' ' + 'VABNSP VATNSP VABRMN'
035200010201     C                   ENDIF
035300030107     C*
035400030107     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
035500030107     C                   EXSR      CHKNUM
035600030107     C                   IF        PiInt=*on
035700030107     C                   Z-ADD     PiVal         VABNCL
035800030107     C                   ELSE
035900030107     C                   ADD       1             errore
036000030107     C                   EVAL      vinmsg = %trimr(vinmsg)
036100030107     C                             + ' ' + 'VABNCL'
036200030107     C                   ENDIF
036300030107     C*
036400030107     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
036500030107     C                   EXSR      CHKNUM
036600030107     C                   IF        PiNum=*on
036700030107     C                   Z-ADD     PiVal         VABPKB
036800030107     C                   ELSE
036900030107     C                   ADD       1             errore
037000030107     C                   EVAL      vinmsg = %trimr(vinmsg)
037100030107     C                             + ' ' + 'VABPKB'
037200030107     C                   ENDIF
037300030107     C*
037400030107     C                   IF        %trim(%subst(vindta:280:12)) <> *blanks
037500030107     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
037600030107     C                   EXSR      CHKNUM
037700030107     C                   IF        PiNum=*on
037800030107     C                   Z-ADD     PiVal         VABCAS
037900030107     C                   ELSE
038000030107     C                   ADD       1             errore
038100030107     C                   EVAL      vinmsg = %trimr(vinmsg)
038200030107     C                             + ' ' + 'VABCAS'
038300030107     C                   ENDIF
038400030107     C                   ENDIF
038500010205     C*
038600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038700010205     C                   IF        VABCAS > *zeros
038800010205     C                   EVAL      VABCBO = '4'
038900010205     C                   ELSE
039000010205     C                   EVAL      VABCBO = '1'
039100010205     C                   ENDIF
039200020305     C*
039300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039400011113     C                   EXSR      CHKIMPDIV
039500010202     C*
039600000801     C* Ebbene...
039700000801     C                   ADD       1             §CTRMO
039800010201     C                   IF        errore <> *zeros
039900000801     C                   ADD       1             §CTRNO
040000000801     C                   EVAL      recko = vindta
040100000801     C                   EXCEPT    rigadett
040200000801     C                   EVAL      vinflg = '2'
040300000801     C                   ELSE
040400010201     C                   ADD       1             §CTROKVB
040500000801     C                   ENDIF
040600000801     C*
040700000801     C                   ENDSR
040800010201     C*----------------------------------------------------*
040900021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041000010201     C*----------------------------------------------------*
041100020305     C     WRIVAT        BEGSR
041200010201     C*
041300021017     C* Valorizzo l buffer di scrittura del FIVAT
041400030107     C                   EVAL      VATNOT = %triml(%subst(vindta:331:26))
041500021017     C                   write     FIVAT000
041600010201     C*
041700010201     C                   ENDSR
041800010202     C*----------------------------------------------------*
041900021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042000010202     C*----------------------------------------------------*
042100020305     C     PREVAT        BEGSR
042200010202     C*
042300021017     C* Compongo il nome del membro da dare al FIVATWWR
042400010202     C                   eval      parmbr = vlrhdl
042500010202     C                   movel     'M'           parmbr
042600030107     C                   eval      parccm = '00061640'
042700010202     C                   eval      paropz = '1'
042800010202     C* Effettuo la chiamata al CLLE preposto
042900050128     C                   call(e)   'TITVVTC'
043000010202     C                   parm                    parccm
043100010202     C                   parm                    parmbr
043200010202     C                   parm                    paropz
043300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043400010202     C                   if        %error
043500010202     C                   movel     '1'           chkcall
043600010202     C                   else
043700010202     C                   movel     '0'           chkcall
043800010202     C                   endif
043900010202     C*
044000010202     C                   ENDSR
044100000801     C*----------------------------------------------------*
044200000801     C*  CONTROLLO NUMERICITA' CAMPI
044300000801     C*----------------------------------------------------*
044400000801     C     CHKNUM        BEGSR
044500000801     C*
044600000801     C                   call(e)   'ISNUMERIC'
044700000801     C                   PARM                    PiStr            30
044800030107     C                   PARM      ','           PiDecChr          1
044900000801     C                   PARM      *ZEROS        PiVal            30 9
045000000801     C                   PARM      '0'           PiInt             1
045100000801     C                   PARM      '0'           PiNum             1
045200000801     C                   IF        %error
045300000801     C                   EVAL      PiInt=*off
045400030107     C                   EVAL      PiNum=*off
045500000801     C                   ENDIF
045600000801     C*
045700000801     C                   ENDSR
045800000801     C***
045900000801
046000011113
046100011113     C*----------------------------------------------------*
046200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046300011113     C*----------------------------------------------------*
046400011113     C     CHKIMPDIV     BEGSR
046500011113     C*
046600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046700011113     C                   Z-ADD     *zeros        wrkDec            9 9
046800011113     C*
046900011113     C* Come prima cosa effettuo considerazioni sulla divisa
047000011113     C                   IF        vabIAS > *zeros
047100011113     C                   IF        vabVAS <> 'EUR'
047200011113     C                   EVAL      vabVAS =  'ITL'
047300011113     C                   ENDIF
047400011113     C                   ENDIF
047500011113     C*
047600011113     C                   IF        vabCAS > *zeros
047700011113     C                   IF        vabVCA <> 'EUR'
047800011113     C                   EVAL      vabVCA =  'ITL'
047900011113     C                   ENDIF
048000011113     C                   ENDIF
048100011113     C*
048200011113     C                   IF        vabVMD > *zeros
048300020305     C                   IF        vabVAD <> 'EUR'
048400011113     C                   EVAL      vabVAD =  'ITL'
048500011113     C                   ENDIF
048600011113     C                   ENDIF
048700011113     C*
048800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048900011113     C                   Z-ADD     vabIAS        wrkDec
049000011113     C                   IF        wrkDec > *zeros
049100011113     C                   IF        vabVAS = 'ITL'
049200011113     C                   EVAL      vabIAS = *zeros
049300011113     C                   ENDIF
049400011113     C                   ENDIF
049500011113     C*
049600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049700011113     C                   Z-ADD     vabCAS        wrkDec
049800011113     C                   IF        wrkDec > *zeros
049900011113     C                   IF        vabVCA = 'ITL'
050000011113     C                   EVAL      vabCAS = *zeros
050100011113     C                   ENDIF
050200011113     C                   ENDIF
050300011113     C*
050400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050500011113     C                   Z-ADD     vabVMD        wrkDec
050600011113     C                   IF        wrkDec > *zeros
050700011113     C                   IF        vabVAD = 'ITL'
050800011113     C                   EVAL      vabVMD = *zeros
050900011113     C                   ENDIF
051000011113     C                   ENDIF
051100011113     C*
051200011113     C                   ENDSR
051300011113     C***
051400021017
051500000801
051600000801
051700990920      /TITLE Invio dei dati al punto operativo.
051800010202     C     invio         BEGSR
051900990920     C*
052000021017     C* 1° invio FIVAT
052100010201     C                   reset                   dscmz
052200021017     C                   move      vlrpoi        cmzdst
052300021017     C                   eval      cmzfld = 'FIVATWWR'
052400010201     C                   eval      cmzmbd = vlrhdl
052500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052600021009     C***                if        prmfir = *blanks
052700021017     C                   eval      cmzfla = 'FIVAT00F'
052800021017     C                   eval      cmzmba = 'FIVAT00F'
052900021009     C***                else
053000021009     C***                eval      cmzfla = prmfir
053100021009     C***                eval      cmzmba = prmfir
053200021009     C***                endif
053300010201     C                   eval      cmznrr = *zeros
053400020305     C                   move      §ctrokvt      cmznrr
053500021018     C                   eval      cmzlba = vlrfl1
053600010201     C                   call(e)   'TIS711C'
053700010201     C                   parm                    dscmz
053800010201     C                   parm      *blanks       esito
053900010205     C                   if        %error
054000010205     C                             or cmzerr = '1'
054100010205     C                             or esito  = '1'
054200010205     C                   eval      wrkesito = '3'
054300010205     C                   else
054400010201     C*
054500021017     C* 2° invio FIVAB
054600010201     C                   reset                   dscmz
054700021017     C                   move      vlrpoi        cmzdst
054800010201     C                   eval      cmzfld = vlrfou
054900010201     C                   eval      cmzmbd = vlrhdl
055000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055100021009     C***                if        prmfir = *blanks
055200021017     C                   eval      cmzfla = 'FIVAB00F'
055300021017     C                   eval      cmzmba = 'FIVAB00F'
055400021009     C***                else
055500021009     C***                eval      cmzfla = prmfir
055600021009     C***                eval      cmzmba = prmfir
055700021009     C***                endif
055800010201     C                   eval      cmznrr = *zeros
055900010201     C                   move      §ctrokvb      cmznrr
056000021018     C                   eval      cmzlba = vlrfl1
056100010201     C                   call(e)   'TIS711C'
056200010201     C                   parm                    dscmz
056300010201     C                   parm      *blanks       esito
056400010201     C                   if        %error
056500010201     C                             or cmzerr = '1'
056600010201     C                             or esito  = '1'
056700010201     C                   eval      wrkesito = '3'
056800010201     C                   endif
056900010205     C                   endif
057000990920     C*
057100000613     C                   ENDSR
057200000613     C***
057300070411
057400070411     C     *pssr         BEGSR
057500070411     C*
057600070411     C                   if        %open(tivin00r)
057700070411     C                   close     tivin00r
057800070411     C                   endif
057900070411     C                   if        %open(fivabwwr)
058000070411     C                   close     fivabwwr
058100070411     C                   endif
058200070411     C                   if        %open(fivatwwr)
058300070411     C                   close     fivatwwr
058400070411     C                   endif
058500070411     C*
058600070411     C* Effettuo la chiamata al CLLE preposto
058700070411     C                   call(e)   'TITVVTC'
058800070411     C                   parm                    parccm
058900070411     C                   parm                    parmbr
059000070411     C                   parm      '2'           paropz
059100070411     C*
059200070411     C                   eval      wrkesito = '2'
059300070411     C*
059400070411     C                   seton                                        LR
059500070411     C*
059600070411     C                   ENDSR     '*CANCL'
059700070411     C***
059800070411
059900990910
060000000613     C     *inzsr        BEGSR
060100990910     C*
060200990910     C     *entry        plist
060300990920     C                   parm                    tivlrds
060400990921     C                   parm      wrkesito      esito
060500000724     C                   parm                    prmlit
060600000710     C                   parm                    prmfir
060700000613     C*
060800000830     C* CALCOLA LA DATA CORRENTE
060900000830     C                   time                    wn14             14 0
061000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061100000830     C                   z-add     wn8           g08dat
061200000830     C                   z-add     *zeros        g08inv
061300000830     C                   movel     '0'           g08err
061400000830     C                   call      'XSRDA8'
061500000830     C                   parm                    wlbda8
061600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061700000830     C*
061800000613     C                   ENDSR
061900000613     C***
062000990908
062100030107     Otitv28p   E            riepilogo         2
062200990915     O                                              'Upload via Internet'
062300990915     O                                           +1 'Traduzione TIVIN00R -'
062400021017     O                                           +1 'FIVABWWR/FIVATWWR'
062500010201     O                                           +1 'Report testate bolle'
062600990915     O          E            riepilogo   2
062700990915     O                       wrkdata
062800990915     O                       wrkora              +1
062900990915     O                       procname            +1
063000990915     O          E            riepilogo   2
063100990915     O                                              'Cliente..................:'
063200990915     O                       VABCCM        z     +1
063300990915     O          E            riepilogo   2
063400990920     O                                              'Riferimento Strategi.....:'
063500990920     O                       vlrhdl              +1
063600990915     O          E            riepilogo   2
063700990915     O                                              'Giusti...................:'
063800010201     O                       §CTROKVB      2   +  1
063900990915     O          E            riepilogo   2
064000990915     O                                              'Sbagliati e corretti.....:'
064100971022     O                       §CTRMO        2   +  1
064200990915     O          E            riepilogo   2
064300990915     O                                              'Sbagliati e scartati.....:'
064400971022     O                       §CTRNO        2   +  1
064500000613
064600030107     Otitv28ps  E            testdett          2
064700000613     O                                              'Upload via Internet'
064800000613     O                                           +1 'Traduzione TIVIN00R -'
064900021017     O                                           +1 'FIVABWWR/FIVATWWR'
065000010201     O                                           +1 'Report testate bolle'
065100000616     O          E            testdett    3
065200000613     O                                           +2 'N° rec'
065300000613     O                                           +3 'Anteprima contenuto'
065400000616     O          E            rigadett    2
065500000613     O                       rrnum               +2
065600000621     O                       recko               +3
065700000616     O          E            findett     2
065800000613     O                       wrkdata
065900000613     O                       wrkora              +1
066000000613     O                       procname            +1
066100000616     O          E            findett     2
066200000613     O                                              'Cliente..................:'
066300000613     O                       VABCCM        z     +1
066400000616     O          E            findett     2
066500000613     O                                              'Riferimento Strategi.....:'
066600000613     O                       vlrhdl              +1
066700000616     O          E            findett     2
066800000613     O                                              'Giusti...................:'
066900010201     O                       §CTROKVB      2   +  1
067000000616     O          E            findett     2
067100000613     O                                              'Sbagliati e corretti.....:'
067200000613     O                       §CTRMO        2   +  1
067300000616     O          E            findett     2
067400000613     O                                              'Sbagliati e scartati.....:'
067500000613     O                       §CTRNO        2   +  1
067600000512** CMD - COMANDI CL
067700030107OVRPRTF FILE(TITV28P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
067800030107OVRPRTF FILE(TITV28PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
067900030107DLTOVR FILE(TITV28P TITV28PS) LVL(*)
068000000512
068100000512
